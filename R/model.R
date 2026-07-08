#' Fall-risk digital twin model
#'
#' A SINGLE interpretable, ridge-penalized logistic-regression model predicts
#' P(fall) at multiple horizons via a pooled discrete-time (pooled-logistic)
#' design: the prediction horizon enters as a feature (`horizon_7d`), so one
#' fitted object produces both the 24-hour and 7-day risk. This deliberately
#' trades a little accuracy for low complexity, transparent coefficients, and
#' negligible latency (prediction is a single dot-product), which suits a
#' clinical digital twin and its counterfactual "what-if" simulation.
#'
#' The fitted object is a plain list persisted with `saveRDS()`.

#' Predictor columns fed to the model (excluding the horizon indicator)
#'
#' @return Character vector of predictor names.
#' @export
cdt_model_features <- function() {
  c(cdt_static_features(), cdt_sensor_features())
}

#' Full design columns including the pooled-horizon indicator
#'
#' @return Character vector: model features plus `horizon_7d`.
#' @export
cdt_design_features <- function() {
  c(cdt_model_features(), "horizon_7d")
}

#' Ridge-penalized logistic regression via IRLS
#'
#' Minimal, dependency-free implementation. The intercept is not penalized. This
#' guarantees finite coefficients under separation, unlike an unpenalized GLM.
#'
#' @param X Numeric predictor matrix (already standardized), no intercept column.
#' @param y Binary outcome vector.
#' @param lambda L2 penalty strength.
#' @param max_iter Maximum IRLS iterations.
#' @param tol Convergence tolerance on the coefficient change.
#' @return A list with `coef` (named, including `(Intercept)`).
#' @keywords internal
.cdt_fit_ridge_logistic <- function(X, y, lambda = 1.0,
                                    max_iter = 100, tol = 1e-8) {
  Xd <- cbind(`(Intercept)` = 1, X)
  p <- ncol(Xd)
  beta <- rep(0, p)
  # Penalty matrix: penalize all but the intercept.
  P <- diag(lambda, p)
  P[1, 1] <- 0

  for (i in seq_len(max_iter)) {
    eta <- as.numeric(Xd %*% beta)
    mu <- stats::plogis(eta)
    w <- pmax(mu * (1 - mu), 1e-6)
    z <- eta + (y - mu) / w
    XtW <- t(Xd * w)
    beta_new <- tryCatch(
      solve(XtW %*% Xd + P, XtW %*% z),
      error = function(e) {
        solve(XtW %*% Xd + P + diag(1e-6, p), XtW %*% z)
      }
    )
    beta_new <- as.numeric(beta_new)
    if (max(abs(beta_new - beta)) < tol) {
      beta <- beta_new
      break
    }
    beta <- beta_new
  }
  names(beta) <- colnames(Xd)
  list(coef = beta)
}

#' Pool a wide training table into long discrete-time form
#'
#' Stacks each snapshot into two rows (24h and 7d), adding a `horizon_7d`
#' indicator and a single `label` column. This is the design matrix for the
#' pooled-logistic model.
#'
#' @param training_table Output of [cdt_build_training_table()].
#' @return A long data frame with predictors, `horizon_7d`, and `label`.
#' @export
cdt_pool_training_table <- function(training_table) {
  feats <- cdt_model_features()
  stopifnot(all(feats %in% names(training_table)))

  base <- training_table[, feats, drop = FALSE]

  row24 <- base
  row24$horizon_7d <- 0
  row24$label <- training_table$label_24h

  row7 <- base
  row7$horizon_7d <- 1
  row7$label <- training_table$label_7d

  rbind(row24, row7)
}

#' Fit the single pooled digital twin model
#'
#' @param training_table Output of [cdt_build_training_table()].
#' @param lambda Ridge penalty strength (mild by default).
#' @return A `cdt_model` object (list) with ONE fitted pooled model, feature
#'   means/sds for standardization, and metadata.
#' @export
cdt_fit_model <- function(training_table, lambda = 1.0) {
  design <- cdt_design_features()
  pooled <- cdt_pool_training_table(training_table)

  X <- pooled[, design, drop = FALSE]

  # Standardize predictors for stable coefficients / comparable importances.
  # The horizon indicator is standardized too so its coefficient is comparable.
  centers <- vapply(X, function(c) mean(c, na.rm = TRUE), numeric(1))
  scales <- vapply(X, function(c) {
    s <- stats::sd(c, na.rm = TRUE)
    if (is.na(s) || s == 0) 1 else s
  }, numeric(1))

  Xs <- as.matrix(scale(X, center = centers, scale = scales))
  Xs[is.na(Xs)] <- 0

  # Ridge-penalized logistic regression (L2) keeps coefficients finite even
  # under perfect separation or low event counts, which is essential for stable,
  # interpretable counterfactuals on small cohorts.
  fit <- .cdt_fit_ridge_logistic(Xs, pooled$label, lambda = lambda)

  structure(
    list(
      model = fit,
      features = cdt_model_features(),
      design = design,
      centers = centers,
      scales = scales,
      lambda = lambda,
      trained_at = as.character(Sys.time()),
      n_train = nrow(training_table),
      n_pooled = nrow(pooled),
      prevalence = list(
        p24 = mean(training_table$label_24h),
        p7 = mean(training_table$label_7d)
      )
    ),
    class = "cdt_model"
  )
}

#' Standardize a full design row (features + horizon indicator)
#'
#' Builds the design vector for one horizon by appending the `horizon_7d`
#' indicator to the raw feature row, then standardizes against the stored
#' design-indexed centers/scales.
#'
#' @param model A `cdt_model`.
#' @param feature_row A one-row tibble/data frame of raw features.
#' @param horizon_7d The pooled-horizon indicator (0 for 24h, 1 for 7d).
#' @return A named numeric vector of standardized design columns.
#' @keywords internal
.cdt_standardize <- function(model, feature_row, horizon_7d) {
  x <- as.numeric(feature_row[, model$features, drop = FALSE])
  raw <- c(x, horizon_7d)
  names(raw) <- model$design
  xs <- (raw[model$design] - model$centers[model$design]) /
    model$scales[model$design]
  xs[is.na(xs)] <- 0
  names(xs) <- model$design
  xs
}

#' Score a standardized feature vector against a ridge-logistic model part
#'
#' @param model_part A list with `coef` (named, including `(Intercept)`).
#' @param xs Named numeric vector of standardized features.
#' @return Probability in `[0, 1]`.
#' @keywords internal
.cdt_score_part <- function(model_part, xs) {
  co <- model_part$coef
  eta <- co[["(Intercept)"]] + sum(co[names(xs)] * xs)
  stats::plogis(eta)
}

#' Apply counterfactual overrides to a raw feature row
#'
#' Supported override keys map onto engineered features and static factors. Both
#' absolute values and relative deltas are supported:
#' * `steps_pct` : multiply `steps_mean_7d` by (1 + steps_pct/100)
#' * `steps_mean_7d`, `resting_hr_mean_7d`, `sbp_mean_7d`,
#'   `sedentary_hours_mean_7d`, `hr_variability_7d` : absolute override
#' * `sbp_delta` : add to `sbp_mean_7d` (e.g. new BP med lowers SBP by 10)
#' * `polypharmacy`, `n_medications`, `prior_falls` : absolute override
#'
#' @param feature_row One-row raw feature tibble.
#' @param overrides Named list of counterfactual modifications, or `NULL`.
#' @return The modified one-row feature tibble.
#' @export
cdt_apply_overrides <- function(feature_row, overrides = NULL) {
  if (is.null(overrides) || length(overrides) == 0) {
    return(feature_row)
  }
  fr <- feature_row

  if (!is.null(overrides$steps_pct)) {
    fr$steps_mean_7d <- fr$steps_mean_7d * (1 + overrides$steps_pct / 100)
  }
  if (!is.null(overrides$sbp_delta)) {
    fr$sbp_mean_7d <- fr$sbp_mean_7d + overrides$sbp_delta
  }

  absolute <- c(
    "steps_mean_7d", "resting_hr_mean_7d", "sbp_mean_7d",
    "sedentary_hours_mean_7d", "hr_variability_7d",
    "steps_trend_7d", "resting_hr_trend_7d", "sedentary_hours_trend_7d",
    "polypharmacy", "n_medications", "prior_falls", "orthostatic_hypotension"
  )
  for (k in absolute) {
    if (!is.null(overrides[[k]])) {
      fr[[k]] <- overrides[[k]]
    }
  }
  fr
}

#' Predict fall risk for a patient, with optional counterfactual overrides
#'
#' This is the digital-twin entry point. With `modified_inputs = NULL` it returns
#' the baseline risk; with overrides it returns the simulated ("twin") risk. Set
#' `include_baseline = TRUE` to get both in one call for side-by-side plots.
#'
#' @param model A `cdt_model` (or path to an .rds; auto-loaded).
#' @param feature_row One-row raw feature tibble (from [cdt_assemble_features()]).
#' @param modified_inputs Named list of counterfactual overrides, or `NULL`.
#' @param include_baseline If `TRUE`, also return baseline risk and deltas.
#' @return A list with `p_24h`, `p_7d`, `tier_24h`, `tier_7d`, and (optionally)
#'   `baseline` and `delta`.
#' @export
predict_fall_risk <- function(model, feature_row,
                              modified_inputs = NULL,
                              include_baseline = FALSE) {
  if (is.character(model)) {
    model <- readRDS(model)
  }
  stopifnot(inherits(model, "cdt_model"))

  # One pooled model scores both horizons: horizon_7d = 0 gives the 24h risk,
  # horizon_7d = 1 gives the 7-day risk. Prediction is two dot-products.
  score <- function(fr) {
    xs24 <- .cdt_standardize(model, fr, horizon_7d = 0)
    xs7 <- .cdt_standardize(model, fr, horizon_7d = 1)
    p24 <- .cdt_score_part(model$model, xs24)
    p7 <- .cdt_score_part(model$model, xs7)
    list(
      p_24h = p24, p_7d = p7,
      tier_24h = as.character(cdt_risk_tier(p24)),
      tier_7d = as.character(cdt_risk_tier(p7))
    )
  }

  modified_row <- cdt_apply_overrides(feature_row, modified_inputs)
  sim <- score(modified_row)

  if (include_baseline) {
    base <- score(feature_row)
    sim$baseline <- base
    sim$delta <- list(
      p_24h = sim$p_24h - base$p_24h,
      p_7d = sim$p_7d - base$p_7d
    )
  }
  sim
}

#' Standardized-coefficient importances (interpretability aid)
#'
#' Because predictors are standardized, absolute coefficient magnitudes are a
#' transparent, SHAP-adjacent proxy for feature influence on the log-odds. There
#' is a SINGLE pooled model, so importances are horizon-agnostic. The `horizon`
#' argument is retained for backward compatibility with callers but does not
#' change the returned coefficients; the pooled `horizon_7d` term is excluded so
#' the ranking reflects patient-level drivers only.
#'
#' @param model A `cdt_model`.
#' @param horizon Retained for compatibility; ignored (one pooled model).
#' @return A tibble of `feature`, `coefficient`, `abs_coefficient` sorted by
#'   descending influence.
#' @export
cdt_feature_importance <- function(model, horizon = c("7d", "24h")) {
  match.arg(horizon)
  co <- model$model$coef
  co <- co[!names(co) %in% c("(Intercept)", "horizon_7d")]
  tibble::tibble(
    feature = names(co),
    coefficient = as.numeric(co),
    abs_coefficient = abs(as.numeric(co))
  )[order(-abs(as.numeric(co))), ]
}

#' Persist / load the trained model
#'
#' @param model A `cdt_model`.
#' @param path Destination path (default [cdt_model_path()]).
#' @return Invisibly the path written.
#' @export
cdt_save_model <- function(model, path = cdt_model_path()) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(model, path)
  invisible(path)
}

#' @rdname cdt_save_model
#' @export
cdt_load_model <- function(path = cdt_model_path()) {
  if (!file.exists(path)) {
    stop("Model file not found: ", path,
      ". Run data-raw/generate_synthetic_data.R first.",
      call. = FALSE
    )
  }
  readRDS(path)
}

#' Print method for cdt_model
#' @param x A `cdt_model`.
#' @param ... Unused.
#' @export
print.cdt_model <- function(x, ...) {
  cat("<cdt_model>  (single pooled discrete-time logistic)\n")
  cat(sprintf("  trained_at : %s\n", x$trained_at))
  cat(sprintf("  n_train    : %d snapshots\n", x$n_train))
  cat(sprintf("  n_pooled   : %d rows (24h + 7d stacked)\n", x$n_pooled))
  cat(sprintf("  lambda     : %g (ridge L2)\n", x$lambda))
  cat(sprintf(
    "  prevalence : 24h=%.3f  7d=%.3f\n",
    x$prevalence$p24, x$prevalence$p7
  ))
  cat(sprintf("  features   : %d (+ horizon_7d)\n", length(x$features)))
  invisible(x)
}
