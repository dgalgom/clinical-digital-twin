#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Statistical-adequacy checkpoint for the fall-risk digital twin.
#
# Run from the project root (after building the dataset):
#   Rscript checkpoints/evaluate_model.R
#
# Purpose: give an external reviewer a quick, honest read on whether the SINGLE
# pooled model is statistically adequate and whether its outputs make clinical
# sense. It reports, per horizon (24h / 7d):
#   * Discrimination  : AUC (ROC) on a PATIENT-level held-out split (no leakage).
#   * Calibration     : Brier score + a coarse reliability table.
#   * Sensibility     : sign/direction of standardized coefficients vs. clinical
#                       expectation, and directionality of key counterfactuals.
#   * Latency         : mean single-prediction wall-time (the twin must be fast
#                       enough for interactive what-ifs).
#
# It PRINTS a PASS/FAIL summary and exits non-zero on failure, so it can gate a
# CI pipeline. Thresholds are deliberately lenient (synthetic data, tiny cohort)
# and documented inline.
# ---------------------------------------------------------------------------

suppressWarnings(suppressMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(tibble)
}))

# --- Locate + load the package code ---------------------------------------
env_root <- Sys.getenv("CDT_PROJECT_ROOT", unset = NA)
if (!is.na(env_root) && nzchar(env_root) && dir.exists(file.path(env_root, "R"))) {
  root <- normalizePath(env_root)
} else {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
  root <- if (length(file_arg) == 1) {
    normalizePath(file.path(dirname(file_arg), ".."))
  } else {
    normalizePath(getwd())
  }
}
Sys.setenv(CDT_PROJECT_ROOT = root)
for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

# --- Metric helpers (dependency-free) -------------------------------------

# AUC via the Mann-Whitney U relationship (rank-based; ties handled).
.auc <- function(scores, labels) {
  pos <- scores[labels == 1]
  neg <- scores[labels == 0]
  if (length(pos) == 0 || length(neg) == 0) return(NA_real_)
  r <- rank(c(pos, neg))
  (sum(r[seq_along(pos)]) - length(pos) * (length(pos) + 1) / 2) /
    (length(pos) * length(neg))
}

.brier <- function(p, y) mean((p - y)^2)

# Coarse reliability table: bin predicted probability, compare to observed rate.
.reliability <- function(p, y, bins = 5) {
  br <- cut(p, breaks = seq(0, 1, length.out = bins + 1),
    include.lowest = TRUE)
  tibble::tibble(bin = br, p = p, y = y) |>
    dplyr::group_by(bin) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_pred = mean(p),
      obs_rate = mean(y),
      .groups = "drop"
    )
}

# --- Load data ------------------------------------------------------------
con <- cdt_db_connect()
on.exit(DBI::dbDisconnect(con), add = TRUE)

cohort <- cdt_get_cohort(con)
readings <- tibble::as_tibble(DBI::dbGetQuery(con, "SELECT * FROM sensor_readings"))
falls <- tibble::as_tibble(DBI::dbGetQuery(con, "SELECT * FROM fall_events"))

stopifnot(nrow(cohort) > 0, nrow(readings) > 0)

# --- Patient-level split (no snapshot leakage across train/test) ----------
set.seed(2026)
pids <- cohort$patient_id
test_pids <- sample(pids, size = max(1, round(0.30 * length(pids))))
train_pids <- setdiff(pids, test_pids)

train_cohort <- cohort[cohort$patient_id %in% train_pids, ]
test_cohort <- cohort[cohort$patient_id %in% test_pids, ]

tt_train <- cdt_build_training_table(train_cohort, readings, falls)
tt_test <- cdt_build_training_table(test_cohort, readings, falls)

cat(sprintf(
  "Split: %d train patients (%d snapshots), %d test patients (%d snapshots).\n",
  length(train_pids), nrow(tt_train), length(test_pids), nrow(tt_test)
))

# Fit on TRAIN only for an honest held-out read.
eval_model <- cdt_fit_model(tt_train)

# --- Score held-out snapshots per horizon ---------------------------------
# One pooled model: score each test snapshot at horizon_7d = 0 (24h) and 1 (7d).
score_horizon <- function(tt, horizon_7d) {
  feats <- tt[, cdt_model_features(), drop = FALSE]
  vapply(seq_len(nrow(feats)), function(i) {
    xs <- .cdt_standardize(eval_model, feats[i, , drop = FALSE], horizon_7d)
    .cdt_score_part(eval_model$model, xs)
  }, numeric(1))
}

p24 <- score_horizon(tt_test, 0)
p7 <- score_horizon(tt_test, 1)
y24 <- tt_test$label_24h
y7 <- tt_test$label_7d

auc24 <- .auc(p24, y24)
auc7 <- .auc(p7, y7)
brier24 <- .brier(p24, y24)
brier7 <- .brier(p7, y7)

cat("\n== Discrimination & calibration (held-out) ==\n")
cat(sprintf("  24h : AUC=%.3f  Brier=%.4f  (events=%d/%d)\n",
  auc24, brier24, sum(y24), length(y24)))
cat(sprintf("  7d  : AUC=%.3f  Brier=%.4f  (events=%d/%d)\n",
  auc7, brier7, sum(y7), length(y7)))

cat("\n  Reliability (7d, 5 bins):\n")
print(.reliability(p7, y7))

# --- Coefficient sensibility ----------------------------------------------
# Expected sign of the standardized coefficient (log-odds of a fall) for the
# ACTIVITY / VITALS drivers. Static factors are intentionally NOT asserted:
# in this synthetic generator, frailty acts ONLY through the sensor streams, so
# once the sensors are in the model the static factors carry confounded residual
# signal and their signs are not identifiable (documented in the README).
expected_sign <- c(
  steps_mean_7d = -1,            # more activity -> lower risk
  steps_trend_7d = -1,           # declining steps -> higher risk
  sedentary_hours_mean_7d = +1,  # more sedentary -> higher risk
  sedentary_hours_trend_7d = +1,
  resting_hr_trend_7d = +1,      # rising resting HR -> higher risk
  accel_magnitude_mean_7d = -1   # more movement intensity -> lower risk
)
co <- eval_model$model$coef
sign_ok <- vapply(names(expected_sign), function(f) {
  isTRUE(sign(co[[f]]) == expected_sign[[f]]) || abs(co[[f]]) < 1e-6
}, logical(1))

cat("\n== Coefficient directionality (activity/vitals drivers) ==\n")
for (f in names(expected_sign)) {
  cat(sprintf("  %-26s coef=%+.3f  expected=%+d  %s\n",
    f, co[[f]], expected_sign[[f]],
    if (sign_ok[[f]]) "ok" else "UNEXPECTED"))
}

# --- Counterfactual directionality (twin sanity) --------------------------
# Score the highest-risk held-out patient and confirm the twin moves the right
# way for two clinically clear interventions.
feats_test <- lapply(test_pids, function(p) {
  cdt_assemble_features(cohort[cohort$patient_id == p, ],
    readings[readings$patient_id == p, ])
})
base7 <- vapply(feats_test, function(f) {
  predict_fall_risk(eval_model, f)$p_7d
}, numeric(1))
fr <- feats_test[[which.max(base7)]]

cf_better <- predict_fall_risk(eval_model, fr,
  modified_inputs = list(steps_pct = 40, sedentary_hours_mean_7d = 4),
  include_baseline = TRUE)
cf_worse <- predict_fall_risk(eval_model, fr,
  modified_inputs = list(steps_trend_7d = -200, resting_hr_trend_7d = 1.5),
  include_baseline = TRUE)

cf_better_ok <- cf_better$delta$p_7d < 0
cf_worse_ok <- cf_worse$delta$p_7d > 0

cat("\n== Counterfactual directionality (highest-risk held-out patient) ==\n")
cat(sprintf("  more steps + less sedentary : 7d delta=%+.3f  %s\n",
  cf_better$delta$p_7d, if (cf_better_ok) "ok" else "UNEXPECTED"))
cat(sprintf("  declining steps + rising HR : 7d delta=%+.3f  %s\n",
  cf_worse$delta$p_7d, if (cf_worse_ok) "ok" else "UNEXPECTED"))

# --- Latency --------------------------------------------------------------
# The twin must be fast enough for interactive what-ifs. Measure single-call
# prediction latency (full path: assemble already done, standardize + score x2).
n_lat <- 500
t0 <- Sys.time()
for (i in seq_len(n_lat)) predict_fall_risk(eval_model, fr)
elapsed_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
per_call_ms <- elapsed_ms / n_lat

cat("\n== Latency ==\n")
cat(sprintf("  mean single prediction: %.3f ms  (%d calls)\n",
  per_call_ms, n_lat))

# --- PASS / FAIL gate -----------------------------------------------------
# Lenient thresholds: synthetic data, tiny cohort, very low 24h prevalence.
checks <- c(
  "AUC 7d >= 0.65" = isTRUE(auc7 >= 0.65),
  "Brier 7d <= 0.10" = isTRUE(brier7 <= 0.10),
  "activity/vitals coef signs sensible" = all(sign_ok),
  "counterfactual (protective) lowers 7d risk" = cf_better_ok,
  "counterfactual (deteriorating) raises 7d risk" = cf_worse_ok,
  "latency < 5 ms/call" = per_call_ms < 5
)

cat("\n== Checkpoint summary ==\n")
for (nm in names(checks)) {
  cat(sprintf("  [%s] %s\n", if (checks[[nm]]) "PASS" else "FAIL", nm))
}

if (all(checks)) {
  cat("\nAll statistical-adequacy checks PASSED.\n")
} else {
  cat("\nSome checks FAILED. See above.\n")
  quit(status = 1, save = "no")
}
