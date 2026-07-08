#' Chat-query classification + server-side chart rendering for the bot
#'
#' Two jobs live here:
#'   1. `cdt_bot_classify_query()` — a DETERMINISTIC, offline fallback classifier
#'      that maps a free-text clinician query onto one of the supported chart
#'      intents plus the parameters needed to render it (patient, time window,
#'      metric, what-if overrides). It mirrors the taxonomy the Claude-assisted
#'      `viz-query-router` subagent uses, so the bot still works without network.
#'   2. PNG renderers (`cdt_bot_plot_*`) that draw a clean, professional chart to
#'      a temp PNG using base `grDevices::png()` (cairo). No new dependency.
#'
#' Convention for every time series: DATES on the x-axis, the variable of
#' interest on the y-axis. What-if is the one categorical chart (baseline vs
#' simulated risk bars). Renderers return a PNG file path, or `NULL` when there
#' is nothing plottable (e.g. an empty window), so callers can fall back to text.
#'
#' Nothing here touches the model math, features, or schema — it only reads
#' stored (synthetic) rows and the model's prediction outputs for display.

# ---------------------------------------------------------------------------
# Intent taxonomy
# ---------------------------------------------------------------------------

#' The supported visualization intents (the router/grader taxonomy)
#'
#' Each intent maps to a concrete renderable chart backed by real stored columns
#' or model outputs (see docs/data_dictionary.md):
#'   * `fall_history`         — steps + resting HR timeline with fall markers
#'   * `functional_history`   — same multi-panel functional overview
#'   * `steps_over_time`      — daily step_count series
#'   * `resting_hr_over_time` — daily resting_hr series
#'   * `sbp_over_time`        — daily systolic BP series
#'   * `sedentary_over_time`  — daily sedentary hours (sitting + lying)
#'   * `whatif`               — baseline vs simulated 24h/7d risk bars
#'
#' @return Character vector of intent names.
#' @export
cdt_bot_intents <- function() {
  c(
    "fall_history", "functional_history", "steps_over_time",
    "resting_hr_over_time", "sbp_over_time", "sedentary_over_time",
    "whatif"
  )
}

# Map a single-metric intent onto the human label + how to derive it from a
# readings row. Returns NULL for non-series intents.
.cdt_metric_spec <- function(intent) {
  switch(intent,
    steps_over_time = list(
      key = "steps", label = "Daily step count", unit = "steps",
      derive = function(r) as.numeric(r$step_count)
    ),
    resting_hr_over_time = list(
      key = "resting_hr", label = "Resting heart rate", unit = "bpm",
      derive = function(r) as.numeric(r$resting_hr)
    ),
    sbp_over_time = list(
      key = "sbp", label = "Systolic blood pressure", unit = "mmHg",
      derive = function(r) as.numeric(r$sbp)
    ),
    sedentary_over_time = list(
      key = "sedentary", label = "Sedentary time", unit = "hours/day",
      derive = function(r) as.numeric(r$hours_sitting) + as.numeric(r$hours_lying)
    ),
    NULL
  )
}

#' Classify a free-text query into an intent + render parameters (offline)
#'
#' Deterministic keyword/regex router: resolves the patient id, an optional
#' relative time window (via [cdt_parse_relative_window()]), and — for what-if
#' queries — the counterfactual overrides (via [cdt_bot_parse_whatif()]). This is
#' the fallback used when the Claude-assisted router is unavailable.
#'
#' @param text Free-text clinician query.
#' @param chat_id Optional chat id, so focus state can supply the patient when
#'   the message itself omits it.
#' @return A list `list(intent, patient_id, window, metric, whatif_overrides,
#'   rationale)`. `intent` is `NA_character_` when nothing is recognized.
#' @export
cdt_bot_classify_query <- function(text, chat_id = NULL) {
  t <- tolower(text %||% "")

  pid <- cdt_bot_extract_patient(text)
  if (is.null(pid) && !is.null(chat_id)) {
    pid <- cdt_bot_focus(chat_id)
  }

  window <- cdt_parse_relative_window(text)
  overrides <- cdt_bot_parse_whatif(text)

  # What-if wins when explicit counterfactual language is present.
  whatif_words <- grepl("what if|what-if|whatif|simulate|counterfactual|if we|if the patient|remove|stop|deprescribe|increase|decrease|reduce|lower|boost", t)
  if (!is.null(overrides) || (whatif_words && grepl("risk|fall|medic|drug|step|mobil|bp|blood pressure|sedentary", t))) {
    return(list(
      intent = "whatif", patient_id = pid, window = window,
      metric = NULL, whatif_overrides = overrides,
      rationale = "Counterfactual language detected -> baseline vs simulated risk bars."
    ))
  }

  # Fall history: explicit fall/fell language.
  if (grepl("fall|fell|falls history|fall history|fall event", t)) {
    return(list(
      intent = "fall_history", patient_id = pid, window = window,
      metric = NULL, whatif_overrides = NULL,
      rationale = "Fall-related language -> steps + resting HR timeline with fall markers."
    ))
  }

  # Single-metric series (order matters: check specific metrics first).
  if (grepl("\\bstep|walk|activity|mobil", t)) {
    return(list(
      intent = "steps_over_time", patient_id = pid, window = window,
      metric = "steps", whatif_overrides = NULL,
      rationale = "Activity/steps language -> daily step count over time."
    ))
  }
  if (grepl("resting hr|resting heart|heart rate|\\bhr\\b|pulse", t)) {
    return(list(
      intent = "resting_hr_over_time", patient_id = pid, window = window,
      metric = "resting_hr", whatif_overrides = NULL,
      rationale = "Heart-rate language -> resting HR over time."
    ))
  }
  if (grepl("sbp|systolic|blood pressure|\\bbp\\b", t)) {
    return(list(
      intent = "sbp_over_time", patient_id = pid, window = window,
      metric = "sbp", whatif_overrides = NULL,
      rationale = "Blood-pressure language -> systolic BP over time."
    ))
  }
  if (grepl("sedentary|sitting|lying|inactiv|immobil", t)) {
    return(list(
      intent = "sedentary_over_time", patient_id = pid, window = window,
      metric = "sedentary", whatif_overrides = NULL,
      rationale = "Sedentary language -> daily sedentary hours over time."
    ))
  }

  # Functional / trending overview.
  if (grepl("function|trend|overview|history|doing|status|how is", t)) {
    return(list(
      intent = "functional_history", patient_id = pid, window = window,
      metric = NULL, whatif_overrides = NULL,
      rationale = "General functional/trend query -> multi-panel functional overview."
    ))
  }

  list(
    intent = NA_character_, patient_id = pid, window = window,
    metric = NULL, whatif_overrides = NULL,
    rationale = "No visualization intent recognized."
  )
}

# ---------------------------------------------------------------------------
# Shared plotting theme
# ---------------------------------------------------------------------------

# Muted, professional palette (colourblind-safe-ish blues/oranges/greys).
.cdt_theme <- function() {
  list(
    fg = "#1b2733", grid = "#e4e8ee", panel = "#ffffff",
    accent = "#2b6cb0", accent2 = "#dd6b20", accent3 = "#38a169",
    marker = "#c53030", muted = "#718096",
    width = 1000, height = 560, res = 120,
    family = "sans"
  )
}

# Open a cairo PNG device with the shared theme; returns the temp path.
.cdt_png_open <- function(th = .cdt_theme()) {
  path <- tempfile(fileext = ".png")
  grDevices::png(
    filename = path, width = th$width, height = th$height,
    res = th$res, type = "cairo", bg = th$panel
  )
  path
}

# Light horizontal gridlines + a framed plot region, drawn after plot.new().
.cdt_grid <- function(th, xlim, ylim) {
  graphics::rect(xlim[1], ylim[1], xlim[2], ylim[2],
    col = th$panel, border = NA
  )
  yticks <- pretty(ylim)
  graphics::abline(h = yticks, col = th$grid, lwd = 1)
}

# Format a Date axis with sensible tick spacing for the span.
.cdt_date_axis <- function(dates, th) {
  rng <- range(dates, na.rm = TRUE)
  span <- as.numeric(diff(rng))
  by <- if (span <= 21) {
    "1 week"
  } else if (span <= 120) {
    "2 weeks"
  } else if (span <= 400) {
    "1 month"
  } else {
    "3 months"
  }
  at <- seq(rng[1], rng[2], by = by)
  fmt <- if (span <= 120) "%d %b" else "%b %Y"
  graphics::axis(1,
    at = as.numeric(at), labels = format(at, fmt),
    col = th$muted, col.axis = th$fg, cex.axis = 0.8, lwd = 0, lwd.ticks = 1
  )
}

# ---------------------------------------------------------------------------
# Renderers
# ---------------------------------------------------------------------------

#' Plot a single-metric daily time series to PNG
#'
#' DATES on x-axis, the metric on y-axis. NA (non-wear) days leave gaps.
#'
#' @param readings A patient's readings tibble (already window-filtered).
#' @param patient_id Patient id, for the title.
#' @param metric One of "steps", "resting_hr", "sbp", "sedentary".
#' @param window_label Optional human window label for the subtitle.
#' @return PNG path, or NULL if nothing plottable.
#' @export
cdt_bot_plot_series <- function(readings, patient_id, metric,
                                window_label = NULL) {
  intent <- switch(metric,
    steps = "steps_over_time", resting_hr = "resting_hr_over_time",
    sbp = "sbp_over_time", sedentary = "sedentary_over_time", metric
  )
  spec <- .cdt_metric_spec(intent)
  if (is.null(spec) || is.null(readings) || nrow(readings) == 0) {
    return(NULL)
  }

  dates <- cdt_ts_to_date(readings$ts)
  y <- spec$derive(readings)
  ok <- !is.na(dates)
  dates <- dates[ok]
  y <- y[ok]
  if (length(y) == 0 || all(is.na(y))) {
    return(NULL)
  }

  th <- .cdt_theme()
  path <- .cdt_png_open(th)
  on.exit(grDevices::dev.off(), add = TRUE)

  graphics::par(
    mar = c(4.2, 4.6, 3.4, 1.4), family = th$family,
    col.axis = th$fg, col.lab = th$fg, fg = th$muted
  )
  ylim <- range(y, na.rm = TRUE)
  if (diff(ylim) == 0) ylim <- ylim + c(-1, 1)
  xlim <- range(as.numeric(dates))

  graphics::plot.new()
  graphics::plot.window(xlim = xlim, ylim = ylim)
  .cdt_grid(th, xlim, ylim)

  graphics::lines(as.numeric(dates), y, col = th$accent, lwd = 2.2)
  graphics::points(as.numeric(dates), y, col = th$accent, pch = 19, cex = 0.5)

  .cdt_date_axis(dates, th)
  graphics::axis(2,
    col = th$muted, col.axis = th$fg, cex.axis = 0.8,
    lwd = 0, lwd.ticks = 1, las = 1
  )

  sub <- if (!is.null(window_label)) {
    sprintf("Patient %s  \u00b7  %s", patient_id, window_label)
  } else {
    sprintf("Patient %s", patient_id)
  }
  graphics::title(
    main = spec$label, col.main = th$fg, font.main = 2, cex.main = 1.3,
    adj = 0, line = 1.8
  )
  graphics::mtext(sub, side = 3, adj = 0, line = 0.4, cex = 0.85, col = th$muted)
  graphics::title(ylab = sprintf("%s (%s)", spec$label, spec$unit), line = 3.1)
  graphics::mtext("Synthetic data", side = 1, adj = 1, line = 2.6,
    cex = 0.7, col = th$muted)

  path
}

#' Plot a functional / fall history overview to PNG (steps + resting HR)
#'
#' Two stacked panels sharing the date x-axis: daily steps (top) and resting HR
#' (bottom), with vertical markers at fall-event dates. This backs both the
#' `fall_history` and `functional_history` intents.
#'
#' @param readings A patient's readings tibble (already window-filtered).
#' @param patient_id Patient id, for the title.
#' @param fall_dates Optional vector of fall `Date`s (or character y-m-d).
#' @param window_label Optional human window label for the subtitle.
#' @return PNG path, or NULL if nothing plottable.
#' @export
cdt_bot_plot_history <- function(readings, patient_id, fall_dates = NULL,
                                 window_label = NULL) {
  if (is.null(readings) || nrow(readings) == 0) {
    return(NULL)
  }
  dates <- cdt_ts_to_date(readings$ts)
  ok <- !is.na(dates)
  if (!any(ok)) {
    return(NULL)
  }
  dates <- dates[ok]
  steps <- as.numeric(readings$step_count)[ok]
  rhr <- as.numeric(readings$resting_hr)[ok]
  if (all(is.na(steps)) && all(is.na(rhr))) {
    return(NULL)
  }

  fd <- NULL
  if (!is.null(fall_dates) && length(fall_dates) > 0) {
    fd <- as.Date(substr(as.character(fall_dates), 1, 10))
    fd <- fd[!is.na(fd) & fd >= min(dates) & fd <= max(dates)]
    if (length(fd) == 0) fd <- NULL
  }

  th <- .cdt_theme()
  path <- .cdt_png_open(th)
  on.exit(grDevices::dev.off(), add = TRUE)

  graphics::par(
    mfrow = c(2, 1), oma = c(3.4, 0.5, 3.6, 0.5),
    mar = c(1.6, 4.8, 1.2, 1.4), family = th$family,
    col.axis = th$fg, col.lab = th$fg, fg = th$muted
  )
  xlim <- range(as.numeric(dates))

  draw_panel <- function(y, colr, ylab, show_x) {
    if (all(is.na(y))) y <- rep(0, length(y))
    ylim <- range(y, na.rm = TRUE)
    if (diff(ylim) == 0) ylim <- ylim + c(-1, 1)
    graphics::plot.new()
    graphics::plot.window(xlim = xlim, ylim = ylim)
    .cdt_grid(th, xlim, ylim)
    if (!is.null(fd)) {
      graphics::abline(v = as.numeric(fd), col = th$marker, lwd = 1.4, lty = 2)
    }
    graphics::lines(as.numeric(dates), y, col = colr, lwd = 2.2)
    graphics::points(as.numeric(dates), y, col = colr, pch = 19, cex = 0.45)
    graphics::axis(2, col = th$muted, col.axis = th$fg, cex.axis = 0.8,
      lwd = 0, lwd.ticks = 1, las = 1)
    graphics::title(ylab = ylab, line = 3.2, col.lab = th$fg)
    if (show_x) .cdt_date_axis(dates, th)
  }

  draw_panel(steps, th$accent, "Daily steps", FALSE)
  draw_panel(rhr, th$accent2, "Resting HR (bpm)", TRUE)

  ttl <- if (!is.null(fd)) "Functional history with fall events" else "Functional history"
  sub <- if (!is.null(window_label)) {
    sprintf("Patient %s  \u00b7  %s", patient_id, window_label)
  } else {
    sprintf("Patient %s", patient_id)
  }
  graphics::mtext(ttl, side = 3, outer = TRUE, adj = 0, line = 1.4,
    font = 2, cex = 1.3, col = th$fg)
  graphics::mtext(sub, side = 3, outer = TRUE, adj = 0, line = 0.2,
    cex = 0.85, col = th$muted)
  if (!is.null(fd)) {
    graphics::mtext(
      sprintf("Dashed red = fall event (n=%d)", length(fd)),
      side = 3, outer = TRUE, adj = 1, line = 0.2, cex = 0.75, col = th$marker
    )
  }
  graphics::mtext("Synthetic data", side = 1, outer = TRUE, adj = 1,
    line = 2.0, cex = 0.7, col = th$muted)

  path
}

#' Plot baseline vs simulated (what-if) fall-risk bars to PNG
#'
#' Grouped bars at the 24h and 7d horizons: baseline vs simulated probability,
#' from a [predict_fall_risk()] result with `include_baseline = TRUE`.
#'
#' @param risk A list from `predict_fall_risk(..., include_baseline = TRUE)`.
#' @param patient_id Patient id, for the title.
#' @param caption Optional caption describing the counterfactual applied.
#' @return PNG path, or NULL if the baseline is missing.
#' @export
cdt_bot_plot_whatif <- function(risk, patient_id, caption = NULL) {
  if (is.null(risk) || is.null(risk$baseline)) {
    return(NULL)
  }
  base <- c(risk$baseline$p_24h, risk$baseline$p_7d)
  sim <- c(risk$p_24h, risk$p_7d)
  mat <- rbind(Baseline = base, `Simulated` = sim)
  colnames(mat) <- c("24-hour", "7-day")

  th <- .cdt_theme()
  path <- .cdt_png_open(th)
  on.exit(grDevices::dev.off(), add = TRUE)

  graphics::par(
    mar = c(4.4, 4.8, 3.6, 1.6), family = th$family,
    col.axis = th$fg, col.lab = th$fg, fg = th$muted
  )
  ymax <- max(c(base, sim, 0.05), na.rm = TRUE) * 1.25
  cols <- c(th$muted, th$accent)

  bp <- graphics::barplot(
    mat, beside = TRUE, col = cols, border = NA,
    ylim = c(0, ymax), axes = FALSE, names.arg = colnames(mat),
    cex.names = 0.9, col.axis = th$fg
  )
  graphics::abline(h = pretty(c(0, ymax)), col = th$grid, lwd = 1)
  # Redraw bars over the gridlines.
  graphics::barplot(
    mat, beside = TRUE, col = cols, border = NA, add = TRUE,
    ylim = c(0, ymax), axes = FALSE, names.arg = colnames(mat), cex.names = 0.9
  )
  graphics::axis(2, at = pretty(c(0, ymax)),
    labels = sprintf("%.0f%%", pretty(c(0, ymax)) * 100),
    col = th$muted, col.axis = th$fg, cex.axis = 0.8, lwd = 0,
    lwd.ticks = 1, las = 1)

  # Value labels above each bar.
  vals <- as.numeric(mat)
  graphics::text(as.numeric(bp), vals + ymax * 0.03,
    labels = sprintf("%.1f%%", vals * 100), cex = 0.8, col = th$fg)

  graphics::title(main = "What-if fall risk: baseline vs simulated",
    col.main = th$fg, font.main = 2, cex.main = 1.25, adj = 0, line = 1.9)
  graphics::mtext(sprintf("Patient %s", patient_id), side = 3, adj = 0,
    line = 0.5, cex = 0.85, col = th$muted)
  graphics::title(ylab = "P(fall)", line = 3.2)
  graphics::legend("topright", legend = rownames(mat), fill = cols,
    border = NA, bty = "n", cex = 0.85, text.col = th$fg)
  if (!is.null(caption) && nzchar(caption)) {
    graphics::mtext(caption, side = 1, adj = 0, line = 2.7,
      cex = 0.75, col = th$muted)
  }
  graphics::mtext("Synthetic data", side = 1, adj = 1, line = 2.7,
    cex = 0.7, col = th$muted)

  path
}
