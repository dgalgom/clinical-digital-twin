#' Shift-triage change detection (P0-1)
#'
#' The clinical bottleneck in a nursing home is not "who is frail" (that list is
#' stable and gets tuned out) but "who *changed* since the last shift, and why".
#' `cdt_compute_alerts()` snapshots the cohort's current risk, diffs it against
#' the previously stored snapshot, and emits an alert row for each meaningful
#' movement (a risk jump or an upward tier crossing) with a one-line reason that
#' cites the patient's top-moving sensor feature.
#'
#' Structure is ported from the human-digital-twin `AlarmDetector`: compare a
#' current value to a baseline, fire on a threshold + direction, tag a severity,
#' and group by patient. Thresholds live in [cdt_alert_config()], not here.

# Human-readable reason fragment for a patient's dominant recent movement.
# Uses only features already engineered by cdt_assemble_features(); no new math.
.cdt_alert_reason <- function(feats) {
  bits <- character(0)
  st <- feats$steps_trend_7d
  if (!is.null(st) && is.finite(st) && st < -5) {
    bits <- c(bits, sprintf("steps declining (%.0f/day)", st))
  }
  hrt <- feats$resting_hr_trend_7d
  if (!is.null(hrt) && is.finite(hrt) && hrt > 0.3) {
    bits <- c(bits, sprintf("resting HR rising (%+.1f bpm/day)", hrt))
  }
  sed <- feats$sedentary_hours_mean_7d
  if (!is.null(sed) && is.finite(sed) && sed > 16) {
    bits <- c(bits, sprintf("high sedentary time (%.1f h/day)", sed))
  }
  sbp <- feats$sbp_mean_7d
  if (!is.null(sbp) && is.finite(sbp) && sbp < 110) {
    bits <- c(bits, sprintf("low systolic BP (%.0f mmHg)", sbp))
  }
  if (length(bits) == 0) return("risk model inputs shifted")
  paste(utils::head(bits, 2), collapse = "; ")
}

.cdt_alert_severity <- function(delta_pts, cfg) {
  if (delta_pts >= cfg$critical_pts) return("critical")
  if (delta_pts >= cfg$warning_pts) return("warning")
  "info"
}

# Numeric rank of a tier label (Low < Moderate < High), NA-safe.
.cdt_tier_rank <- function(tier) {
  match(as.character(tier), c("Low", "Moderate", "High"))
}

#' Compute shift-triage alerts against the last stored snapshot (P0-1)
#'
#' Diffs the current cohort risk against the previously persisted snapshot,
#' inserts an alert per meaningful movement, then (optionally) records the
#' current risk as the new snapshot for next time.
#'
#' @param con A DBI connection.
#' @param model A `cdt_model`.
#' @param as_of Label for the new snapshot (default: today's ISO date).
#' @param write_snapshot If `TRUE` (default), persist the current risk as a new
#'   snapshot after diffing. Set `FALSE` for a read-only "what would fire?" pass.
#' @return A tibble of the alerts inserted this run (zero-row if none).
#' @export
cdt_compute_alerts <- function(con, model, as_of = as.character(Sys.Date()),
                               write_snapshot = TRUE) {
  cfg <- cdt_alert_config()
  current <- cdt_cohort_snapshot(con, model)
  if (nrow(current) == 0) {
    return(tibble::tibble())
  }
  prev <- cdt_get_last_snapshot(con)

  fired <- list()
  if (nrow(prev) > 0) {
    prev_by <- prev[!duplicated(prev$patient_id), ]
    for (i in seq_len(nrow(current))) {
      pid <- current$patient_id[i]
      p_now <- current$p_7d[i]
      pr <- prev_by[prev_by$patient_id == pid, ]
      if (nrow(pr) == 0 || !is.finite(p_now) || !is.finite(pr$p_7d[1])) next
      delta_pts <- 100 * (p_now - pr$p_7d[1])

      tier_now <- .cdt_tier_rank(current$tier_7d[i])
      tier_prev <- .cdt_tier_rank(pr$tier_7d[1])
      tier_crossed_up <- is.finite(tier_now) && is.finite(tier_prev) &&
        tier_now > tier_prev

      if (delta_pts >= cfg$jump_pts || tier_crossed_up) {
        # Reason cites the patient's own recent sensor movement.
        patient <- cdt_get_patient(con, pid)
        readings <- cdt_get_patient_timeline(con, pid)
        feats <- tryCatch(cdt_assemble_features(patient, readings),
          error = function(e) NULL)
        reason_move <- if (is.null(feats)) "risk model inputs shifted" else
          .cdt_alert_reason(feats)

        kind <- if (tier_crossed_up) "tier_up" else "risk_jump"
        sev <- if (tier_crossed_up) {
          # Tier crossing is at least the configured severity; escalate on jump.
          max_sev <- .cdt_alert_severity(max(delta_pts, 0), cfg)
          ord <- c(info = 1L, warning = 2L, critical = 3L)
          if (ord[[max_sev]] >= ord[[cfg$tier_up_severity]]) max_sev else
            cfg$tier_up_severity
        } else {
          .cdt_alert_severity(delta_pts, cfg)
        }

        reason <- sprintf(
          "7d risk %+.1f pts (%s -> %s); %s",
          delta_pts, pr$tier_7d[1], current$tier_7d[i], reason_move)

        id <- cdt_insert_alert(con, pid, kind = kind, severity = sev,
          delta_pts = delta_pts, reason_text = reason)
        fired[[length(fired) + 1L]] <- tibble::tibble(
          alert_id = id, patient_id = pid, kind = kind, severity = sev,
          delta_pts = delta_pts, reason_text = reason)
      }
    }
  }

  if (isTRUE(write_snapshot)) {
    cdt_write_risk_snapshot(con, current, as_of = as_of)
  }

  if (length(fired) == 0) tibble::tibble() else dplyr::bind_rows(fired)
}
