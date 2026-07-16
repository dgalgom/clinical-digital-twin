#' Simulation orchestrator + hidden P08 blind experiment (Phase 5)
#'
#' Runs the day-by-day 8-step pipeline for one run `(simulation_id, branch)` over
#' the fixed 10-resident module, persisting into the isolated simulation tables.
#' The blind experiment lives entirely inside the hidden orchestrator: P08's
#' latent risk rises from a seed-chosen onset day and a fall is sampled
#' stochastically from a rising hazard. Branch A is the control; Branch B fires a
#' predefined intervention when the MODEL's reported risk crosses a threshold,
#' which lowers the hidden hazard. A and B share one base RNG stream so the
#' counterfactual is valid. The ground truth is written ONLY to the restricted
#' `ground_truth_evaluation` table; nothing on the clinical surface can read it.

# Assemble features + predict for one patient on THIS run's timeline only. This
# is the run-scoped analogue of cdt_patient_risk: it must never read production
# rows or another run's rows.
.cdt_sim_patient_risk <- function(con, model, simulation_id, branch, patient) {
  readings <- cdt_sim_get_patient_timeline(con, simulation_id, branch,
    patient$patient_id)
  if (nrow(readings) == 0) {
    return(NULL)
  }
  fr <- cdt_assemble_features(patient, readings)
  predict_fall_risk(model, fr, include_baseline = FALSE)
}

# Initialise the hidden P08 latent experiment: draw the onset day from the
# configured window using the run's base RNG stream (so A and B agree).
.cdt_p08_latent_init <- function(exp_cfg) {
  win <- exp_cfg$latent_onset_window
  onset <- sample(seq.int(win[1], win[2]), 1)
  list(onset = onset, latent = 0)
}

# Advance the hidden latent risk one day and derive the day's hazard.
.cdt_p08_update_latent <- function(state, day, exp_cfg) {
  if (day >= state$onset) {
    state$latent <- min(exp_cfg$hazard_ceiling,
      state$latent + exp_cfg$hazard_slope)
  }
  state$hazard <- state$latent
  state
}

#' Run a single simulation branch end-to-end
#'
#' @param con A DBI connection with both schemas initialised and the 10 patients
#'   already written to `patients`.
#' @param model A trained `cdt_model` (or a path).
#' @param simulation_id Run id (e.g. "sim1_baseline", "sim2_flu").
#' @param branch "A" (control) or "B" (intervention).
#' @param days Number of days (default 30; fast mode 3).
#' @param seed Base RNG seed for this run.
#' @param mock Mock-LLM override passed to the agent path.
#' @param flu Logical; enable the flu outbreak (Sim 2).
#' @param start_date Date of day 1.
#' @return Invisibly a list with per-day gate statuses and the P08 onset day.
#' @export
cdt_run_simulation <- function(con, model, simulation_id, branch, days = 30,
                               seed = 20260401L, mock = NULL, flu = FALSE,
                               start_date = cdt_data_start_date()) {
  stopifnot(branch %in% c("A", "B"))
  if (is.character(model)) {
    model <- cdt_load_model(model)
  }
  patients <- cdt_sim_patients()
  institution <- cdt_sim_institution()
  affinity <- cdt_sim_affinity_matrix()
  flu_cfg <- cdt_sim_flu_config()
  exp_cfg <- cdt_sim_p08_experiment()

  # Base RNG stream: identical across A and B so the P08 counterfactual and all
  # shared stochasticity line up. The intervention is the ONLY divergence.
  set.seed(seed)
  p08_state <- .cdt_p08_latent_init(exp_cfg)

  prior_decisions <- stats::setNames(vector("list", nrow(patients)),
    patients$patient_id)
  prior_pred <- stats::setNames(vector("list", nrow(patients)),
    patients$patient_id)
  gate_statuses <- character(0)

  weekdays3 <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

  for (day in seq_len(days)) {
    dow <- weekdays3[((as.integer(format(start_date + day - 1, "%u")) - 1) %% 7) + 1]
    is_weekend <- dow %in% c("Sat", "Sun")
    physio_today <- dow %in% institution$physio_days
    flu_active <- flu && day >= flu_cfg$start_day &&
      day < (flu_cfg$start_day + flu_cfg$duration_days)

    # (2) Social layer -------------------------------------------------------
    soc <- cdt_sim_social_day(day, affinity, institution, mock = mock,
      seed = seed + day)
    if (nrow(soc$rows) > 0) {
      soc_rows <- soc$rows
      soc_rows$simulation_id <- simulation_id
      soc_rows$branch <- branch
      cdt_sim_write_social(con, soc_rows)
    }
    social_status <- validate_social_interactions(soc$rows,
      patients$patient_id)

    # (3) Agents + (4) sensors + (5) inference, per patient ------------------
    decision_rows <- list()
    sensor_rows <- list()
    pred_rows <- list()
    bio_status <- "pass"
    agent_status <- "pass"
    model_status <- "pass"

    for (i in seq_len(nrow(patients))) {
      pt <- patients[i, , drop = FALSE]
      pid <- pt$patient_id
      ctx <- list(
        weekend = is_weekend, physio_today = physio_today,
        flu_active = flu_active,
        social_summary = if (length(soc$context[[pid]])) {
          paste(soc$context[[pid]], collapse = " ")
        } else {
          NULL
        },
        prior_mood = prior_decisions[[pid]]$mood_fatigue %||% NULL
      )

      agent <- cdt_call_agent(pt, day, ctx, prior_decision = prior_decisions[[pid]],
        mock = mock)
      dec <- agent$decision
      prior_decisions[[pid]] <- dec
      if (validate_agent_json(dec)$status == "fail") agent_status <- "fail"

      # Flu context only for affected patients on active days.
      flu_ctx <- if (flu_active && pid %in% flu_cfg$affected) flu_cfg else NULL
      reading <- cdt_sim_day_sensors(pt, dec, institution, day,
        start_date = start_date, flu_ctx = flu_ctx, seed = seed + 100 * day + i,
        simulation_id = simulation_id, branch = branch)
      bio <- validate_biological_plausibility(reading)
      if (bio$status == "fail") bio_status <- "fail"
      cdt_db_write(con, "sensor_readings", reading, append = TRUE)

      # Persist the decision row (after the sensor write so the day's timeline
      # exists for inference).
      decision_rows[[pid]] <- data.frame(
        simulation_id = simulation_id, branch = branch, day = as.integer(day),
        patient_id = pid,
        mobility_pct_of_baseline = as.numeric(dec$mobility_pct_of_baseline),
        participated_group_activity = as.integer(dec$participated_group_activity),
        medication_adherence = as.integer(dec$medication_adherence),
        meaningful_social_interaction = as.integer(dec$meaningful_social_interaction),
        mood_fatigue = as.character(dec$mood_fatigue),
        notable_event = as.character(dec$notable_event %||% NA_character_),
        confidence = as.numeric(dec$confidence),
        agent_output_invalid = as.integer(agent$invalid),
        temperature = as.numeric(agent$temperature),
        prompt_text = agent$prompt,
        raw_reply = as.character(agent$raw %||% NA_character_),
        stringsAsFactors = FALSE
      )

      pred <- .cdt_sim_patient_risk(con, model, simulation_id, branch, pt)
      if (is.null(pred)) next
      mo <- validate_model_output(pred,
        prior_p7d = prior_pred[[pid]]$p_7d %||% NULL)
      if (mo$status == "fail") model_status <- "fail"
      prior_pred[[pid]] <- pred
      pred_rows[[pid]] <- data.frame(
        simulation_id = simulation_id, branch = branch, day = as.integer(day),
        patient_id = pid,
        p_24h = pred$p_24h, p_7d = pred$p_7d,
        tier_24h = pred$tier_24h, tier_7d = pred$tier_7d,
        quality_flag = if (length(mo$issues)) paste(mo$issues, collapse = "; ") else NA_character_,
        stringsAsFactors = FALSE
      )
    }

    if (length(decision_rows) > 0) {
      cdt_sim_write_agent_decisions(con, do.call(rbind, decision_rows))
    }
    if (length(pred_rows) > 0) {
      cdt_sim_write_predictions(con, do.call(rbind, pred_rows))
    }

    # (6)+(7) Hidden P08 experiment -----------------------------------------
    p08_state <- .cdt_p08_update_latent(p08_state, day, exp_cfg)
    intervention_fired <- 0L
    hazard <- p08_state$hazard
    p08_pred <- prior_pred[["P08"]]
    if (branch == "B" && !is.null(p08_pred)) {
      crossed <- (p08_pred$p_24h >= exp_cfg$intervention_threshold_24h) ||
        (p08_pred$p_7d >= exp_cfg$intervention_threshold_7d)
      if (crossed) {
        intervention_fired <- 1L
        # Log a real clinician-style intervention and lower the hidden hazard.
        cdt_log_intervention(con, "P08", "Simulated preventive review",
          detail = sprintf("branch B day %d threshold crossing", day),
          created_by = "simulation")
        hazard <- hazard * exp_cfg$hazard_reduction
      }
    }
    fall_sampled <- as.integer(stats::rbinom(1, 1, min(1, max(0, hazard))))
    cdt_sim_write_ground_truth(con, simulation_id, branch, day, "P08",
      latent_risk = p08_state$latent, hazard = hazard,
      fall_sampled = fall_sampled, intervention_fired = intervention_fired)

    # (8) Day gate -----------------------------------------------------------
    gate <- run_daily_checkpoint_gate(con, simulation_id, branch, day, list(
      social = social_status,
      agent_json = list(status = agent_status, issues = character(0)),
      biological = list(status = bio_status, issues = character(0)),
      model = list(status = model_status, issues = character(0))
    ))
    gate_statuses <- c(gate_statuses, gate$status)
    if (gate$status == "fail") {
      break
    }
  }

  invisible(list(
    gate_statuses = gate_statuses,
    p08_onset = p08_state$onset,
    days_completed = length(gate_statuses)
  ))
}
