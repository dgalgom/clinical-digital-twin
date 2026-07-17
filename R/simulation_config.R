#' Multi-agent simulation configuration
#'
#' Hand-authored, deterministic configuration for the 30-day multi-agent
#' simulation of the fictional "Los Almendros" care-home module (10 residents,
#' P01-P10). Everything here is data, not logic: the institution profile, the 10
#' patient fichas (as canonical `patients`-schema rows plus a behavioural
#' `system_prompt`), the social affinity matrix, the flu-outbreak parameters, and
#' the blind P08 fall experiment parameters. Keeping it as config (accessor
#' functions) — mirroring `config.R` — means the orchestrator stays free of
#' hardcoded content. All data is synthetic; no real PHI.

#' Institution profile for the simulated module
#'
#' Anchored in real Sevilla/Andalucia staffing norms (Decreto 388/2010 + 2022
#' state framework): 84-plaza centre, our 10 residents form one module; no
#' titrated night nurse; physiotherapy 3 mornings/week; doctor 2 days/week.
#'
#' @return A named list describing the institution and its operating rhythm.
#' @export
cdt_sim_institution <- function() {
  list(
    name = "Residencia Los Almendros",
    location = "Sevilla (urban, high-density)",
    total_plazas = 84L,
    module_size = 10L,
    night_nurse = FALSE,
    physio_days = c("Mon", "Wed", "Fri"),
    physio_period = "morning",
    doctor_days = c("Tue", "Thu"),
    activity_days = c("Mon", "Tue", "Wed", "Thu", "Fri"),
    activity_window = "11:00-12:30",
    # Auxiliary-per-resident coverage by shift (module of 10).
    staff_ratio = list(morning = 0.20, afternoon = 0.12, night = 0.08),
    weekend_activity = FALSE,
    weekend_staff_multiplier = 0.75
  )
}

#' The 10 simulated patient fichas in the canonical clinical schema
#'
#' Each row uses the exact `patients`-table columns produced by
#' [cdt_generate_cohort()] (so [cdt_assemble_features()] works unchanged), plus
#' two extra columns used only by the simulation: `system_prompt` (the agent's
#' behavioural persona) and `baseline_notes`. Note the canonical schema encodes
#' `prior_falls` as a 0/1 flag, not a count.
#'
#' @return A tibble with one row per resident (P01-P10).
#' @export
cdt_sim_patients <- function() {
  tibble::tibble(
    patient_id = sprintf("P%02d", 1:10),
    name = paste0("[SYNTHETIC] ", c(
      "Rosario", "Antonio", "Carmen", "Manuel", "Dolores",
      "Francisco", "Isabel", "Joaquin", "Pilar", "Vicente"
    )),
    age = as.integer(c(88, 82, 91, 79, 85, 90, 83, 77, 94, 86)),
    sex = c("F", "M", "F", "M", "F", "M", "F", "M", "F", "M"),
    parkinsons = as.integer(c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0)),
    osteoporosis = as.integer(c(1, 0, 0, 0, 1, 0, 1, 0, 0, 0)),
    orthostatic_hypotension = as.integer(c(0, 0, 1, 0, 0, 1, 0, 0, 0, 0)),
    polypharmacy = as.integer(c(0, 1, 0, 0, 0, 1, 1, 0, 1, 0)),
    prior_falls = as.integer(c(1, 1, 1, 0, 0, 1, 1, 0, 1, 0)),
    n_medications = as.integer(c(3, 6, 4, 1, 3, 6, 5, 3, 7, 2)),
    medications = c(
      "alendronate;calcium+vitD;amlodipine",
      "levodopa;amlodipine;lisinopril;sertraline;gabapentin;calcium+vitD",
      "donepezil;amlodipine;tamsulosin;calcium+vitD",
      "atorvastatin",
      "amlodipine;gabapentin;calcium+vitD",
      "furosemide;lisinopril;amlodipine;atorvastatin;warfarin;calcium+vitD",
      "lorazepam;sertraline;alendronate;calcium+vitD;amlodipine",
      "metformin;gabapentin;amlodipine",
      "donepezil;amlodipine;furosemide;sertraline;calcium+vitD;gabapentin;lorazepam",
      "atorvastatin;amlodipine"
    ),
    comorbidities = c(
      "hypertension;arthritis",
      "hypertension;depression",
      "cognitive_impairment;hypertension",
      "",
      "arthritis",
      "hypertension;atrial_fibrillation;chronic_kidney_disease",
      "depression;arthritis",
      "type2_diabetes;peripheral_neuropathy",
      "cognitive_impairment;hypertension",
      "hypertension"
    ),
    system_prompt = c(
      # P01 Rosario
      paste(
        "You are Rosario, 88, severe osteoporosis with a prior vertebral fracture.",
        "You have significant fear of falling and tend to AVOID activity out of",
        "caution more than physical incapacity. On days with no physiotherapist,",
        "reduce your mobility more than your capacity would justify, from anxiety.",
        "You never accept high-impact or spinal-flexion exercise."
      ),
      # P02 Antonio
      paste(
        "You are Antonio, 82, Parkinson's (Hoehn-Yahr 2-3), polypharmacy.",
        "You are proud and minimise symptoms to staff ('I'm fine'), so your",
        "reported mood understates reality, but your actual mobility faithfully",
        "reflects your motor state. Mobility varies markedly with time since your",
        "last medication dose (simulate wearing-off in the 2h before each dose)."
      ),
      # P03 Carmen
      paste(
        "You are Carmen, 91, mild-moderate cognitive decline, orthostatic",
        "hypotension, urinary incontinence with frequent night rises. Your real",
        "risk is concentrated at NIGHT, not during the day. Generate explicit",
        "'night rise with postural dizziness' notable events on several nights.",
        "Your daytime behaviour is stable and not predictive of your real risk."
      ),
      # P04 Manuel
      paste(
        "You are Manuel, 79, no major comorbidity, good relative fitness. You are",
        "the stable low-risk control and the social connector. Keep a stable,",
        "healthy baseline unless the orchestrator explicitly signals a discrete",
        "clinical event."
      ),
      # P05 Dolores
      paste(
        "You are Dolores, 85, osteoporosis, bilateral knee arthrosis, overweight.",
        "Knee pain limits your gait, worse in the AFTERNOON after a morning of",
        "activity. Your mobility must be notably lower in the afternoons than the",
        "mornings each day (cumulative daytime pain pattern)."
      ),
      # P06 Francisco
      paste(
        "You are Francisco, 90, mild-moderate heart failure, orthostatic",
        "hypotension, a NEW diuretic recently added. From the day the orchestrator",
        "marks 'diuretic start', reduce your mobility progressively over 5-7 days",
        "and report dizziness on standing more often."
      ),
      # P07 Isabel
      paste(
        "You are Isabel, 83, osteoporosis with a prior hip fracture (recovered),",
        "chronic night benzodiazepine. Report greater instability and drowsiness",
        "in the first 3 hours after waking, consistently, unless the orchestrator",
        "signals a benzodiazepine dose reduction."
      ),
      # P08 Joaquin (blind experiment)
      paste(
        "You are Joaquin, 77, type-2 diabetes, mild peripheral neuropathy",
        "(reduced foot sensation). Independent and somewhat stubborn; you decline",
        "help ('I can manage'). Keep activity levels relatively high and stable.",
        "Your real risk is SUBTLE and must NOT be obvious from step volume alone;",
        "you rarely verbalise it as a notable event."
      ),
      # P09 Pilar
      paste(
        "You are Pilar, 94, moderate-severe dementia, very reduced mobility, walker",
        "with supervision. High baseline risk. Keep minimal day-to-day variability;",
        "occasionally generate night-agitation events with unsupervised standing",
        "attempts. Do not produce artificial improvements."
      ),
      # P10 Vicente
      paste(
        "You are Vicente, 86, recovering from a recent pneumonia hospitalisation,",
        "post-hospital weakness, no major chronic comorbidity. You were very active",
        "before. Gradually increase your mobility over the simulation (realistic,",
        "non-linear post-hospital recovery with the occasional setback day)."
      )
    ),
    baseline_notes = c(
      "steps~1400 rhr~74 sbp~138 sed~10.5",
      "steps~900 rhr~68 sbp~122 sed~12",
      "steps~1100 rhr~80 sbp~118/96 sed~11 night-risk",
      "steps~3800 rhr~65 sbp~128 sed~7 control",
      "steps~1600 rhr~76 sbp~145 sed~10 afternoon-pain",
      "steps~1200 rhr~84 sbp~130/105 sed~11.5 diuretic-event",
      "steps~1300 rhr~72 sbp~125 sed~10.5 benzo-AM",
      "steps~2400 rhr~70 sbp~135 sed~8.5 blind-experiment",
      "steps~450 rhr~78 sbp~128 sed~14 high-risk-stable",
      "steps~600 rhr~88 sbp~115 sed~13 recovery-trajectory"
    )
  )
}

#' Social affinity matrix for the 10 residents
#'
#' A symmetric-by-construction 10x10 matrix of affinity weights derived from the
#' fichas: 0 = none, 1 = low, 2 = medium, 3 = high. P04 (connector) has high
#' affinity with most; P09 (advanced dementia) never *initiates* interaction
#' (its whole row is 0) but may be the object of interaction (its column is
#' non-zero). Diagonal is 0 (no self-interaction).
#'
#' @return A 10x10 numeric matrix with `P01`..`P10` row/column names.
#' @export
cdt_sim_affinity_matrix <- function() {
  ids <- sprintf("P%02d", 1:10)
  m <- matrix(0L, nrow = 10, ncol = 10, dimnames = list(ids, ids))
  # Helper to set a symmetric pair.
  pair <- function(a, b, w) {
    m[a, b] <<- w
    m[b, a] <<- w
  }
  pair("P01", "P05", 3L) # both limited-mobility, share sedentary activities
  pair("P01", "P04", 2L)
  pair("P01", "P10", 1L)
  pair("P04", "P05", 3L) # connector
  pair("P04", "P06", 2L)
  pair("P04", "P08", 2L)
  pair("P04", "P10", 3L)
  pair("P04", "P03", 2L)
  pair("P05", "P07", 1L)
  pair("P06", "P10", 2L)
  pair("P08", "P10", 2L)
  pair("P02", "P08", 1L) # both reserved, occasional
  pair("P03", "P05", 1L)
  # P09 initiates nothing: force its row to 0 (others may still reach it via
  # their own rows, e.g. P04 sitting with P09).
  m["P09", ] <- 0L
  m["P04", "P09"] <- 2L # P04 reaches out to P09
  diag(m) <- 0L
  m
}

#' Flu-outbreak scenario parameters (Sim 2 only)
#'
#' The outbreak is the only difference between `sim1_baseline` and `sim2_flu`,
#' applied identically to both branches within Sim 2.
#'
#' @return A named list of flu parameters.
#' @export
cdt_sim_flu_config <- function() {
  list(
    start_day = 10L,
    duration_days = 6L,
    affected = c("P06", "P09"),
    resting_hr_bump = 8,          # bpm above baseline while febrile (plausible)
    mobility_multiplier = 0.6,    # affected residents move ~40% less
    staff_reduction = 0.25        # module auxiliary time reduced ~25% for all
  )
}

#' Blind P08 fall-experiment parameters
#'
#' The hidden latent risk begins rising at a seed-chosen day within
#' `latent_onset_window`; the fall is sampled stochastically from a rising hazard
#' capped at `hazard_ceiling`. Branch B fires a predefined intervention when the
#' model's reported risk crosses a threshold, reducing the hidden hazard.
#'
#' P08 (Joaquin, diabetic peripheral neuropathy) deteriorates the way real
#' neuropathy-driven fall risk does: NOT through falling step volume — he stays
#' independent and keeps walking — but through subtler channels the wearable
#' still sees. As the hidden latent risk rises, `projection` maps it onto the
#' three observable features the model is sensitive to (verified empirically):
#' more sedentary/guarding hours (primary), less steady gait (lower accelerometer
#' magnitude), and a mild resting-HR drift. This is what lets the fall-risk model
#' actually DETECT the deterioration — so Branch B's model risk can cross the
#' intervention threshold — while step count stays high, honouring the persona
#' that the risk "must not be obvious from step volume alone". The projection is
#' scaled by `latent / hazard_ceiling` (0 before onset, 1 at ceiling). The
#' intervention dampens the *observable* deterioration too (`intervention_relief`)
#' — a preventive review improves the resident, so the signal recovers and the
#' branch does not re-fire every subsequent day.
#'
#' @return A named list of experiment parameters.
#' @export
cdt_sim_p08_experiment <- function() {
  list(
    patient = "P08",
    seed = 20260401L,
    latent_onset_window = c(12L, 20L), # day D drawn from this inclusive window
    hazard_ceiling = 0.18,             # max per-day fall probability
    hazard_slope = 0.02,               # per-day latent risk increment after D
    intervention_threshold_24h = 0.15,
    intervention_threshold_7d = 0.25,
    hazard_reduction = 0.6,            # Branch B multiplies hazard by this
    # Latent -> observable projection (see @details). Coefficients are the value
    # added/multiplied at FULL latent (latent == hazard_ceiling); the actual
    # daily effect scales linearly with latent / hazard_ceiling.
    projection = list(
      sedentary_hours_add = 6.0,   # +hours sitting/lying at full latent
      accel_magnitude_mult = 0.80, # gait steadiness multiplier at full latent
      resting_hr_add = 6.0,        # +bpm resting-HR drift at full latent
      intervention_relief = 0.6    # post-intervention: observable effect x this
    )
  )
}
