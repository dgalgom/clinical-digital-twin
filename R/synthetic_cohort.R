#' Synthetic patient cohort generation
#'
#' Generates a clearly-fake cohort with realistic distributions of fall-risk
#' factors. NO REAL PHI is used or produced. Every value is drawn from a random
#' number generator seeded for reproducibility.

#' Fake given/family name parts for synthetic labels
#'
#' These are obviously-generic tokens so no output resembles a real person.
#' @keywords internal
.cdt_fake_name_parts <- function() {
  list(
    given = c(
      "Ada", "Boro", "Ciri", "Devi", "Enzo", "Faye", "Goro", "Hana",
      "Iris", "Juno", "Kato", "Lumi", "Milo", "Nova", "Otto", "Pia",
      "Quin", "Rue", "Suki", "Taro", "Uma", "Vex", "Wren", "Xara",
      "Yuki", "Zed"
    ),
    family = c(
      "Alder", "Birch", "Cedar", "Dogwood", "Elm", "Fir", "Gum",
      "Hazel", "Ivy", "Juniper", "Koa", "Larch", "Maple", "Neem",
      "Oak", "Pine", "Quince", "Rowan", "Spruce", "Teak", "Willow"
    )
  )
}

#' Generate a synthetic patient cohort
#'
#' Fall-risk factor prevalence roughly mirrors an elderly at-risk population but
#' the numbers are illustrative, not epidemiologically calibrated.
#'
#' @param n Number of patients (default 80; recommend 50-100).
#' @param seed RNG seed for reproducibility.
#' @return A tibble with one row per patient in the canonical clinical schema.
#' @export
cdt_generate_cohort <- function(n = 80, seed = 42) {
  stopifnot(n >= 1)
  set.seed(seed)

  parts <- .cdt_fake_name_parts()

  patient_id <- sprintf("P%03d", seq_len(n))

  # Elderly-skewed age distribution, clamped to a plausible range.
  age <- round(pmin(pmax(stats::rnorm(n, mean = 74, sd = 9), 55), 98))
  sex <- sample(c("F", "M"), n, replace = TRUE, prob = c(0.56, 0.44))

  # Risk-factor prevalence (independent Bernoulli draws for simplicity).
  parkinsons <- stats::rbinom(n, 1, 0.12)
  osteoporosis <- stats::rbinom(n, 1, prob = ifelse(sex == "F", 0.40, 0.18))
  orthostatic_hypotension <- stats::rbinom(n, 1, 0.22)

  # Medication count rises with age; polypharmacy flagged at >= 5.
  n_medications <- stats::rpois(n, lambda = pmax(1, (age - 55) / 12))
  n_medications <- pmin(n_medications, 14L)
  polypharmacy <- as.integer(n_medications >= 5)

  # Prior falls correlate with the other risk factors.
  fall_logit <- -1.8 + 0.9 * parkinsons + 0.6 * osteoporosis +
    0.5 * orthostatic_hypotension + 0.4 * polypharmacy + (age - 74) / 20
  prior_falls <- stats::rbinom(n, 1, plogis(fall_logit))

  med_pool <- c(
    "levodopa", "amlodipine", "lisinopril", "furosemide", "warfarin",
    "metformin", "atorvastatin", "sertraline", "lorazepam", "gabapentin",
    "alendronate", "calcium+vitD", "tamsulosin", "donepezil"
  )
  comorbid_pool <- c(
    "hypertension", "type2_diabetes", "atrial_fibrillation",
    "chronic_kidney_disease", "depression", "cognitive_impairment",
    "peripheral_neuropathy", "arthritis"
  )

  medications <- vapply(seq_len(n), function(i) {
    k <- min(n_medications[i], length(med_pool))
    if (k <= 0) {
      return("")
    }
    paste(sample(med_pool, k), collapse = ";")
  }, character(1))

  comorbidities <- vapply(seq_len(n), function(i) {
    k <- sample(0:4, 1)
    if (k == 0) {
      return("")
    }
    paste(sample(comorbid_pool, k), collapse = ";")
  }, character(1))

  name <- paste(
    sample(parts$given, n, replace = TRUE),
    sample(parts$family, n, replace = TRUE)
  )

  tibble::tibble(
    patient_id = patient_id,
    name = paste0("[SYNTHETIC] ", name),
    age = as.integer(age),
    sex = sex,
    parkinsons = as.integer(parkinsons),
    osteoporosis = as.integer(osteoporosis),
    orthostatic_hypotension = as.integer(orthostatic_hypotension),
    polypharmacy = as.integer(polypharmacy),
    prior_falls = as.integer(prior_falls),
    n_medications = as.integer(n_medications),
    medications = medications,
    comorbidities = comorbidities
  )
}
