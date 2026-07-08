#' Data ingestion and preprocessing
#'
#' Takes a raw "institution" patient CSV and normalizes it into the canonical
#' clinical schema used throughout the system. Robust to column-name variants
#' and missing optional fields. Works only on synthetic data for this project.

#' Canonical clinical column names produced by ingestion
#'
#' @return Character vector of canonical column names.
#' @export
cdt_canonical_patient_cols <- function() {
  c(
    "patient_id", "name", "age", "sex", "parkinsons", "osteoporosis",
    "orthostatic_hypotension", "polypharmacy", "prior_falls",
    "n_medications", "medications", "comorbidities"
  )
}

#' Normalize a sex value to "F"/"M"
#'
#' @param x Character/logical vector of raw sex values.
#' @return Character vector of "F"/"M" (NA passed through as "F" default-safe).
#' @keywords internal
.cdt_normalize_sex <- function(x) {
  x <- toupper(trimws(as.character(x)))
  out <- ifelse(x %in% c("M", "MALE", "1"), "M",
    ifelse(x %in% c("F", "FEMALE", "0", "2"), "F", NA_character_)
  )
  out[is.na(out)] <- "F"
  out
}

#' Coerce a value to a 0/1 integer flag
#'
#' Accepts logicals, 0/1, "yes"/"no", "true"/"false", "y"/"n".
#'
#' @param x Vector of raw flag values.
#' @return Integer vector of 0/1.
#' @keywords internal
.cdt_as_flag <- function(x) {
  if (is.logical(x)) {
    return(as.integer(x))
  }
  xs <- tolower(trimws(as.character(x)))
  as.integer(xs %in% c("1", "yes", "y", "true", "t"))
}

#' Ingest a patient CSV into the canonical schema
#'
#' Column-name matching is case-insensitive and tolerant of common variants
#' (e.g. `id`/`patient_id`, `gender`/`sex`). Missing optional flags default to 0.
#'
#' @param path Path to a CSV file, OR a data frame already in memory.
#' @return A tibble in the canonical clinical schema.
#' @export
cdt_ingest_patient_csv <- function(path) {
  raw <- if (is.data.frame(path)) {
    path
  } else {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  }
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)

  # Build a lowercase-name lookup for flexible matching.
  lc <- tolower(trimws(names(raw)))
  pick <- function(...) {
    aliases <- tolower(c(...))
    idx <- which(lc %in% aliases)
    if (length(idx) == 0) {
      return(NULL)
    }
    raw[[idx[1]]]
  }

  n <- nrow(raw)
  pid <- pick("patient_id", "id", "pid")
  if (is.null(pid)) {
    pid <- sprintf("P%03d", seq_len(n))
  }
  pid <- as.character(pid)

  age_raw <- pick("age")
  age <- if (is.null(age_raw)) rep(NA_integer_, n) else suppressWarnings(as.integer(age_raw))
  # Impute missing age with the cohort median (documented MVP behavior).
  if (any(is.na(age))) {
    med <- suppressWarnings(stats::median(age, na.rm = TRUE))
    if (is.na(med)) med <- 75L
    age[is.na(age)] <- as.integer(round(med))
  }

  n_meds_raw <- pick("n_medications", "num_medications", "med_count")
  meds_str <- pick("medications", "meds", "medication_list")
  if (is.null(meds_str)) meds_str <- rep("", n)
  meds_str <- as.character(meds_str)

  n_medications <- if (!is.null(n_meds_raw)) {
    suppressWarnings(as.integer(n_meds_raw))
  } else {
    # Derive count from the medication string when not given explicitly.
    vapply(meds_str, function(s) {
      s <- trimws(s)
      if (!nzchar(s)) {
        return(0L)
      }
      length(strsplit(s, "[;,|]")[[1]])
    }, integer(1), USE.NAMES = FALSE)
  }
  n_medications[is.na(n_medications)] <- 0L

  poly_raw <- pick("polypharmacy")
  polypharmacy <- if (is.null(poly_raw)) {
    as.integer(n_medications >= 5)
  } else {
    .cdt_as_flag(poly_raw)
  }

  out <- tibble::tibble(
    patient_id = pid,
    name = {
      nm <- pick("name", "patient_name")
      if (is.null(nm)) paste0("[SYNTHETIC] ", pid) else as.character(nm)
    },
    age = age,
    sex = .cdt_normalize_sex(if (is.null(pick("sex", "gender"))) rep("F", n) else pick("sex", "gender")),
    parkinsons = .cdt_as_flag(if (is.null(pick("parkinsons", "pd"))) rep(0, n) else pick("parkinsons", "pd")),
    osteoporosis = .cdt_as_flag(if (is.null(pick("osteoporosis"))) rep(0, n) else pick("osteoporosis")),
    orthostatic_hypotension = .cdt_as_flag(
      if (is.null(pick("orthostatic_hypotension", "orthostasis", "oh"))) rep(0, n) else pick("orthostatic_hypotension", "orthostasis", "oh")
    ),
    polypharmacy = polypharmacy,
    prior_falls = .cdt_as_flag(if (is.null(pick("prior_falls", "previous_falls", "fall_history"))) rep(0, n) else pick("prior_falls", "previous_falls", "fall_history")),
    n_medications = n_medications,
    medications = meds_str,
    comorbidities = {
      cm <- pick("comorbidities", "conditions", "diagnoses")
      if (is.null(cm)) rep("", n) else as.character(cm)
    }
  )

  # Guarantee column order/presence.
  out[, cdt_canonical_patient_cols()]
}

#' Validate a canonical patient tibble
#'
#' @param df A tibble expected to match the canonical schema.
#' @return Invisibly `TRUE`; errors with a message on the first problem found.
#' @export
cdt_validate_patients <- function(df) {
  need <- cdt_canonical_patient_cols()
  missing <- setdiff(need, names(df))
  if (length(missing) > 0) {
    stop("Missing canonical columns: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(df$patient_id)) {
    stop("Duplicate patient_id values found.", call. = FALSE)
  }
  if (any(df$age < 0 | df$age > 120, na.rm = TRUE)) {
    stop("Age values out of plausible range.", call. = FALSE)
  }
  invisible(TRUE)
}
