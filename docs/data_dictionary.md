# Data Dictionary — Clinical Digital Twin

Authoritative reference for the two input datasets the system consumes: the
**patient (demographics/clinical) table** and the **sensor readings** time
series. This is the shared source of truth for humans and for the
`data-format-reviewer` subagent. Field definitions mirror the code in
`R/ingest.R`, `R/config.R`, `R/db.R`, and `R/features.R`.

> Scope: this project runs on **synthetic data only**. In any real deployment,
> patient data is PHI and must be handled under the appropriate governance
> (see `docs/production_readiness.md`).

---

## 1. Patient table (canonical schema)

Produced by `cdt_ingest_patient_csv()` and validated by `cdt_validate_patients()`.
Column-name matching is **case-insensitive** and tolerant of the aliases below.
After ingestion, all canonical columns are guaranteed present and in this order
(`cdt_canonical_patient_cols()`).

| Canonical column | Required | Type | Accepted aliases (case-insensitive) | Notes / ingestion behavior |
|---|---|---|---|---|
| `patient_id` | Recommended | string | `id`, `pid` | Must be unique. If the column is absent, IDs are auto-generated as `P001`, `P002`, … |
| `name` | No | string | `patient_name` | Defaults to `[SYNTHETIC] <patient_id>` if absent. Display only. |
| `age` | Yes* | integer | — | Validated to `[0, 120]`. Missing values imputed to the **cohort median** (or **75** if the whole column is missing). |
| `sex` | Yes* | `"F"` / `"M"` | `gender` | Normalized: `M`/`MALE`/`1` → `M`; `F`/`FEMALE`/`0`/`2` → `F`. Missing/unrecognized → `F`. |
| `parkinsons` | No | 0/1 | `pd` | Flag coercion (see below). Default `0`. |
| `osteoporosis` | No | 0/1 | — | Flag coercion. Default `0`. |
| `orthostatic_hypotension` | No | 0/1 | `orthostasis`, `oh` | Flag coercion. Default `0`. |
| `polypharmacy` | No | 0/1 | — | If the **column is absent**, **derived**: `1` when `n_medications >= 5`, else `0`. If present, flag coercion. |
| `prior_falls` | No | 0/1 | `previous_falls`, `fall_history` | Flag coercion. Default `0`. |
| `n_medications` | No | integer | `num_medications`, `med_count` | If the **column is absent**, **derived** by counting entries in `medications` split on `;` `,` or `\|`. Missing → `0`. |
| `medications` | No | string | `meds`, `medication_list` | Free text; entries separated by `;`, `,`, or `\|`. Default `""`. |
| `comorbidities` | No | string | `conditions`, `diagnoses` | Free text. Default `""`. Not a model feature. |

> **Derivation caveat:** `polypharmacy` and `n_medications` are auto-derived only
> when their **column is absent** from the input. An all-blank *column* is not the
> same as an absent one — if you include an empty `n_medications` column, the
> blanks are read as `0`/`NA` and the derivation is skipped. When you don't have
> these values, omit the columns entirely and keep `medications`.

\* `age` and `sex` are "required" in the sense that the model needs them; the
ingestion layer will still fill defaults (median/75 age, `F` sex) rather than
error. Prefer supplying real values.

### Flag coercion (`.cdt_as_flag`)
Truthy → `1`: `1`, `yes`, `y`, `true`, `t`, or logical `TRUE` (case-insensitive).
Everything else → `0`.

### Validation errors (`cdt_validate_patients`)
Hard-stops that reject the dataset:
1. **Missing canonical columns** after ingestion.
2. **Duplicate `patient_id`** values.
3. Any **`age` outside `[0, 120]`**.

### Static model features derived from the patient table
`cdt_static_features()`: `age`, `sex_male` (1 if `sex == "M"`), `parkinsons`,
`osteoporosis`, `orthostatic_hypotension`, `polypharmacy`, `prior_falls`,
`n_medications`.

---

## 2. Sensor readings table

One row per patient per day. Schema in `R/db.R` (`sensor_readings`).
Individual daily values **may be `NA`** to represent device non-wear.

| Column | Required | Type | Units | Notes |
|---|---|---|---|---|
| `patient_id` | Yes | string | — | Must match a `patient_id` in the patient table. |
| `ts` | Yes | string | ISO-8601 | Daily read-out at **06:00 Europe/Berlin** with explicit DST offset, e.g. `2026-01-01T06:00:00+0100` (CET) / `2026-07-01T06:00:00+0200` (CEST). **No automatic format conversion** — Excel-serial or Unix timestamps must be reformatted first. |
| `heart_rate` | Yes | real | bpm | Mean HR; feeds `hr_variability_7d` (SD over the window). |
| `resting_hr` | Yes | real | bpm | Resting HR. Feeds mean/trend features. |
| `sbp` | Yes | real | mmHg | Systolic BP. Feeds `sbp_mean_7d`. |
| `dbp` | No | real | mmHg | Diastolic BP. Stored for completeness; not a model feature. |
| `step_count` | Yes | integer | steps/day | Feeds `steps_mean_7d` / `steps_trend_7d`. |
| `accel_counts` | No | integer | counts | Stored; **excluded from the model** (~0.8 collinear with steps). |
| `accel_magnitude` | Yes | real | g | Mean vector magnitude. Feeds `accel_magnitude_mean_7d`. |
| `hours_sitting` | Yes | real | hours | Part of `sedentary = sitting + lying`. |
| `hours_lying` | Yes | real | hours | Part of `sedentary`. |
| `hours_standing` | Yes | real | hours | Sitting + lying + standing should sum to ≈ 24 (advisory, not enforced). |

### Engineered sensor features (`cdt_engineer_sensor_features`, 7-day window)
Means and trends over the most recent `window_days` (default 7):
`steps_mean_7d`, `steps_trend_7d`, `resting_hr_mean_7d`, `resting_hr_trend_7d`,
`sbp_mean_7d`, `sedentary_hours_mean_7d`, `sedentary_hours_trend_7d`,
`hr_variability_7d`, `accel_magnitude_mean_7d` (plus `accel_counts_*` computed
but not used). Trends need ≥ 3 non-missing points or return `0`. An all-`NA`
window falls back to neutral defaults (e.g. `steps_mean_7d = 3000`).

**Recency:** at least **7 recent daily readings** per patient are recommended so
the rolling features are well-defined; fewer days use whatever is available.

---

## 3. From CSV to prediction

```r
# Load library source (no install needed)
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

# Patient table
df <- cdt_ingest_patient_csv("path/to/normalized_patients.csv")
cdt_validate_patients(df)                      # errors on schema / dup / age

con <- cdt_db_connect(); cdt_db_init_schema(con)
cdt_db_write(con, "patients", df, append = TRUE)
# Sensors (already in canonical column names):
# cdt_db_write(con, "sensor_readings", sensor_df, append = TRUE)

model <- cdt_load_model()
cdt_patient_risk(con, model, df$patient_id[1], include_baseline = TRUE)

# Counterfactual / what-if:
cdt_patient_risk(con, model, df$patient_id[1],
                 modified_inputs = list(steps_pct = 30), include_baseline = TRUE)
```

Supported `modified_inputs` levers include relative `steps_pct`, absolute deltas
like `sbp_delta`, and absolute overrides of engineered features
(`steps_mean_7d`, `sedentary_hours_mean_7d`, `polypharmacy`, …). See
`cdt_apply_overrides()` in `R/model.R`.
