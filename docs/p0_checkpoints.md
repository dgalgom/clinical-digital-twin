# P0 Clinical-Workflow Checkpoints — for review BEFORE any code changes

> Scope: **P0 only** (the 4 highest-impact, demo-visible items from
> `improvement_plan_clinical_digital_twin.md` §3). Target: hackathon submission.
> This document is a **decision gate**: nothing in `R/`, `app.R`, `api/`, the DB
> schema, or the data generator will change until you approve each item below.
>
> Each checkpoint states: **Why it matters**, **Is it feasible** (grounded in the
> actual current code, with exact anchor points), **What changes** (files + new
> tables), **Risk / blast radius**, **Verification**, and a **Decision** line for
> you to mark.
>
> Ideas folded in from the sibling `human-digital-twin` Python project
> (`src/alarms/detector.py`, `src/agents/schemas.py`) are flagged with **[HDT]**.

---

## Grounding — what the code looks like today (verified, not assumed)

| Thing | Current reality (file:anchor) |
|---|---|
| Service layer the front-ends consume | `R/service.R` — `cdt_cohort_snapshot()`, `cdt_patient_risk()`, `cdt_patient_context()` |
| DB schema + helpers | `R/db.R` — `cdt_db_init_schema()` (5 tables: users, sessions, patients, sensor_readings, fall_events), `cdt_db_write()`, `cdt_get_*()` |
| Config / constants | `R/config.R` — `cdt_risk_cutoffs()` (`moderate=0.15, high=0.35`), `cdt_risk_tier()`, feature lists |
| Model + drivers | `R/model.R` — `predict_fall_risk()`, `cdt_feature_importance()` (cohort-level standardized coefficients) |
| Dashboard tabs | `app.R` — `tabsetPanel(id="tabs", ...)`: *Cohort overview*, *Patient detail*, *What-if simulator*, *Patient data* |
| What-if overrides | `R/model.R::cdt_apply_overrides()` + `app.R` sliders `wi_steps/wi_sbp/wi_sed/wi_meds` |
| Bot dispatcher | `R/bot.R::cdt_bot_reply()` returns `list(text=, photo=)`; slash commands parsed by `.cdt_parse_command()` |
| Bot command menu | `R/bot.R::cdt_bot_commands()` — **already lists** `triage`, `drivers`, `dashboard` |
| Existing `/triage` | `R/bot.R:571` — returns top-N patients by **absolute** 7d risk (a static worklist, **not** change-detection) |
| Fall events | `R/db.R` `fall_events(event_id, patient_id, ts, severity)` — used **only as model labels**; generated in `R/synthetic_sensors.R` |
| Feature importance in UI | `app.R::output$plot_importance` + bot `/drivers` (`R/bot.R:621`) |
| Tests | `tests/testthat/` (8 files, 222 passing) + `verify.R` (7-step gate) + `checkpoints/evaluate_model.R` |

**Architecture rule (must hold):** all logic lives in `R/` modules; `app.R`,
`api/plumber.R`, `R/bot.R` stay thin and call the service layer; everything must
keep passing `verify.R`, `tests/testthat`, and `checkpoints/evaluate_model.R`;
the system must run offline in mock mode with SQLite and no API keys.

**Key finding that de-risks P0:** none of the 4 P0 items touch the *model*. They
add *workflow tables and views around* the existing predictions. So
`checkpoints/evaluate_model.R` (the statistical gate) is **unaffected** — its
metrics cannot regress because the model, features, and DGP are untouched.
(One exception: CP-1's synthetic "previous snapshot" seeding is a data-only
convenience; it does not alter the training table or model.)

---

## CP-1 — Shift-triage view + risk-delta alerts  *(plan §3 P0-1)*

**Why it matters.** The plan's central clinical thesis (§1): the bottleneck in
nursing homes is *not* prediction — it is answering *"who changed since
yesterday/last shift, and why?"* The current `/triage` and Cohort tab both sort
by **absolute** risk, so the same frail patients top the list every day and staff
tune them out (alarm fatigue, §1.7). A **delta** view surfaces movement. **[HDT]**
`AlarmDetector.detect()` proves the exact pattern: compare today's value to the
trailing 7-day mean, fire on a threshold factor + direction, tag a severity,
group by patient. I will port that structure to R.

**Is it feasible?** Yes.
- Delta needs a "previous" snapshot to compare against. `cdt_cohort_snapshot()`
  already computes today's risk per patient. I add a persisted history table so
  "previous" is well-defined and deterministic in the demo.
- Alert reason strings reuse data already computed: `steps_trend_7d`,
  `resting_hr_trend_7d`, etc. from `R/features.R` (no new math).
- Threshold config slots into `R/config.R` alongside `cdt_risk_cutoffs()`.
- A new landing tab is a one-line addition to the existing `tabsetPanel`.
- `/triage` already exists — I upgrade its body (and keep the absolute-worklist
  as `/triage all` for continuity).

**What changes (proposed).**
| File | Change |
|---|---|
| `R/db.R` | New tables: `risk_snapshots(snapshot_id, patient_id, as_of, p_24h, p_7d, tier_7d)` and `alerts(alert_id, patient_id, created_at, kind, severity, delta_pts, reason_text, acknowledged_by, acknowledged_at)`. Add `cdt_write_risk_snapshot()`, `cdt_get_last_snapshot()`, `cdt_insert_alert()`, `cdt_ack_alert()`. |
| `R/config.R` | `cdt_alert_config()` — delta threshold (default **+8 pts** 7d), tier-crossing trigger, severity mapping. **[HDT]**-style (direction + threshold + severity + label). |
| `R/service.R` (new file `R/alerts.R`) | `cdt_compute_alerts(con, model)`: snapshot now, diff vs last stored snapshot, emit alert rows with a one-line reason citing the top-moving feature(s). |
| `app.R` | New **first** tab "Shift triage": alerts sorted by severity, one-line reason, **Acknowledge** button; keep Cohort overview as tab 2. |
| `R/bot.R` | Upgrade `/triage` → risk-delta worklist; `/triage all` → current absolute list. |
| `data-raw/generate_synthetic_data.R` | Seed a "yesterday" `risk_snapshots` row so deltas are visible on first launch (data-only; deterministic under the existing seed). |
| `tests/testthat/test-alerts.R` (new) | Alert firing on +Δ and on tier crossing; ack path; no-alert when stable. |
| `verify.R` | Add a step exercising `cdt_compute_alerts()` on the demo DB. |

**Risk / blast radius.** Low–medium. New tables are additive (schema is
`CREATE TABLE IF NOT EXISTS`, idempotent). The only behavior change to existing
surfaces is the `/triage` body and a new default tab — both reversible. No model
impact. Main judgement call: **what counts as "the previous shift"** in a demo
that has one daily reading — I propose "previous stored snapshot", seeded once,
and re-snapshotted on demand. *Open question A below.*

**Verification.** `verify.R` PASS; new testthat file PASS; 222 existing tests
still PASS; `checkpoints/evaluate_model.R` unchanged (no model touch).

**Decision:** ⬜ Approve as-is ⬜ Approve with changes ⬜ Defer ⬜ Reject

---

## CP-2 — Driver → intervention mapping  *(plan §3 P0-2)*

**Why it matters.** A probability alone doesn't change care (§1.2). Each risk
driver should map to 2–3 concrete, evidence-based interventions (declining steps
→ PT/mobility referral; orthostatic hypotension → BP-med review + slow-transfer;
polypharmacy → structured medication review; nocturnal restlessness → toileting
schedule/bed-exit precautions). Pure content work, high perceived-usefulness.
**[HDT]** `RecommendedAction` (schemas.py) shows the shape: risk-level/driver →
`actions[]` + `urgency`, **loaded from config, never hardcoded**.

**Is it feasible?** Yes — this is the lowest-risk item. `cdt_feature_importance()`
already yields the driver names shown in `app.R::plot_importance` and bot
`/drivers`. I add a static lookup keyed on those exact feature names and render
the mapped suggestions beside the existing importance output.

**What changes (proposed).**
| File | Change |
|---|---|
| `inst/extdata/interventions.yaml` (or `R/interventions_map.R`) | Curated map: `feature → list(interventions, urgency, evidence_note)`. Keys are the canonical names from `cdt_static_features()` + `cdt_sensor_features()`. |
| `R/service.R` | `cdt_driver_interventions(model, patient_id, top_n=3)` — join top drivers to the map. |
| `app.R` | In *Patient detail*, render an "Suggested interventions" panel next to `plot_importance`. |
| `R/bot.R` | Include mapped suggestions in `/drivers` (and grounded replies). |
| `tests/testthat/test-interventions.R` (new) | Every model feature has ≥1 mapped intervention; lookup returns expected rows. |

**Risk / blast radius.** Very low. Additive, content-only; no schema, no model,
no DGP. Worst case is wording quality (mitigated with an "illustrative, not
clinical guidance" caption, consistent with existing synthetic-data banners).

**Verification.** New test asserts full coverage of feature keys; `verify.R` PASS.

**Decision:** ⬜ Approve as-is ⬜ Approve with changes ⬜ Defer ⬜ Reject

---

## CP-3 — Interventions log + closed loop  *(plan §3 P0-3)*

**Why it matters.** Makes the what-if panel *lead somewhere*: simulate →
convince → prescribe → **log** → track. Overlaying logged interventions on the
risk-trend chart answers the "did it work?" question visually. This is the
narrative spine of the plan's demo arc (§5 Demo).

**Is it feasible?** Yes. Writing rows uses the existing `cdt_db_write()`. The
overlay uses the existing plotly time-series in *Patient detail* (`plot_steps`
etc.) — I add `add_markers`/vertical lines at intervention dates. The what-if
panel already computes a counterfactual (`whatif_result()`), so a "log this
counterfactual as a planned intervention" button just persists that override set.

**What changes (proposed).**
| File | Change |
|---|---|
| `R/db.R` | New table `interventions(intervention_id, patient_id, type, detail, created_by, created_at)` + `cdt_log_intervention()`, `cdt_get_interventions()`. |
| `app.R` | *Patient detail*: form to log an intervention; *What-if*: "Log this scenario as a planned intervention" button; overlay intervention markers on the trend plot. |
| `R/bot.R` | (Optional) `/log` command — deferred unless you want it. |
| `tests/testthat/test-interventions-log.R` (new) | Insert/read round-trip; overlay data assembly. |

**Risk / blast radius.** Low. Additive table; UI-only wiring. Writes are
clinician-initiated (button), never automatic. No model/DGP impact.
*Open question B: should logging be gated by role (e.g. clinician only)?*

**Verification.** Round-trip test PASS; `verify.R` PASS.

**Decision:** ⬜ Approve as-is ⬜ Approve with changes ⬜ Defer ⬜ Reject

---

## CP-4 — Post-fall huddle module  *(plan §3 P0-4)*  ⭐ flagship Claude moment

**Why it matters.** Best practice after every fall is a structured post-fall
huddle (§1.5). Today `fall_events` is used **only as model labels** — the single
biggest missing clinical workflow. This is also the plan's flagship
Claude-integration demo: Claude drafts the huddle narrative grounded in the 72h
of pre-fall sensor data ("night activity rose 40% in the 3 days before…").

**Is it feasible?** Yes, and it reuses proven machinery:
- `fall_events` extension is additive columns (idempotent schema).
- The grounded-context pattern already exists: `cdt_patient_context()`
  (`R/service.R:64`) injects real synthetic facts into an LLM prompt with a
  "don't invent details" instruction. `cdt_draft_huddle_summary()` follows the
  same recipe, windowing the 72h before the fall via `cdt_get_patient_timeline()`.
- Mock fallback is already the norm (`cdt_llm_is_mock()`), so the demo works
  offline with a deterministic template.
- Critically: **the LLM output never writes to the DB directly** — it drafts;
  the clinician reviews/edits before saving. (Consistent with the safety posture
  already in the codebase and **[HDT]**'s "raw_response stored for audit" idea.)

**What changes (proposed).**
| File | Change |
|---|---|
| `R/db.R` | Extend `fall_events` with `location, activity_at_fall, injury_level, contributing_factors, plan, huddle_completed_by, huddle_completed_at`. Add `cdt_complete_huddle()`, `cdt_get_open_huddles()`. |
| `R/service.R` (or new `R/huddle.R`) | `cdt_draft_huddle_summary(con, model, event_id)` — grounds a Claude prompt on 72h pre-fall context; deterministic template in mock mode. |
| `app.R` | Shiny modal to complete a huddle for an un-huddled fall (draft → clinician edits → save). |
| `R/bot.R` | (Optional) `/huddle` flow — deferred unless you want it. |
| `data-raw/generate_synthetic_data.R` | Leave a couple of recent falls **un-huddled** so the demo has something to action. |
| `tests/testthat/test-huddle.R` (new) | Field round-trip; mock draft is deterministic and grounded (mentions the windowed metrics). |

**Risk / blast radius.** Medium (largest of the four). Altering `fall_events`
needs care: it's read by the training-table builder (`cdt_build_training_table()`
reads `patient_id, ts`). **Adding** columns is safe because that builder selects
only the columns it needs; I will verify the model rebuild still produces
identical labels (the new columns are ignored by training). No change to
`severity` or `ts`, so `checkpoints/evaluate_model.R` is unaffected.
*Open question C: live-Claude drafting requires `ANTHROPIC_API_KEY`; the demo
default stays mock unless you enable it.*

**Verification.** Rebuild model, confirm `checkpoints/evaluate_model.R` metrics
**identical** before/after (they must be — training ignores new columns); huddle
round-trip + mock-draft tests PASS; `verify.R` PASS.

**Decision:** ⬜ Approve as-is ⬜ Approve with changes ⬜ Defer ⬜ Reject

---

## Cross-cutting: verification contract applied to EVERY approved item

Before I mark any item done, all of these must hold (same discipline used for
the deploy fixes):
1. `Rscript verify.R` → PASS (I'll add a step per new subsystem).
2. `tests/run_tests.R` (testthat) → all previously-passing tests still pass +
   new tests pass.
3. `Rscript checkpoints/evaluate_model.R` → metrics **unchanged** for CP-1/2/3
   (no model touch) and **identical** for CP-4 (training ignores new columns).
4. Runs offline in mock mode, SQLite, no keys.
5. No secrets/artifacts staged; local commit only (you push), matching the
   established workflow.

---

## Suggested build order (if you approve multiple)

`CP-2` (safest, content) → `CP-3` (small additive table + UI) →
`CP-1` (alerts, the workflow centerpiece) → `CP-4` (flagship, largest surface).
Each is independently shippable and independently revertible.

---

## Open questions for you (please answer alongside your approvals)

- **A (CP-1 semantics):** In a one-reading-per-day demo, what is "the previous
  shift" for delta computation? Proposed: compare against the **last stored
  `risk_snapshots` row**, seeded once at build so day-1 shows movement. OK?
- **B (CP-3 gating):** Should logging an intervention be restricted by user role,
  or is any signed-in clinician fine for the MVP?
- **C (CP-4 LLM):** Keep huddle drafting **mock by default** (works offline) and
  only call live Claude when `ANTHROPIC_API_KEY` is set — confirm?
- **D (bot scope):** For CP-3/CP-4, do you want the optional bot commands
  (`/log`, `/huddle`) now, or dashboard-only for P0 and bot later?
- **E (order):** Accept the suggested build order, or reprioritize?
