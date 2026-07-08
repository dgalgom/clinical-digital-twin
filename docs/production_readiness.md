# Production Readiness — Clinical Digital Twin

This document inventories the gaps between the current **hackathon MVP** and a
production deployment, and (in the second half) proposes concrete features that
close them. It is deliberately honest: the system runs on **synthetic data only**
and is **not for clinical use**. Several "obstacles" below are acceptable *because*
the data is synthetic, but every one of them would be a hard blocker before any
real-patient (PHI) deployment.

Code references use `file:line` against the repository at the time of writing.

---

## 1. Obstacles

### 1.1 Security & authentication
- **No login throttling, lockout, or password policy.** `cdt_login()` (`R/auth.R:64`)
  verifies a `sodium` scrypt hash but imposes no rate limit, attempt cap, or
  minimum-strength rule, so it is open to online brute force.
- **The Telegram webhook is completely unauthenticated.** `POST /telegram/webhook`
  (`api/plumber.R:158`) runs the bot for *any* caller who can reach the endpoint —
  there is no check of Telegram's `X-Telegram-Bot-Api-Secret-Token` header, so
  anyone can query the (synthetic) patient database through the bot. This is the
  single most conspicuous audit finding.
- **Session token accepted as a URL query parameter.** `.extract_token()`
  (`api/plumber.R:50`) reads `?token=`, which leaks into logs, browser history,
  and referrer headers. Bearer-header-only would be safer.
- **No TLS / CSRF handling.** The API and Shiny app serve plain HTTP by default and
  document TLS as an out-of-scope deployment concern (`R/auth.R:7-9`).

### 1.2 PHI & privacy
- **Identifiers are sent to a third-party LLM in plaintext.** `cdt_patient_context()`
  (`R/service.R:77-78`) embeds the patient `name` (and age/sex) into the prompt that
  `cdt_claude_reply()` forwards to the Anthropic API. On synthetic data this is
  harmless; on real data it is a PHI disclosure. (Feature A1 de-identifies this.)
- **No de-identification layer** between the datastore and the LLM/bot surfaces.
- **No role-based access control.** The `users` table carries a `role` column
  (`R/db.R`, `cdt_create_user(role=...)` `R/auth.R:40`) but nothing enforces it —
  any authenticated user sees the whole cohort.
- **Datastore is plaintext SQLite.** No at-rest encryption for the patient table.

### 1.3 Bot UX & capability gaps
- **Replies are text-only.** The Telegram bot (`cdt_bot_handle_message()`
  `R/bot.R:106`) returns a string; it cannot deliver the visualizations that make
  the dashboard useful (functional/fall history, trends, what-if comparison).
- **No query→visualization routing.** There is no component that classifies a free
  clinician query ("fall history of patient 6", "daily steps over time", "what if we
  stop drug X") into a chart intent + parameters, so the bot cannot decide *what* to
  plot.
- **What-if levers are coarse.** `cdt_bot_parse_whatif()` (`R/bot.R:63`) handles
  steps %, SBP delta, sedentary, and a blanket polypharmacy toggle, but has **no
  per-drug lever** — "remove medication X" cannot be expressed even though the model
  supports `n_medications`/`polypharmacy` overrides (`R/model.R:209-219`).
- **No menu/command scaffolding or dashboard hand-off.** Only `/start` is handled
  (`R/bot.R:109`); there is no `/help`, `/history`, `/whatif`, or deep link to the
  Shiny dashboard.

### 1.4 Reliability
- **No retry / circuit-breaker** around the Claude or Telegram HTTP calls
  (`R/claude_client.R:105`, `R/telegram_client.R:59`). A transient upstream error
  degrades to a static error string but there is no backoff or breaker.
- **Single shared DB connection is a SPOF.** The app and API each hold one
  process-wide connection (`app.R:31`, `api/plumber.R:31`); a broken connection has
  no reconnect path.
- **Model load is fatal.** `cdt_load_model()` (`R/model.R:311`, stop at
  `R/model.R:313`) stops the process if the `.rds` is missing rather than degrading
  gracefully.

### 1.5 Observability
- **No structured logging or metrics.** There is no request/latency/error logging
  suitable for aggregation; failures surface only as inline strings.
- **`/health` is static.** It always returns `status:"ok"` (`api/plumber.R:72`)
  without checking the DB or model, so it cannot detect a degraded process.

### 1.6 Scalability
- **SQLite is single-writer.** Fine for a demo cohort (n=100 × 90 days) but it
  serializes writes and will not scale to concurrent institutional load.
- **Shiny is single-threaded per process.** Concurrent clinicians contend for one R
  process; horizontal scaling needs a load balancer + session affinity.

### 1.7 Data validation
- **`/predict` accepts arbitrary JSON.** The body's `modified_inputs` is passed
  through (`api/plumber.R:142-147`) with no schema/range validation.
- **Silent neutral imputation.** Missing features fall back to hard-coded neutral
  medians (`R/features.R:38-46`, `R/features.R:89-99`) with no signal to the caller
  that a value was imputed rather than measured.
- **No sensor-recency guard.** The engineered window uses whatever days exist
  (`R/features.R:49`); a patient with stale data still yields a confident-looking
  score.

### 1.8 Model governance
- **No model versioning / lineage.** The fitted object stores `trained_at` and
  sizing (`R/model.R:136-142`) but there is no version id, dataset hash, or registry;
  `/health` and API responses don't expose which model is live.
- **No retraining / drift / calibration monitoring.** Nothing tracks input drift or
  recalibrates the risk tiers (`cdt_risk_cutoffs()` `R/config.R:59`), which are
  documented as an MVP heuristic, not a validated clinical threshold.

### 1.9 Deployment & testing
- **No container / CI / process manager.** There is a thorough `verify.R` and a
  `tests/` suite, but no Dockerfile, CI gate wrapping them, or supervisor for the
  API/app processes.
- **No load / soak testing.** Latency is checked once in the statistical checkpoint;
  there is no concurrency or endurance testing.

---

## 2. Proposed features

Each proposal maps to one or more obstacles above. The **"Infra"** column notes any
new infrastructure dependency; features marked *none* are pure code changes buildable
inside this repository with no new services and no new hard package dependency (base
`grDevices::png()` + the already-present `httr2` cover the bot images). "Effort" is a
rough relative sizing, not a schedule.

The **A-series** (A1–A5) are the features slated for implementation in this iteration
(they close the highest-signal audit findings — PHI leakage and the unauthenticated
bot — and deliver the requested clinician-facing bot capabilities). **B–G** are
smaller supporting hardening items proposed as documented, ready-to-build follow-ups.

| # | Feature | Obstacle(s) addressed | Infra | Effort |
|---|---|---|---|---|
| **A1** | **De-identify LLM prompts.** Drop the patient `name` from the context sent to Claude (`R/service.R:77-78`); keep `patient_id`, age, sex, coded 0/1 clinical flags, and derived risk. Removes the only PHI field crossing the API boundary. | 1.2 (identifiers to third-party LLM) | none | low / high impact |
| **A2** | **Bot PNG charts + `viz-query-router` subagent.** Classify a free-text clinician query into a chart *intent* (fall/functional history, steps/HR/SBP/sedentary over time, what-if) + parameters, render server-side to PNG (base `png()`), and deliver via Telegram `sendPhoto`. A deterministic in-code fallback classifier keeps the bot working offline; the subagent is the richer LLM-assisted path. | 1.3 (text-only replies; no query→viz routing) | none | med |
| **A3** | **Bot username gate + webhook secret-token.** Require the user to identify with a known username (looked up read-only in `users`) before any patient query; verify Telegram's `X-Telegram-Bot-Api-Secret-Token` header against `TELEGRAM_WEBHOOK_SECRET` when that env var is set. No passwords over Telegram (synthetic data, open-source-auditable). | 1.1 (unauthenticated webhook) | none | low-med |
| **A4** | **Named-drug what-if lever.** Parse "remove/stop/deprescribe *drug X*"; if X is in the patient's `medications`, simulate `n_medications - 1` and recompute `polypharmacy` via the existing `cdt_apply_overrides` path (`R/model.R:209-219`). If the drug is not on the list, say so rather than fabricate an effect. | 1.3 (no per-drug lever) | none | low |
| **A5** | **Telegram menu commands + dashboard deep-link.** Handle `/help`, `/history`, `/whatif`, and `/dashboard`; `/dashboard` returns a deep link to the Shiny app (`CDT_APP_URL`) as a chat menu option. Provide a `cdt_bot_commands()` list suitable for `setMyCommands` (registration is a deploy step). | 1.3 (no menu/dashboard hand-off) | none | low |
| B | **Login throttling / lockout / password policy.** Per-username attempt counter + backoff/lockout and a minimum-strength rule in `cdt_login()` (`R/auth.R:64`). | 1.1 (online brute force) | none | low-med |
| C | **`/predict` input validation.** Schema + range check `modified_inputs` before it reaches the model (`api/plumber.R:142-147`) rather than passing arbitrary JSON through. | 1.7 (arbitrary JSON) | none | low |
| D | **Structured logging (no PHI/secrets).** Emit request/latency/error records to a machine-parseable stream, redacting names/keys, for aggregation. | 1.5 (no logging/metrics) | none | med |
| E | **Model version metadata in responses.** Add a version id / dataset hash to the fitted object (`R/model.R:136-142`) and expose which model is live in API responses. | 1.8 (no versioning/lineage) | none | low |
| F | **Real `/health`.** Have `/health` (`api/plumber.R:72`) actually probe the DB and model rather than always returning `status:"ok"`, so a degraded process is detectable. | 1.5 (static health) | none | low |
| G | **Header-only auth (deprecate `?token=`).** Stop accepting the session token as a URL query parameter (`api/plumber.R:50`); require the `Authorization: Bearer` header only. | 1.1 (token leaks into logs/history) | none | low (breaking) |

### Deferred (infrastructure-dependent)

These close real obstacles but require services, ops, or data-governance work beyond a
code change in this repo, so they are tracked in `docs/next_steps.md` rather than built
here:

- **Containerization + CI gate** wrapping `verify.R` / `tests/`, and a process manager
  for the API/app (obstacle 1.9).
- **TLS termination** and CSRF handling for the API + Shiny app (obstacle 1.1).
- **At-rest encryption / Postgres migration** — SQLite is single-writer and plaintext
  (obstacles 1.2, 1.6).
- **Retry / circuit-breaker** around the Claude and Telegram HTTP calls (obstacle 1.4).
- **Connection pooling / reconnect** to remove the single-connection SPOF (obstacle 1.4).
- **Retraining + drift / calibration monitoring** and validated (not heuristic) risk
  cutoffs (obstacle 1.8).
- **Role-based access control** enforcing the existing `role` column (obstacle 1.2).
- **Load / soak testing** for concurrency and endurance (obstacle 1.9).

---

*Implementation of the A-series features (A1–A5) proceeds in the next step, one
sub-feature at a time, each independently verified.*
