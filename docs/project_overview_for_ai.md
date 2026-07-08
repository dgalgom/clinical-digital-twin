# Clinical Digital Twin Monitoring System — Project Context

> **Purpose of this file.** A self-contained primer you can paste into (or upload
> to) an AI chat so it understands what this project is, how it's structured, and
> what each component does — without needing the whole repo. Everything here
> describes an R codebase. **All patient data is SYNTHETIC; this is a hackathon
> MVP, not for clinical use.**

---

## 1. What the project is

An end-to-end **R** system that lets clinicians visualize (synthetic) patient
vitals/activity data and run **"what-if" simulations on statistical digital
twins** to estimate **fall risk** over the next **24 hours** and **7 days**.

Three user-facing surfaces sit on one shared core library:
1. **Shiny + plotly dashboard** — login → cohort overview by risk tier → patient
   detail (time series, risk cards, model drivers) → interactive what-if panel →
   a "Patient data" table (searchable/sortable/exportable cohort view).
2. **plumber REST API** — auth, cohort/patient queries, prediction, Telegram webhook.
3. **Telegram bot** — clinicians ask natural-language questions (*"How is patient
   P042 trending?"*, *"What if we increase P042's mobility by 25%?"*). The bot
   grounds a prompt with real (synthetic) data + model outputs and calls **Claude**.

**License:** MIT. **R version:** ≥ 4.2. **Language:** R (one small Python MCP helper).

---

## 2. The digital twin model (core idea)

- A **single**, interpretable, ridge-penalized **logistic regression** predicting
  `P(fall)`. It uses a **pooled discrete-time (pooled-logistic)** design: the
  prediction horizon is itself a feature (`horizon_7d`). Scoring with the
  indicator at 0 → 24h risk; at 1 → 7-day risk. **One fitted object serves both
  horizons.**
- **Inputs:** engineered wearable-sensor features (rolling means/trends of steps,
  resting HR, sedentary time, plus accelerometry mean vector magnitude) and
  static risk factors (age, Parkinson's, osteoporosis, orthostatic hypotension,
  polypharmacy, prior falls).
- **What-if / counterfactuals:** `predict_fall_risk(model, feature_row,
  modified_inputs)` returns baseline risk when `modified_inputs = NULL`, or
  counterfactual "twin" risk given overrides like `steps_pct` (relative %),
  `sbp_delta` (absolute mmHg), or absolute overrides for
  `steps_mean_7d`/`resting_hr_mean_7d`/`sedentary_hours_mean_7d`/`polypharmacy`.
- **Interpretability:** standardized-predictor coefficients *are* the explanation;
  `cdt_feature_importance()` ranks drivers. Latency ~0.1 ms/prediction.
- **Modeling caveat (by design):** in the synthetic data-generating process,
  frailty acts *only through* the sensor streams it shifts, so static-factor
  coefficient **signs are not identifiable** and shouldn't be read clinically.
  Only activity/vitals signs are asserted in the checkpoint.

---

## 3. Repository structure

```
clinical-digital-twin/
├── R/                          # core library (package-style modules)
│   ├── config.R                # paths, schema constants, risk tiers
│   ├── db.R                    # SQLite schema + indexed queries (DBI/RSQLite)
│   ├── auth.R                  # sodium password hashing + session tokens
│   ├── env.R                   # loads git-ignored .env/.Renviron secrets
│   ├── ingest.R                # CSV -> canonical clinical schema
│   ├── synthetic_cohort.R      # synthetic patient generator (seeded)
│   ├── synthetic_sensors.R     # wearable streams: BP/HR/accelerometry @06:00 CET
│   ├── features.R              # feature engineering (shared train + inference)
│   ├── model.R                 # the digital twin: fit + predict_fall_risk()
│   ├── service.R               # combines db + model for all front-ends
│   ├── claude_client.R         # Claude API (httr2) + deterministic mock mode
│   ├── telegram_client.R       # Telegram API (httr2) + mock mode
│   ├── bot.R                   # bot dispatcher (cdt_bot_reply) + prompt grounding
│   ├── bot_router.R            # intent routing (LLM-assisted, with mock)
│   ├── bot_viz.R               # bot chart rendering (PNG replies)
│   └── bot_dates.R             # relative-date parsing for the bot
├── app.R                       # Shiny + plotly dashboard (bslib "darkly" theme)
├── api/
│   ├── plumber.R               # REST API + POST /telegram/webhook
│   ├── run_api.R               # API launcher (reads CDT_API_HOST/PORT)
│   └── run_bot_poll.R          # LOCAL bot via long-polling (no public URL/webhook)
├── data-raw/
│   ├── generate_synthetic_data.R        # reproducibly builds DB + trains model
│   └── example_institution_patients.csv # demo ingestion input (generated)
├── checkpoints/
│   └── evaluate_model.R        # statistical-adequacy checkpoint (AUC/Brier/...)
├── verify.R                    # one-command end-to-end verification (no keys)
├── tests/
│   ├── run_tests.R, testthat.R, integration_check.R
│   └── testthat/               # unit tests: model, preprocessing, auth, bot(*)
├── deploy/
│   ├── deploy_shinyapps.R      # deploy dashboard to shinyapps.io (P3M binaries)
│   ├── Dockerfile              # container for the bot/API (e.g. Render)
│   └── render.yaml             # Render blueprint (Docker web service)
├── docs/
│   ├── deployment.md           # shinyapps.io + local bot + Render + webhook guide
│   ├── data_dictionary.md      # column-level schema reference
│   ├── production_readiness.md # known gaps / hardening roadmap
│   └── project_overview_for_ai.md  # THIS FILE
├── inst/mcp/date_server.py     # tiny MCP "date" server (reproducible "today")
├── data/                       # generated SQLite + model .rds (GIT-IGNORED)
├── renv.lock                   # pinned deps (repo -> P3M for binary installs)
├── setup.R                     # install deps + build dataset/model
├── DESCRIPTION / LICENSE / README.md / .Renviron.example
```

Front-ends are thin; all logic lives in `R/` and is shared via `service.R`.

---

## 4. Data model (SQLite)

Five tables (see `R/db.R`):

| Table | Role |
|-------|------|
| `patients` | static cohort: id, name, age, sex, parkinsons, osteoporosis, orthostatic_hypotension, polypharmacy, prior_falls, n_medications, medications, comorbidities |
| `sensor_readings` | daily wearable read-outs (steps, resting HR, systolic BP, sedentary hours, accelerometry counts + mean vector magnitude) at 06:00 Europe/Berlin |
| `fall_events` | simulated fall labels/timestamps |
| `users` | clinician accounts (sodium-hashed passwords) |
| `sessions` | random-hex session tokens with a fixed TTL |

The SQLite DB (`data/clinical_twin.sqlite`) and trained model
(`data/fall_risk_model.rds`) are **git-ignored** and rebuilt reproducibly (seeded)
by `data-raw/generate_synthetic_data.R`.

---

## 5. Key functions to know

- `cdt_db_connect()` / `cdt_db_init_schema()` — open DB / create tables.
- `cdt_load_model()` — load the persisted twin.
- `predict_fall_risk(model, feature_row, modified_inputs=NULL)` — the twin.
- `cdt_cohort_snapshot(con, model)` — full cohort joined with `p_24h`, `p_7d`,
  `tier_24h`, `tier_7d` (sorted desc by 7-day risk). Powers the dashboard + bot.
- `cdt_feature_importance()` — ranked coefficient drivers.
- `cdt_bot_reply(con, model, chat_id, text, llm_mock=NULL)` — **the bot's core
  dispatcher**; returns `list(text=, photo=)`. Handles commands, an auth gate,
  patient queries, what-if, and viz. Both the webhook and the polling runner call it.
- `cdt_bot_commands()` — the Telegram command menu (start, help, risk, history,
  whatif, triage, drivers, explain, dashboard).
- `cdt_load_env()` — loads secrets from a git-ignored `.env`/`.Renviron`
  (precedence: shell/CI > .env > .Renviron; empty placeholders skipped; values
  never printed).

---

## 6. Secrets & modes (important for understanding behavior)

Secrets are read **only** from the environment; nothing is hardcoded. Copy
`.Renviron.example` → `.env` (or `.Renviron`), both git-ignored.

| Variable | Effect if SET | Effect if UNSET |
|----------|---------------|-----------------|
| `ANTHROPIC_API_KEY` | Live Claude replies | Bot uses deterministic **mock** replies |
| `TELEGRAM_BOT_TOKEN` | Live Telegram sends / polling | Sends captured in-memory (mock) |
| `TELEGRAM_WEBHOOK_SECRET` | Webhook rejects mismatched header (401) | Webhook check disabled |
| `CDT_APP_URL` | Bot `/dashboard` deep-link base | Falls back to `http://127.0.0.1:3838` |
| `CDT_MOCK_LLM=1` / `CDT_MOCK_TELEGRAM=1` | Force mock even with keys | — |

**Mock mode is a first-class feature:** the whole system runs offline with no
keys, which is how the tests and `verify.R` exercise the bot.

---

## 7. How to run (from project root)

```bash
Rscript setup.R          # install deps + build synthetic DB + train model (once)

# Dashboard (demo login: clinician / demo1234)
Rscript -e "shiny::runApp('app.R', port=3838, launch.browser=TRUE)"

# REST API + Telegram webhook
Rscript api/run_api.R    # http://127.0.0.1:8000

# Telegram bot locally, no public URL (long-polling; needs TELEGRAM_BOT_TOKEN):
Rscript api/run_bot_poll.R

# Verification / tests (no keys needed)
Rscript verify.R
Rscript tests/run_tests.R
Rscript checkpoints/evaluate_model.R
```

**Bot delivery models (mutually exclusive):**
- **Long-polling** (`api/run_bot_poll.R`) — pulls updates over an outbound HTTPS
  connection; no public URL/hosting. Runs while the process is up.
- **Webhook** (`POST /telegram/webhook` in `plumber.R`) — needs a public HTTPS
  URL + `setWebhook`; suited to always-on hosting (e.g. Render via `deploy/`).

---

## 8. Deployment (current state)

- **Dashboard → shinyapps.io.** `deploy/deploy_shinyapps.R` bundles `app.R` +
  `R/*.R` + generated `data/` artifacts. `renv.lock` pins the full 84-package
  dependency closure with its CRAN repo pointed at **Posit Public Package Manager
  (P3M)** so packages (notably `RSQLite`) install as **precompiled Linux
  binaries** instead of compiling from source. Live at
  `https://dgalgom.shinyapps.io/clinical-digital-twin/`.
- **Bot/API → optional Render** (`deploy/Dockerfile` + `deploy/render.yaml`) for
  an always-on webhook, or just run the local polling runner.

---

## 9. Known limitations (be explicit when reasoning about it)

- **Synthetic data only** — illustrative distributions, simulated fall labels; not
  epidemiologically calibrated or validated against real outcomes.
- **MVP auth, not production security** — random-hex tokens with fixed TTL; no
  CSRF, rate limiting, lockout, password policy, or TLS enforcement in-code.
  Passwords hashed with `sodium` (scrypt).
- **SQLite** for the hackathon (portable DBI layer; Postgres swap documented).
- **Daily-resolution sensors** (one 06:00 CET read-out/day).
- **Collinear features excluded by design** (accelerometry counts vs step
  summaries) — only the near-orthogonal magnitude enters the model.
- **The bot is decision *support*, grounded on injected data** — it doesn't invent
  clinical facts, but output should still be reviewed.

---

## 10. Typical questions this file should let an AI answer

- "Where is fall risk computed?" → `R/model.R` (`predict_fall_risk`), surfaced via
  `R/service.R` (`cdt_cohort_snapshot`).
- "How does the bot work / why was it silent?" → `R/bot.R` (`cdt_bot_reply`);
  originally webhook-only, now also runnable via `api/run_bot_poll.R` (polling).
- "How are secrets handled?" → `R/env.R` + the table in §6; nothing hardcoded.
- "Why did shinyapps.io fail on RSQLite?" → incomplete `renv.lock` + source
  compile; fixed by a full closure pinned to P3M binaries (§8).
- "Is any of this real patient data?" → No. 100% synthetic, seeded, rebuildable.
