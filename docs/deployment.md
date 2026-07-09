# Deployment Guide

The system is **two processes** that must be deployed separately:

| Component | File | Host | Public URL role |
|---|---|---|---|
| **Dashboard** (Shiny) | `app.R` | **shinyapps.io** | Clinician UI; also the `CDT_APP_URL` the bot deep-links to |
| **Bot / REST API** (plumber) | `api/plumber.R` | **Render** (Docker) | Hosts `POST /telegram/webhook` for Telegram |

shinyapps.io runs only Shiny apps, so it **cannot** host the Telegram webhook — that is why the bot goes to Render.

> **Secrets rule:** never commit or bake `ANTHROPIC_API_KEY`, `TELEGRAM_BOT_TOKEN`, or `TELEGRAM_WEBHOOK_SECRET`. `.env` / `.Renviron` are git-ignored. On the hosts, set secrets through the platform's environment settings only.

---

## Part 0 — Run the bot locally (no public URL, no Render)

The fastest way to get a **working** bot calling the Claude API is **long-polling**:
`api/run_bot_poll.R` pulls messages from Telegram over an outbound HTTPS
connection and feeds them to the same dispatcher the webhook uses. No inbound
URL, tunnel, or hosting is required — only your machine and a terminal.

```bash
# 1. Put your real secrets in a git-ignored .env (copy from .Renviron.example):
#      TELEGRAM_BOT_TOKEN=<from BotFather>
#      ANTHROPIC_API_KEY=<your Anthropic key>   # omit -> deterministic mock replies
#      CDT_APP_URL=https://dgalgom.shinyapps.io/clinical-digital-twin/
# 2. Build the synthetic DB + model once:
Rscript setup.R
# 3. Register the command menu + description and clear any stale webhook/queue:
Rscript api/setup_telegram.R
# 4. Start the bot (stays online; Ctrl-C to stop):
Rscript api/run_bot_poll.R
```

`api/setup_telegram.R` is a one-off, idempotent SETUP step: it publishes the
folded **command menu** (`setMyCommands`, sourced from `cdt_bot_commands()`), sets
the empty-chat **description** (`setMyDescription`), and calls `deleteWebhook`
with `drop_pending_updates=TRUE` so the poller starts clean. Re-run it any time.

`run_bot_poll.R` prints `LLM mode: LIVE Claude`/`Groq` when a key is set (else
`MOCK`). With `GROQ_API_KEY` set the bot uses Groq's Llama 3.3 70B (~0.3–0.8 s);
with only `ANTHROPIC_API_KEY` it uses Claude. In Telegram: `/start` →
`login as clinician` → `How is patient P001 trending?`.

> **Polling vs webhook are mutually exclusive.** If a webhook was ever registered
> for this bot, `getUpdates` returns HTTP 409. `api/setup_telegram.R` clears it
> for you (via `deleteWebhook`); otherwise clear it manually with
> `curl "https://api.telegram.org/bot<TOKEN>/deleteWebhook"`. Use Part 0 (polling)
> **or** Parts 2+4 (webhook), not both.

Trade-off: the bot only runs while that terminal/process is up. For always-on
hosting use the Render webhook path (Parts 2 + 4) instead.

---

## Part 1 — Dashboard on shinyapps.io

**One-time local setup (you do this — it involves your account token):**

1. `install.packages("rsconnect")`
2. Log in at <https://www.shinyapps.io> → **Account → Tokens → Show** → copy the
   `rsconnect::setAccountInfo(name=…, token=…, secret=…)` line and run it once in
   your R session. Do **not** commit those values.

**Deploy:**

```bash
Rscript deploy/deploy_shinyapps.R
```

This rebuilds the synthetic DB + model (they are git-ignored), bundles `app.R` +
`R/*.R` + the generated `data/` artifacts, and pushes to shinyapps.io. When it
finishes it prints your URL, e.g. `https://<you>.shinyapps.io/clinical-digital-twin/`.

**Notes / caveats (fine for a demo, documented in `docs/production_readiness.md`):**
- Free tier sleeps on inactivity; the SQLite file is per-instance and ephemeral
  (resets on restart) — acceptable because the data is synthetic and rebuilt.
- The dashboard needs **no** API keys.
- Rename the app via `CDT_SHINYAPP_NAME=my-name Rscript deploy/deploy_shinyapps.R`.

**RSQLite build failures (fixed):** shinyapps.io previously failed while trying to
*compile* `RSQLite` from CRAN source. The fix is already applied — `renv.lock`'s
CRAN repository points at the **Posit Public Package Manager**
(`https://packagemanager.posit.co/cran/latest`), which serves precompiled Linux
binaries. `rsconnect` captures dependencies from `renv.lock`, so the generated
bundle manifest tells the shinyapps.io builder to install the RSQLite **binary**
(no `gcc`, fast, reliable). If you regenerate the lockfile with
`renv::snapshot()`, re-point that repository URL to P3M or the compile failure
returns.

Copy the resulting URL — you will give it to the bot as `CDT_APP_URL` in Part 2.

---

## Part 2 — Bot / API on Render

The repo ships `deploy/Dockerfile` and `deploy/render.yaml`. The image builds the
synthetic data at build time and runs `api/plumber.R`, binding Render's `$PORT`.

**Steps:**

1. Push this repo to GitHub (see Part 3).
2. In Render: **New + → Blueprint** → select this repo. Render detects
   `deploy/render.yaml` and proposes the `cdt-bot` web service (Docker runtime,
   free plan, health check `/health`).
   - If Blueprint doesn't pick up the nested path, instead choose **New + → Web
     Service → Docker**, set **Dockerfile path** = `deploy/Dockerfile` and
     **Docker context** = `.` (repo root).
3. In the service's **Environment** settings, add (these are `sync:false` in the
   blueprint, so you enter them by hand):
   - `TELEGRAM_BOT_TOKEN` = your BotFather token *(required for the bot to send)*
   - `ANTHROPIC_API_KEY` = optional (enables live Claude replies; otherwise mock)
   - `TELEGRAM_WEBHOOK_SECRET` = any random string *(recommended)*
   - `CDT_APP_URL` = your shinyapps.io URL from Part 1
   - Do **not** set `PORT` — Render injects it; the Dockerfile maps it to `CDT_API_PORT`.
4. Deploy. When live, note the service URL, e.g. `https://cdt-bot.onrender.com`.
5. Verify the API: open `https://cdt-bot.onrender.com/health` → `{"status":"ok",…}`.

**Free-plan note:** Render free web services spin down when idle and cold-start on
the next request. Telegram retries webhook deliveries, so the bot still works but
the first message after idle may be slow.

---

## Part 3 — Push the repo to GitHub

The local commit is already created for you. To publish:

```bash
# Create an empty repo on github.com first (no README/license), then:
git remote add origin https://github.com/<you>/<repo>.git
git push -u origin master
```

If you use the GitHub CLI instead:

```bash
gh auth login
gh repo create <repo> --private --source=. --remote=origin --push
```

`.gitignore` already excludes secrets (`.env`, `.Renviron`) and generated
artifacts (`data/*.sqlite`, `data/*.rds`), so nothing sensitive is pushed.

---

## Part 4 — Register the Telegram webhook + menu (after the bot is live)

Run these yourself (they use your real bot token and hit Telegram's network).
Replace `<TOKEN>` and `<bot-host>` (your Render URL).

```bash
# Point Telegram at your Render service. Include the secret if you set one.
curl "https://api.telegram.org/bot<TOKEN>/setWebhook" \
  -d "url=https://<bot-host>/telegram/webhook" \
  -d "secret_token=<TELEGRAM_WEBHOOK_SECRET>"

# Confirm registration + see any delivery errors (best diagnostic):
curl "https://api.telegram.org/bot<TOKEN>/getWebhookInfo"

# Register the command menu (payload comes from cdt_bot_commands()):
Rscript -e 'for(f in list.files("R","[.]R$",full.names=TRUE))source(f); \
  cat(jsonlite::toJSON(list(commands=cdt_bot_commands()),auto_unbox=TRUE))' > /tmp/cmds.json
curl -X POST "https://api.telegram.org/bot<TOKEN>/setMyCommands" \
  -H "Content-Type: application/json" -d @/tmp/cmds.json
```

Then message your bot: `/start` → `login as clinician` → `/triage`, `/history P001`,
`/dashboard P001`. If `/start` is silent, check `getWebhookInfo` — the
`last_error_message` field will show TLS, 401 (secret mismatch), or 5xx problems.

---

## Why the bot was silent before

Out of the box the bot is **webhook-only**, so `/start` did nothing until Telegram
had a public HTTPS endpoint to POST to. Two ways to fix it:

- **Local, simplest (Part 0):** `Rscript api/run_bot_poll.R` — long-polls Telegram,
  no public URL needed. Runs while the process is up.
- **Always-on (Parts 2 + 4):** host `api/plumber.R` on Render and register the
  webhook with `setWebhook`. Requires `TELEGRAM_BOT_TOKEN` on the host, a public
  HTTPS URL, and the `setWebhook` call.

Either path calls the Claude API automatically once `ANTHROPIC_API_KEY` is set.
