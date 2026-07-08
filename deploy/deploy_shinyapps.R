#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Deploy the Shiny dashboard (app.R) to shinyapps.io.
#
# PREREQUISITES (you do these once, locally):
#   1. install.packages("rsconnect")
#   2. Create a free account at https://www.shinyapps.io
#   3. Account -> Tokens -> "Show" -> copy the setAccountInfo() call and run it
#      ONCE in your R session (do NOT commit the token/secret):
#        rsconnect::setAccountInfo(name="<acct>", token="<token>", secret="<secret>")
#
# Then run:  Rscript deploy/deploy_shinyapps.R
#
# NOTES
#  * shinyapps.io runs ONLY the Shiny app (app.R) -- NOT the plumber API / bot.
#    Deploy the bot separately (see deploy/Dockerfile + deploy/render.yaml).
#  * The SQLite DB + model are git-ignored and are rebuilt at deploy time by the
#    generator below so the bundle is self-contained and reproducible.
#  * NEVER put ANTHROPIC_API_KEY / TELEGRAM_* here. The dashboard does not need
#    them; if you ever do, set them via the shinyapps.io dashboard env settings,
#    never in the bundle.
# ---------------------------------------------------------------------------

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Please install.packages('rsconnect') and run setAccountInfo() first.")
}

root <- normalizePath(file.path(dirname(sub("^--file=", "",
  commandArgs(FALSE)[grep("^--file=", commandArgs(FALSE))])), ".."))
if (!nzchar(root) || is.na(root)) root <- normalizePath(getwd())
setwd(root)
Sys.setenv(CDT_PROJECT_ROOT = root)

# Ensure the demo dataset + model exist so the deployed app has data to show.
if (!file.exists(file.path(root, "data", "clinical_twin.sqlite")) ||
    !file.exists(file.path(root, "data", "fall_risk_model.rds"))) {
  message("Building synthetic dataset + model for the bundle...")
  source(file.path(root, "data-raw", "generate_synthetic_data.R"))
}

app_name <- Sys.getenv("CDT_SHINYAPP_NAME", unset = "clinical-digital-twin")

# Files the app needs at runtime. The generated data/ artifacts are included
# explicitly (they are .gitignored but must ship in the deploy bundle).
app_files <- c(
  "app.R",
  list.files("R", pattern = "[.]R$", full.names = TRUE),
  "data/clinical_twin.sqlite",
  "data/fall_risk_model.rds"
)

message("Deploying '", app_name, "' to shinyapps.io ...")
rsconnect::deployApp(
  appDir = root,
  appFiles = app_files,
  appPrimaryDoc = "app.R",
  appName = app_name,
  forceUpdate = TRUE
)
message("Done. Your dashboard URL will be printed above / shown in the ",
  "shinyapps.io dashboard. Use it as CDT_APP_URL for the bot's /dashboard link.")
