# ---------------------------------------------------------------------------
# Bot / REST API host for the Clinical Digital Twin (plumber).
# Deploy target: Render (Web Service, Docker runtime). Also works on any
# container host. This image runs api/plumber.R (the Telegram webhook lives at
# POST /telegram/webhook).
#
# The Shiny DASHBOARD is deployed separately to shinyapps.io (see
# deploy/deploy_shinyapps.R); this image is only the API/bot.
#
# Build context is the REPO ROOT:
#   docker build -f deploy/Dockerfile -t cdt-bot .
#   docker run -p 8000:8000 --env-file .env cdt-bot
#
# SECRETS: never baked into the image. Provide at runtime via env vars
#   (Render dashboard -> Environment): TELEGRAM_BOT_TOKEN, ANTHROPIC_API_KEY,
#   TELEGRAM_WEBHOOK_SECRET, CDT_APP_URL. See docs/deployment.md.
# ---------------------------------------------------------------------------
FROM rocker/r-ver:4.4.1

# System libs needed by RSQLite / httr2 / curl / png rendering.
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev libsodium-dev \
    zlib1g-dev libpng-dev python3 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Install R package dependencies (pinned to CRAN). Kept as one layer for cache.
RUN R -e "install.packages(c( \
    'DBI','RSQLite','plogr','memoise','bit64','blob','rlang','dplyr','tidyr','tibble','lubridate','jsonlite','sodium', \
    'plumber','httr2','curl'), dependencies=TRUE, repos='https://cloud.r-project.org')"

# Copy the project source.
COPY . /app
ENV CDT_PROJECT_ROOT=/app

# Build the synthetic dataset + model at image-build time so the container is
# self-contained (the .sqlite/.rds are git-ignored and not in the repo).
RUN R -e "Sys.setenv(CDT_PROJECT_ROOT='/app'); source('data-raw/generate_synthetic_data.R')"

# Render provides $PORT; default to 8000 locally. Bind all interfaces so the
# platform can route to the container.
ENV CDT_API_HOST=0.0.0.0
EXPOSE 8000

# run_api.R reads CDT_API_HOST / CDT_API_PORT. Map Render's $PORT -> CDT_API_PORT.
CMD ["sh", "-c", "CDT_API_PORT=${PORT:-8000} Rscript api/run_api.R"]
