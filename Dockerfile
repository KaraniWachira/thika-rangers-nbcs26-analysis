FROM rocker/shiny:4.4.3

LABEL description="Thika Rangers NBCS 2026 — Interactive Season Analysis Dashboard"
LABEL maintainer="Keith Karani"

# ── System dependencies ──────────────────────────────────────────────
RUN apt-get update && apt-get install -y --no-install-recommends \
    libfftw3-dev \
    libx11-dev \
    libxt-dev \
    && rm -rf /var/lib/apt/lists/*

# ── R packages ───────────────────────────────────────────────────────
RUN R -e "install.packages(c( \
    'bslib', 'dplyr', 'tidyr', 'ggplot2', 'plotly', \
    'gt', 'DT', 'scales', 'patchwork', 'targets', \
    'imager', 'fontawesome', 'magrittr' \
  ), repos = 'https://cloud.r-project.org', Ncpus = parallel::detectCores())" \
  && rm -rf /tmp/downloaded_packages

# ── Copy project ─────────────────────────────────────────────────────
COPY . /app
WORKDIR /app

# ── Run pipeline to pre-compute outputs ─────────────────────────────
RUN R -e "targets::tar_make()"

# ── Port and startup ────────────────────────────────────────────────
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('shiny/app.R', host = '0.0.0.0', port = 3838)"]
