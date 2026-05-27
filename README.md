# Thika Rangers — NBCS 2026 Regular Season Analysis

Interactive batting & pitching analysis dashboard for the Thika Rangers Baseball Club's 2026 NBCS regular season. Built with R/{targets}, {ggplot2}/{plotly}, {gt}, and {bslib} Shiny.

---

## Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                   TARGETS PIPELINE                           │
│  tr-stats26.csv  ──►  batting_data  ──►  plots              │
│                        team_summary  ──►  gt_table           │
│                        save_plots    ──►  export_data        │
└──────────┬───────────────────────────────────────────────────┘
           │  output/*.rds, output/*.png
           ▼
┌──────────────────────────────────────────────────────────────┐
│                    SHINY DASHBOARD                           │
│  app.R                                                       │
│    ├── Header bar (team colours + branding)                  │
│    ├── Nav tabs (6):                                         │
│    │   ├── Visualizations  — sidebar + plotly cards          │
│    │   ├── Batting Table   — interactive GT table            │
│    │   ├── Pitching Table  — interactive GT table            │
│    │   ├── Discussion      — performance commentary          │
│    │   ├── Methodology     — statistical definitions          │
│    │   └── Workflow        — pipeline docs                   │
│    └── Value boxes  ───  Team OPS / Runs / RBI / HR          │
└──────────────────────────────────────────────────────────────┘
```

### Pipeline (7 targets)

| Target | Description |
|---|---|
| `raw_data_path` | Track source CSV (invalidates on change) |
| `batting_data` | Parse & clean batting/pitching/fielding data |
| `team_summary` | Compute aggregate offensive metrics |
| `plots` | Generate 12 ggplot2 visualizations |
| `gt_table` | Build interactive GT batting table |
| `save_plots` | Export all plots as high-res PNGs |
| `export_data` | Serialise RDS for Shiny consumption |

### Visualizations (13)

1. OPS Leaderboard — ranked lollipop
2. Offensive Profile Matrix — faceted AVG/OBP/SLG/OPS
3. OBP vs SLG Quadrant — sweet-spot zones
4. Offensive Trinity — OBP × SLG × AVG bubble
5. Pitching Summary — tile heatmap + rate stats (Maina · Tuju · Derrick · Caleb)
6. Run Production — runs vs RBI scatter
7. Hit Type Composition — 1B/2B/3B/HR proportions
8. Plate Discipline — walks vs strikeouts
9. Counting Stats Heatmap — all batting counts
10. PA Distribution — playing time volume
11. Extra-Base Hit Rate — XBH % of hits
12. Contact Quality — AVG vs SLG trend
13. Rate Stats Comparison — ERA/WHIP/K-BB among pitchers

---

## Directory Structure

```
thika-rangers-nbcs26-analysis/
├── _targets.R                # targets pipeline definition
├── tr-stats26.csv            # Source data (GameChanger export)
├── report.qmd                # Quarto static report
├── custom.scss               # Quarto theme overrides
├── output/                   # Pipeline outputs (PNGs + RDS)
├── R/
│   ├── functions.R           # Data ingestion, 12 plot fns, GT tables
│   └── team_theme.R          # Logo colour extraction + ggplot2/bslib theme
└── shiny/
    ├── app.R                 # Shiny dashboard
    └── www/
        ├── TR-Logo.png       # Team logo
        └── rebranded-logo.png
```

---

## R Packages

### Core
| Package | Version | Purpose |
|---|---|---|
| shiny | 1.13.0 | Web app framework |
| bslib | 0.10.0 | Bootstrap 5 theming |
| targets | 1.12.0 | Reproducible pipeline |
| dplyr | 1.2.1 | Data manipulation |
| tidyr | 1.3.2 | Data reshaping |
| ggplot2 | 4.0.3 | Static plots |
| plotly | 4.12.0 | Interactive widgets |
| gt | 1.3.0 | Beautiful tables |
| DT | 0.34.0 | Data table interactivity |

### Supporting
| Package | Version | Purpose |
|---|---|---|
| scales | 1.4.0 | Axis formatting |
| patchwork | 1.3.2 | Plot composition |
| imager | 1.0.8 | Logo colour extraction (k-means) |
| fontawesome | 0.5.3 | Tab icons |
| magrittr | 2.0.5 | Pipe operator |

### Not installed (system dependency blockers)
- **gganimate** — blocked by {transformr} → {sf} system libs
- **magick** — blocked by libmagick++

---

## Setup

```r
# Install packages
install.packages(c(
  "shiny", "bslib", "targets", "dplyr", "tidyr",
  "ggplot2", "plotly", "gt", "DT", "scales",
  "patchwork", "imager", "fontawesome", "magrittr"
))

# Run pipeline (from project root)
targets::tar_make()

# Launch dashboard
shiny::runApp("shiny/app.R")
```

---

## Deployment Options

### 1. Docker (recommended)

Build and run locally:

```bash
docker build -t thika-rangers .
docker run -p 3838:3838 thika-rangers
# Open http://localhost:3838
```

The `Dockerfile`:
- Uses `rocker/shiny:4.4.3` as base
- Installs system deps (`libfftw3-dev`, `libx11-dev`, `libxt-dev`) for {imager}
- Installs all R packages in parallel
- Runs the targets pipeline at build time (pre-computes all outputs)
- Starts the Shiny app on port 3838

### 2. shinyapps.io

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(token = "...", secret = "...")
rsconnect::deployApp("shiny/app.R")
```

Free tier: 5 apps, 25 active hours/month.

### 3. Hugging Face Spaces

1. Push this repo to GitHub
2. Create a Space at https://huggingface.co/new-space
3. SDK: **Docker**
4. Connect your GitHub repo — the `Dockerfile` at the root will be used automatically
5. Space will build and serve the app

### 4. Shiny Server (self-hosted)

```bash
# On a Linux VPS
sudo apt install shiny-server
git clone https://github.com/<user>/thika-rangers-nbcs26-analysis.git
sudo ln -s /path/to/shiny/ /srv/shiny-server/thika-rangers
```

---

## CI/CD

A GitHub Actions workflow (`.github/workflows/docker.yml`) is included. On every push to `main`/`master`:

1. Builds the Docker image using cache from previous runs
2. Pushes to **GitHub Container Registry** (GHCR) — `ghcr.io/<your-org>/thika-rangers-nbcs26-analysis`
3. Tags: `latest` (on default branch) + short SHA + PR ref

No secrets are required — it uses the built-in `GITHUB_TOKEN` with `packages: write` permission.

### Deploy the GHCR image to a VPS

Add a deploy step by creating a GitHub secret `SSH_HOST` / `SSH_KEY` and uncomment the deploy block in the workflow, or run manually:

```bash
docker pull ghcr.io/<your-org>/thika-rangers-nbcs26-analysis:latest
docker run -d -p 3838:3838 --restart unless-stopped \
  --name thika-rangers \
  ghcr.io/<your-org>/thika-rangers-nbcs26-analysis:latest
```

---

## Data Source

Game statistics exported from GameChanger via the **Nairobi Baseball Community**. The raw CSV contains batting (cols 4–54), pitching (cols 55–112), and fielding (cols 156–180) data.

---

## Credits

**Analysis by:** Keith Karani  
**Data:** Nairobi Baseball Community  
**Season:** NBCS 2026 Regular Season  
**Colours:** Extracted from team logo via k-means clustering (R/{imager})
