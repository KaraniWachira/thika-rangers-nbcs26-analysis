###############################################################################
#  NBCS 2026 — Thika Rangers Season Analysis Dashboard
#
#  Header + tab navigation layout. 12 visualizations + pitching summary +
#  interactive GT tables (batting & pitching) + methodology + workflow docs.
#
#  Run with:
#    shiny::runApp("shiny/app.R")           # from project root
#    shiny::runApp()                        # from shiny/ directory
###############################################################################

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gt)
library(DT)
library(plotly)

# ── Resolve paths ──────────────────────────────────────────────────────────
app_dir <- getwd()
if (basename(app_dir) == "shiny") app_dir <- dirname(app_dir)

logo_path <- file.path(app_dir, "shiny/www/TR-Logo.png")
if (!file.exists(logo_path))
  logo_path <- file.path(app_dir, "shiny/www/rebranded-logo.png")
options(thika_logo_path = logo_path)

source(file.path(app_dir, "R/team_theme.R"), local = TRUE)
source(file.path(app_dir, "R/functions.R"),  local = TRUE)

# ── Load data ──────────────────────────────────────────────────────────────
data_path  <- file.path(app_dir, "output/batting_data.rds")
summary_path <- file.path(app_dir, "output/team_summary.rds")
gt_path    <- file.path(app_dir, "output/gt_table.rds")

if (file.exists(data_path)) {
  batting <- readRDS(data_path)
  team_ss <- readRDS(summary_path)
  gt_tab  <- readRDS(gt_path)
} else {
  batting <- read_batting_data(file.path(app_dir, "tr-stats26.csv"))
  team_ss <- compute_team_summary(batting)
  gt_tab  <- build_gt_batting_table(batting, team_ss)
}
plots <- generate_all_plots(batting, csv_path = file.path(app_dir, "tr-stats26.csv"))
pitching_gt <- build_gt_pitching_table(file.path(app_dir, "tr-stats26.csv"))
fielding    <- read_fielding_data(file.path(app_dir, "tr-stats26.csv"))

# ── Derived values for UI ───────────────────────────────────────────────────
top_ops_player <- batting$Player[which.max(batting$OPS)]
top_ops_val    <- max(batting$OPS, na.rm = TRUE)
top_avg_player <- batting$Player[which.max(batting$AVG)]
top_hr_player  <- batting$Player[which.max(batting$HR)]
top_hr_count   <- max(batting$HR, na.rm = TRUE)
top_h_player   <- batting$Player[which.max(batting$H)]
top_h_count    <- max(batting$H, na.rm = TRUE)

# ── Fielding aggregates ─────────────────────────────────────────────────────
team_tc  <- sum(fielding$TC, na.rm = TRUE)
team_po  <- sum(fielding$PO, na.rm = TRUE)
team_a   <- sum(fielding$A, na.rm = TRUE)
team_e   <- sum(fielding$E, na.rm = TRUE)
team_fpct <- round((team_po + team_a) / team_tc, 3)
team_dp  <- sum(fielding$DP, na.rm = TRUE)
team_pb  <- sum(fielding$PB, na.rm = TRUE)
team_sb  <- sum(fielding$SB, na.rm = TRUE)
team_cs  <- sum(fielding$CS, na.rm = TRUE)
team_sbatt <- sum(fielding$SBATT, na.rm = TRUE)
team_cs_pct <- round(team_cs / team_sbatt * 100, 1)

# ── Pitching discussion stats ───────────────────────────────────────────────
pitching_disc <- read_pitching_data(file.path(app_dir, "tr-stats26.csv"))
pitchers <- pitching_disc %>% filter(IP > 0)

maina <- pitchers %>% filter(grepl("Maina", Player))
tuju  <- pitchers %>% filter(grepl("Tuju", Player))
derrick <- pitchers %>% filter(grepl("Derrick", Player))
caleb <- pitchers %>% filter(grepl("Caleb", Player))

# ── Discussion HTML ─────────────────────────────────────────────────────────
discussion_html <- tagList(
  tags$div(class = "p-4",
    tags$style(HTML("
      .disc-section { margin-bottom: 28px; }
      .disc-section h4 {
        color: #77121C; font-weight: 600;
        border-bottom: 2px solid #77121C;
        padding-bottom: 4px; margin-bottom: 12px;
        font-size: 1rem;
      }
      .disc-section p { margin-bottom: 6px; line-height: 1.6; font-size: 0.88rem; }
      .disc-section ul { padding-left: 18px; font-size: 0.88rem; }
      .disc-section li { margin-bottom: 3px; }
      .stat-highlight { font-weight: 600; color: #2C3E50; }
      .stat-muted { color: #6B7B8D; }
      .divider { border: none; border-top: 1px solid #EAE6E0; margin: 16px 0; }
    ")),
    # ── Batting ──
    tags$div(class = "disc-section",
      tags$h4(tags$span(style = "margin-right: 8px;", "\u26CF"), "Batting"),
      tags$p(
        "The Thika Rangers posted ", tags$span(class = "stat-highlight", "elite offensive numbers"),
        " during the NBCS 2026 regular season, finishing with a team slash line of ",
        tags$span(class = "stat-highlight", sprintf(".%.3f/.%.3f/.%.3f", team_ss$team_AVG, team_ss$team_OBP, team_ss$team_SLG)),
        " and an OPS of ", tags$span(class = "stat-highlight", sprintf("%.3f", team_ss$team_OPS)),
        " across ", sum(batting$PA, na.rm = TRUE), " plate appearances. The .", sprintf("%.0f", team_ss$team_OBP * 1000),
        " on-base percentage reflects a disciplined approach — the club drew ",
        tags$span(class = "stat-highlight", sum(batting$BB, na.rm = TRUE), " walks"),
        " against ", sum(batting$SO, na.rm = TRUE), " strikeouts (",
        sprintf("%.0f", sum(batting$BB, na.rm = TRUE) / sum(batting$SO, na.rm = TRUE) * 100),
        "% BB/K ratio), with a ", sprintf("%.1f", sum(batting$BB, na.rm = TRUE) / sum(batting$PA, na.rm = TRUE) * 100),
        "% walk rate that kept pressure on opposing pitchers."
      ),
      tags$p(
        "The offense was ", tags$span(class = "stat-highlight", "contact-driven"),
        " with limited raw power: ", sum(batting$H, na.rm = TRUE), " hits produced just ",
        sum(batting$`1B`, na.rm = TRUE), " singles, ", sum(batting$`2B`, na.rm = TRUE), " doubles, ",
        sum(batting$`3B`, na.rm = TRUE), " triples, and ", sum(batting$HR, na.rm = TRUE), " home run",
        if (sum(batting$HR, na.rm = TRUE) == 1) "" else "s",
        " (", sprintf("%.1f", sum(batting$XBH, na.rm = TRUE) / sum(batting$H, na.rm = TRUE) * 100),
        "% extra-base hit rate). The ", sprintf("%.0f", sum(batting$TB, na.rm = TRUE)),
        " total bases and .", sprintf("%.0f", team_ss$team_SLG * 1000),
        " slugging percentage show that the team manufactured runs through consistent
        contact and smart baserunning rather than the long ball."
      ),
      tags$ul(
        tags$li(
          tags$span(class = "stat-highlight", "Maina Isaiah"),
          " was the engine of the offence: ", sprintf("%.0f", batting$PA[batting$Player == "Maina Isaiah"]), " PA, ",
          " .636/.789/.727, 7 hits, 8 RBI, 7 runs, 8 walks against only 2 strikeouts. His ",
          sprintf("%.1f", 8 / 19 * 100), "% walk rate and ", sprintf("%.0f", 7 / 8 * 100), "% on-base from contact were elite."
        ),
        tags$li(
          tags$span(class = "stat-highlight", "Tuju Oduori"),
          " provided the team's only home run while slashing .556/.667/1.000 with 5 RBI in 12 PA."
        ),
        tags$li(
          tags$span(class = "stat-highlight", "Moses Makabe"),
          " and ", tags$span(class = "stat-highlight", "Brian Valusi"),
          " both collected 6+ hits with OPS above 1.100, forming a deep core of reliable hitters."
        ),
        tags$li(
          tags$span(class = "stat-muted", "Gilbert Talam"),
          " drew a team-high 7 walks (", sprintf("%.0f", 7 / 20 * 100), "% BB rate) but struck out 7 times,
          posting a .231 average. His .500 OBP shows he was a table-setter despite the low batting average."
        ),
        tags$li(
          "The team produced ", sum(batting$R, na.rm = TRUE), " runs and ",
          sum(batting$RBI, na.rm = TRUE), " RBI across ",
          sum(batting$GP, na.rm = TRUE), " games, scoring well above the league average through aggressive baserunning and on-base ability."
        )
      )
    ),
    tags$hr(class = "divider"),
    # ── Pitching ──
    tags$div(class = "disc-section",
      tags$h4(tags$span(style = "margin-right: 8px;", "\U1F3D2"), "Pitching"),
      tags$p(
        "The pitching staff was ", tags$span(class = "stat-highlight", "a tale of strikeout dominance and control struggles"),
        ". Four pitchers logged meaningful innings, headlined by workhorse ",
        tags$span(class = "stat-highlight", "Maina Isaiah"),
        " who covered ", maina$IP, " innings across multiple appearances."
      ),
      tags$ul(
        tags$li(
          tags$span(class = "stat-highlight", "Maina Isaiah"),
          " (", maina$W, "-", maina$L, "): ", maina$IP, " IP, ", maina$SO, " strikeouts",
          " (", sprintf("%.1f", maina$SO / as.numeric(maina$IP) * 9), " K/9) — an elite strikeout rate.
          However, ", maina$BB, " walks (", sprintf("%.1f", maina$BB / as.numeric(maina$IP) * 9), " BB/9) and ",
          maina$H, " hits allowed drove his ERA to ", sprintf("%.2f", maina$ERA),
          " and WHIP to ", sprintf("%.2f", maina$WHIP),
          ". A ", sprintf("%.2f", maina$K_BB), " K/BB ratio shows room for improved command."
        ),
        tags$li(
          tags$span(class = "stat-highlight", "Tuju Oduori"),
          " (", tuju$W, "-", tuju$L, "): ", tuju$IP, " IP, ", tuju$SO, " strikeouts",
          " (", sprintf("%.1f", tuju$SO / as.numeric(tuju$IP) * 9), " K/9), ",
          tuju$BB, " walks, ", sprintf("%.2f", tuju$K_BB), " K/BB.
          His ", sprintf("%.2f", tuju$ERA), " ERA and ", sprintf("%.2f", tuju$WHIP),
          " WHIP were similar to Maina's but he limited damage well enough to earn a perfect ",
          tuju$W, "-", tuju$L, " record."
        ),
        tags$li(
          tags$span(class = "stat-muted", "Derrick Mwendwa"),
          " and ", tags$span(class = "stat-muted", "Caleb Ochego"),
          " had limited opportunities (", derrick$IP, " and ", caleb$IP, " IP respectively).
          Derrick showed a strong strikeout rate (", sprintf("%.1f", derrick$SO / as.numeric(derrick$IP) * 9),
          " K/9) in a small sample, while Caleb struggled with command (",
          caleb$BB, " walks in ", caleb$IP, " IP)."
        ),
        tags$li(
          "Collectively, the staff ", tags$span(class = "stat-muted", "struck out ",
          sum(pitchers$SO, na.rm = TRUE), " batters"),
          " in ", sprintf("%.0f", sum(pitchers$IP, na.rm = TRUE)), " innings with a K/9 of ",
          sprintf("%.0f", sum(pitchers$SO, na.rm = TRUE) / as.numeric(sum(pitchers$IP, na.rm = TRUE)) * 9),
          ". The primary area for growth is limiting free passes and reducing hits per inning."
        )
      )
    ),
    tags$hr(class = "divider"),
    # ── Fielding ──
    tags$div(class = "disc-section",
      tags$h4(tags$span(style = "margin-right: 8px;", "\U1F3DF"), "Fielding"),
      tags$p(
        "Defensively, the Rangers recorded ", tags$span(class = "stat-highlight",
        sprintf("%.3f", team_fpct), " fielding percentage"),
        " across ", team_tc, " total chances (", team_po, " putouts, ", team_a,
        " assists, ", team_e, " errors). They turned ", team_dp, " double plays.
        The ", team_e, " errors are an area for improvement, but several players posted perfect fielding marks."
      ),
      tags$ul(
        tags$li(
          tags$span(class = "stat-highlight", "Gilbert Talam"),
          " handled the most chances among position players (", team_tc, " TC) with ",
          team_e, " errors and a .", paste0(substr(as.character(team_fpct), 3, 5)), " fielding percentage."
        ),
        tags$li(
          tags$span(class = "stat-highlight", "Derrick Mwendwa"),
          " (19 TC), ", tags$span(class = "stat-highlight", "Manyara Karanja"),
          " (15 TC), and ", tags$span(class = "stat-highlight", "Stephen Kyalo"),
          " (4 TC) all posted ", tags$span(class = "stat-highlight", "perfect 1.000 FPCT"),
          ", along with Keith Wachira, Ezra Omolo, Moses Makabe, Brian Valusi, and Wenslaus Juma."
        ),
        tags$li(
          "The catching corps (", 
          paste(
            fielding %>% filter(C > 0 & !is.na(C)) %>% pull(Player) %>% head(3),
            collapse = ", "
          ),
          ") handled ", team_pb, " passed balls, ", team_sbatt, " stolen base attempts",
          " (", team_sb, " SB, ", team_cs, " CS) for a ", team_cs_pct, "% caught-stealing rate.
          Three passed balls suggest room for refined pitch-framing and blocking."
        ),
        tags$li(
          tags$span(class = "stat-muted", "Benson Kahugu"),
          " (8 TC, 2 errors, .750 FPCT) and ", tags$span(class = "stat-muted", "Orgot Norton"),
          " (5 TC, 1 error, .800 FPCT) had the lowest fielding percentages among players with multiple chances."
        )
      )
    ),
    tags$hr(class = "divider"),
    tags$p(style = "font-size: 0.8rem; color: #8B8B8B; text-align: center; margin-top: 8px;",
      "Analysis by Keith Karani  ·  Data: Nairobi Baseball Community  ·  NBCS 2026 Regular Season")
  )
)

# ── Plot choices (12 total) ─────────────────────────────────────────────────
plot_choices <- list(
  "OPS Leaderboard"               = "leaderboard",
  "Offensive Profile Matrix"      = "matrix",
  "OBP vs SLG Quadrant"           = "quadrant",
  "Offensive Trinity Bubble"      = "triple",
  "Pitching Summary"               = "contributions",
  "Run Production"                = "production",
  "Hit Type Composition"          = "composition",
  "Plate Discipline"              = "discipline",
  "Counting Stats Heatmap"        = "heatmap",
  "PA Distribution"               = "pa_dist",
  "Extra-Base Hit Rate"           = "xbh_rate",
  "Contact Quality: AVG vs SLG"   = "contact_slg"
)

plot_info <- list(
  matrix       = c("Offensive Profile Matrix",
                   "AVG / OBP / SLG / OPS with team mean reference lines"),
  quadrant     = c("OBP vs SLG Quadrant Analysis",
                   "Sweet-spot zones — bubble size = PA, colour = OPS"),
  composition  = c("Hit Type Composition",
                   "Proportion of 1B / 2B / 3B / HR among total hits"),
  discipline   = c("Plate Discipline: Walks vs Strikeouts",
                   "Above line = more SO than BB — point colour = OBP"),
  heatmap      = c("Counting Stats Heatmap",
                   "Darker = higher counts — all batting metrics in one view"),
  leaderboard  = c("OPS Leaderboard",
                   "On-Base Plus Slugging — ranked best to worst"),
  production   = c("Run Production",
                   "Runs Scored vs RBI — size = hits, colour = OPS"),
  contributions = c("Pitching Summary — Maina · Tuju · Derrick · Caleb",
                    "Tile heatmap of 11 stats + rate stat comparison"),
  triple       = c("Offensive Trinity",
                   "OBP vs SLG — colour = AVG, bubble size = PA"),
  pa_dist      = c("Plate Appearance Distribution",
                   "Playing time volume — bar length = PA, colour = OBP"),
  xbh_rate     = c("Extra-Base Hit Rate",
                   "% of hits that went for extra bases (2B+3B+HR) — colour = SLG"),
  contact_slg  = c("Contact Quality: AVG vs SLG",
                   "Does average translate to power? — size = hits, colour = OBP")
)

# ── Common sidebar for non-viz tabs ─────────────────────────────────────────
common_sidebar <- function(text) {
  sidebar(width = 200, bg = "#F5F0EB", helpText(text))
}

# ── UI ─────────────────────────────────────────────────────────────────────

ui <- page_fluid(
  theme = shiny_theme,
  window_title = "NBCS 2026 — Thika Rangers Season Analysis",
  lang = "en",

  # ── Custom CSS ──────────────────────────────────────────────────────────
  tags$head(
    tags$style(HTML("
      .nav-tabs {
        border-bottom: 2px solid #EAE6E0;
        padding-left: 8px;
        background: white;
      }
      .nav-tabs .nav-link {
        color: #5A7D9C;
        font-weight: 500;
        border: none;
        border-bottom: 2px solid transparent;
        margin-bottom: -2px;
        padding: 10px 18px;
        transition: color 0.15s, border-color 0.15s;
      }
      .nav-tabs .nav-link:hover {
        color: #2C3E50;
        border-color: transparent;
      }
      .nav-tabs .nav-link.active {
        color: #77121C;
        font-weight: 600;
        border-bottom: 2px solid #77121C;
        background: transparent;
      }
      .nav-tabs .nav-link i, .nav-tabs .nav-link svg {
        margin-right: 6px;
      }
      .bslib-full-screen-enter {
        position: absolute !important;
        bottom: 10px !important;
        right: 10px !important;
        z-index: 10 !important;
        background: white !important;
        border: 1px solid #77121C !important;
        color: #77121C !important;
        font-size: 0.8rem !important;
        padding: 4px 10px !important;
        opacity: 0.85 !important;
        transition: opacity 0.15s, background 0.15s !important;
        box-shadow: 0 1px 4px rgba(0,0,0,0.12) !important;
        border-radius: 4px !important;
        line-height: 1 !important;
      }
      .bslib-full-screen-enter:hover {
        opacity: 1 !important;
        background: #77121C !important;
        color: white !important;
        box-shadow: 0 2px 8px rgba(119, 18, 28, 0.3) !important;
      }
      .value-box-grid {
        display: grid;
        grid-template-columns: repeat(2, 1fr);
        gap: 6px;
      }
      .value-box-grid .bslib-value-box .card-body {
        padding: 4px 8px !important;
      }
      .value-box-grid .bslib-value-box .value-box-title {
        font-size: 0.55rem !important;
        margin-bottom: 0 !important;
      }
      .value-box-grid .bslib-value-box .value-box-value {
        font-size: 0.85rem !important;
        line-height: 1.1 !important;
      }
      .value-box-grid .bslib-value-box .value-box-subtitle {
        font-size: 0.55rem !important;
        margin-top: 0 !important;
      }
      .value-box-grid .bslib-value-box {
        min-height: unset !important;
        height: 48px !important;
      }
      .value-box-grid .bslib-value-box .value-box-showcase {
        font-size: 0.85rem !important;
        align-self: center !important;
      }

    "))
  ),

  # ── Header bar ──────────────────────────────────────────────────────────
  div(
    class = "d-flex align-items-center px-3 py-2",
    style = sprintf("background: %s;", team_palette$dark),
    if (file.exists(file.path(app_dir, "shiny/www/rebranded-logo.png")))
      tags$img(src = "rebranded-logo.png", height = "26px",
               alt = "TR", style = "margin-right: 10px;")
    else if (file.exists(file.path(app_dir, "shiny/www/TR-Logo.png")))
      tags$img(src = "TR-Logo.png", height = "26px",
               alt = "TR", style = "margin-right: 10px;"),
    tags$span("NBCS 2026 Regular Season  ·  Thika Rangers",
              style = "color: white; font-weight: 500; font-size: 0.95rem;
                       letter-spacing: 0.3px;"),
    div(style = "flex: 1;"),
    tags$span("Analysis by Keith Karani",
              style = "color: rgba(255,255,255,0.5); font-size: 0.75rem;")
  ),

  # ── Tab navigation ──────────────────────────────────────────────────────
  navset_tab(

    # ────────────────────────────────────────────────────────────────────
    # TAB 1: VISUALIZATIONS
    # ────────────────────────────────────────────────────────────────────
    nav_panel(
      title = "Visualizations",
      icon  = fontawesome::fa_i("chart-simple"),

      div(
        style = "height: calc(100vh - 100px); display: flex;
                 flex-direction: column; padding: 8px 12px 4px 12px;",
        layout_sidebar(
          fillable = TRUE,
          sidebar = sidebar(
            width = 260,
            open = "always",
            bg = "#F5F0EB",
            padding = c(10, 12),

            div(
              style = "text-align: center; margin-bottom: 6px;
                       display: flex; align-items: center; gap: 6px;
                       justify-content: center;",
              if (file.exists(file.path(app_dir, "shiny/www/rebranded-logo.png")))
                tags$img(src = "rebranded-logo.png", height = "32px", alt = "")
              else if (file.exists(file.path(app_dir, "shiny/www/TR-Logo.png")))
                tags$img(src = "TR-Logo.png", height = "32px", alt = ""),
              tags$span("NBCS 2026",
                        style = "font-weight: 700; font-size: 0.9rem;
                                 color: #2C3E50;")
            ),

            radioButtons(
              inputId  = "plot_selector",
              label    = tags$span(style = "font-weight: 600; color: #2C3E50;
                                            font-size: 0.8rem;",
                                   "Select Visualization"),
              choices  = plot_choices,
              selected = "leaderboard",
              width    = "100%"
            ),

            hr(style = "margin: 4px 0; border-color: #E0D8CE;"),

            tags$div(
              style = "font-size: 0.78rem; color: #2C3E50;",
              tags$p(style = "font-weight: 600; margin: 4px 0 2px 0;
                              font-size: 0.8rem;", "Team Summary"),
              tags$table(
                style = "width: 100%; line-height: 1.2;",
                tags$tr(tags$td("AVG"),
                  tags$td(sprintf("%.3f", team_ss$team_AVG),
                          style = "text-align: right; font-weight: 600;")),
                tags$tr(tags$td("OBP"),
                  tags$td(sprintf("%.3f", team_ss$team_OBP),
                          style = "text-align: right; font-weight: 600;")),
                tags$tr(tags$td("SLG"),
                  tags$td(sprintf("%.3f", team_ss$team_SLG),
                          style = "text-align: right; font-weight: 600;")),
                tags$tr(tags$td("OPS"),
                  tags$td(sprintf("%.3f", team_ss$team_OPS),
                          style = "text-align: right; font-weight: 700;
                                   color: #77121C;")),
                tags$tr(tags$td("Runs"),
                  tags$td(as.character(team_ss$total_R),
                          style = "text-align: right;")),
                tags$tr(tags$td("RBI"),
                  tags$td(as.character(team_ss$total_RBI),
                          style = "text-align: right;")),
                tags$tr(tags$td("HR"),
                  tags$td(as.character(team_ss$total_HR),
                          style = "text-align: right;"))
              )
            ),

            hr(style = "margin: 4px 0; border-color: #E0D8CE;"),

            tags$div(
              style = "font-size: 0.78rem; color: #2C3E50;",
              tags$p(style = "font-weight: 600; margin: 4px 0 2px 0;
                              font-size: 0.8rem;", "Top Contributors"),
              tags$div(sprintf("\U2022 OPS: %s (%.3f)",
                               top_ops_player, top_ops_val),
                       style = "margin-bottom: 1px;"),
              tags$div(sprintf("\U2022 AVG: %s (.%s)", top_avg_player,
                               format(round(max(batting$AVG,na.rm=TRUE)*1000))),
                       style = "margin-bottom: 1px;"),
              tags$div(sprintf("\U2022 Hits: %s (%d)", top_h_player, top_h_count),
                       style = "margin-bottom: 1px;"),
              tags$div(sprintf("\U2022 HR: %s (%d)", top_hr_player, top_hr_count),
                       style = "margin-bottom: 1px;")
            )
          ),

          # ── Main panel (fills remaining height) ───────────────────────
          div(
            style = "display: flex; flex-direction: column; height: 90%;",

          
            # Plot card (fills remaining space)
            div(
              style = "flex: 1; min-height: 350px;",
              card(
                full_screen = TRUE,
                style = "border: none; box-shadow: 0 1px 6px rgba(0,0,0,0.06);
                         background: #FAFAF7; height: 100%; min-height: 350px;
                         position: relative;",
                card_header(
                  class = "d-flex justify-content-between align-items-center",
                  style = "background: white; border-bottom: 2px solid #77121C;
                           padding: 6px 14px;",
                  textOutput("plot_title", inline = TRUE,
                             container = function(...) {
                               tags$span(..., style = "font-size: 0.85rem;
                                                       font-weight: 600;
                                                       color: #2C3E50;")
                             }),
                  textOutput("plot_subtitle", inline = TRUE,
                             container = function(...) {
                               tags$span(..., class = "text-muted",
                                        style = "font-size: 0.65rem;")
                             })
                ),
                  div(
                    class = "html-fill-container",
                    style = "flex: 1; min-height: 0; padding: 4px;",
                    plotlyOutput("main_plot", height = "100%")
                )
              )
            )
          )
        )
      )
    ),

    # ────────────────────────────────────────────────────────────────────
    # TAB 2: BATTING TABLE
    # ────────────────────────────────────────────────────────────────────
    nav_panel(
      title = "Batting Table",
      icon  = fontawesome::fa_i("table"),
      div(
        style = "padding: 12px 16px;
                 height: calc(100vh - 100px);",
        card(
          full_screen = TRUE,
          style = "border: none; box-shadow: 0 1px 6px rgba(0,0,0,0.06);",
          card_header("NBCS 2026 Regular Season — Full Batting Statistics"),
          gt_output("gt_batting_table")
        )
      )
    ),

    # ────────────────────────────────────────────────────────────────────
    # TAB 3: PITCHING TABLE
    # ────────────────────────────────────────────────────────────────────
    nav_panel(
      title = "Pitching Table",
      icon  = fontawesome::fa_i("table"),
      div(
        style = "padding: 12px 16px;
                 height: calc(100vh - 100px);",
        card(
          full_screen = TRUE,
          style = "border: none; box-shadow: 0 1px 6px rgba(0,0,0,0.06);",
          card_header("NBCS 2026 Regular Season — Pitching Statistics"),
          gt_output("gt_pitching_table")
        )
      )
    ),

    # ────────────────────────────────────────────────────────────────────
    # TAB 4: METHODOLOGY
    # ────────────────────────────────────────────────────────────────────
    nav_panel(
      title = "Methodology",
      icon  = fontawesome::fa_i("book"),
      div(
        style = "padding: 12px 16px;
                 height: calc(100vh - 100px); overflow-y: auto;",
        card(
          full_screen = TRUE,
          style = "border: none; box-shadow: 0 1px 6px rgba(0,0,0,0.06);",
          card_header("Statistical Definitions"),
          tags$div(class = "p-4",
            tags$h5("Rate Metrics"),
            tags$ul(
              tags$li(tags$strong("AVG"), " = H / AB — hit frequency"),
              tags$li(tags$strong("OBP"),
                      " = (H + BB + HBP) / (AB + BB + HBP + SF) — on-base"),
              tags$li(tags$strong("SLG"), " = TB / AB — power"),
              tags$li(tags$strong("OPS"), " = OBP + SLG — overall offence"),
              tags$li(tags$strong("XBH Rate"),
                      " = (2B + 3B + HR) / H — extra-base hit percentage")
            ),
            tags$h5("Counting Metrics"),
            tags$ul(
              tags$li(tags$strong("PA"), " — Plate Appearances"),
              tags$li(tags$strong("AB"), " — At-Bats"),
              tags$li(tags$strong("H"), " — Hits"),
              tags$li(tags$strong("1B / 2B / 3B / HR"),
                      " — Singles / Doubles / Triples / Home Runs"),
              tags$li(tags$strong("BB"), " — Walks"),
              tags$li(tags$strong("SO"), " — Strikeouts"),
              tags$li(tags$strong("TB"), " — Total Bases"),
              tags$li(tags$strong("XBH"), " — Extra-Base Hits")
            ),
            tags$h5("Pitching Metrics"),
            tags$ul(
              tags$li(tags$strong("IP"), " — Innings Pitched"),
              tags$li(tags$strong("ERA"), " = ER / IP × 9 — Earned Run Average"),
              tags$li(tags$strong("WHIP"), " = (BB + H) / IP — Walks + Hits per Inning"),
              tags$li(tags$strong("BAA"), " — Batting Average Against"),
              tags$li(tags$strong("K/BB"), " — Strikeout-to-Walk Ratio"),
              tags$li(tags$strong("K/BF"), " — Strikeouts per Batter Faced")
            ),
            tags$h5("Visualizations"),
            tags$ul(
              tags$li(tags$strong("Pitching Summary"),
                      " — tile heatmap of 11 stats with rate stat comparison"),
              tags$li(tags$strong("Offensive Trinity"),
                      " — OBP vs SLG bubble chart with AVG colour"),
              tags$li(tags$strong("PA Distribution"),
                      " — playing time volume with OBP colour"),
              tags$li(tags$strong("XBH Rate"),
                      " — extra-base hit proportion with SLG fill"),
              tags$li(tags$strong("Contact Quality"),
                      " — AVG vs SLG with linear trend, coloured by OBP")
            )
          )
        )
      )
    ),

    # ────────────────────────────────────────────────────────────────────
    # TAB 5: WORKFLOW
    # ────────────────────────────────────────────────────────────────────
    nav_panel(
      title = "Workflow",
      icon  = fontawesome::fa_i("gear"),
      div(
        style = "padding: 12px 16px;
                 height: calc(100vh - 100px); overflow-y: auto;",
        card(
          full_screen = TRUE,
          style = "border: none; box-shadow: 0 1px 6px rgba(0,0,0,0.06);",
          card_header("Pipeline & Workflow Documentation"),
          tags$div(class = "p-4",
            tags$h5("Data Source"),
            tags$p("Game statistics exported from GameChanger via the Nairobi Baseball Community. The raw CSV contains batting, pitching, and fielding data for all Thika Rangers players across the NBCS 2026 Regular Season."),
            tags$p("File: ", tags$code("tr-stats26.csv"), " — 24 player rows + Team totals row."),

            tags$h5("Reproducible Pipeline"),
            tags$p("The analysis is driven by a ", tags$code("{targets}"), " pipeline with 7 steps:"),
            tags$ol(
              tags$li(tags$code("raw_data_path"), " — track the source CSV file"),
              tags$li(tags$code("batting_data"), " — parse and clean batting statistics"),
              tags$li(tags$code("team_summary"), " — compute aggregate team offensive metrics"),
              tags$li(tags$code("plots"), " — generate all 12 ggplot2/patchwork visualisations, rendered interactively in the dashboard via ", tags$code("{plotly}")),
              tags$li(tags$code("gt_table"), " — build interactive GT batting table"),
              tags$li(tags$code("save_plots"), " — export all plots as high-res PNGs"),
              tags$li(tags$code("export_data"), " — serialise outputs as RDS files")
            ),
            tags$p("Run: ", tags$code("targets::tar_make()")),

            tags$h5("Visualisations (12 total)"),
            tags$ol(
              tags$li(tags$strong("OPS Leaderboard"), " — ranked lollipop of On-Base Plus Slugging"),
              tags$li(tags$strong("Offensive Profile Matrix"), " — faceted lollipop of AVG, OBP, SLG, OPS"),
              tags$li(tags$strong("OBP vs SLG Quadrant"), " — quadrant analysis with sweet-spot zones"),
              tags$li(tags$strong("Offensive Trinity"), " — OBP vs SLG bubble chart, coloured by AVG"),
              tags$li(tags$strong("Pitching Summary"), " — tile heatmap + rate stat comparison for 4 pitchers"),
              tags$li(tags$strong("Run Production"), " — runs vs RBI scatter sized by hits"),
              tags$li(tags$strong("Hit Type Composition"), " — stacked proportion bar of 1B/2B/3B/HR"),
              tags$li(tags$strong("Plate Discipline"), " — walks vs strikeouts with OBP colour"),
              tags$li(tags$strong("Counting Stats Heatmap"), " — tile heatmap of all batting counts"),
              tags$li(tags$strong("PA Distribution"), " — playing time volume, coloured by OBP"),
              tags$li(tags$strong("Extra-Base Hit Rate"), " — XBH % of total hits, coloured by SLG"),
              tags$li(tags$strong("Contact Quality"), " — AVG vs SLG with linear regression trend")
            ),

            tags$h5("Colour Palette"),
            tags$p("Team colours extracted from the Thika Rangers logo via k-means clustering on the RGB cube using ", tags$code("{imager}"), ". Six cluster centres are computed; the darkest, most saturated, and lightest are mapped to primary, accent, and secondary. Neutral tones (slate, ivory, sage) are added for readability."),

            tags$h5("Shiny Dashboard"),
            tags$p("Built with ", tags$code("{bslib}"), " (Bootstrap 5). Layout: header bar + tab navigation + sidebar (plot selector + team snapshot) + main panel (value boxes + plot card). All visualizations are interactive using ", tags$code("{plotly}"), " — hover tooltips, zoom, pan, and auto-resize in full-screen mode."),

            tags$h5("R Packages"),
            tags$p("pipeline: ", tags$code("targets"), ". analysis: ", tags$code("dplyr"), ", ", tags$code("tidyr"), ", ", tags$code("ggplot2"), ", ", tags$code("scales"), ", ", tags$code("imager"), ". tables: ", tags$code("gt"), ", ", tags$code("DT"), ". interactivity: ", tags$code("plotly"), ". layout: ", tags$code("patchwork"), ", ", tags$code("bslib"), ", ", tags$code("fontawesome"), ". rendering: ", tags$code("quarto"), " (report.qmd)."),

            tags$h5("Reproducibility"),
            tags$p("1. ", tags$code("targets::tar_make()"), " — execute the pipeline from the project root."),
            tags$p("2. ", tags$code("shiny::runApp('shiny/app.R')"), " — launch the interactive dashboard (ggplot2 plots auto-converted to interactive plotly widgets)."),
            tags$p("3. ", tags$code("quarto render report.qmd"), " — generate the static HTML/PDF report."),

            tags$hr(),
            tags$p(style = "font-size: 0.85rem; color: #8B8B8B;",
                   "Analysis by Keith Karani  ·  Data: Nairobi Baseball Community  ·  NBCS 2026 Regular Season")
          )
        )
      )
    ),

    # ────────────────────────────────────────────────────────────────────
    # TAB 6: DISCUSSION
    # ────────────────────────────────────────────────────────────────────
    nav_panel(
      title = "Discussion",
      icon  = fontawesome::fa_i("comments"),
      div(
        style = "padding: 12px 16px;
                 height: calc(100vh - 100px); overflow-y: auto;",
        card(
          full_screen = TRUE,
          style = "border: none; box-shadow: 0 1px 6px rgba(0,0,0,0.06);",
          card_header("Performance Discussion — Batting  ·  Pitching  ·  Fielding"),
          uiOutput("discussion")
        )
      )
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  selected_plot <- reactive({
    req(input$plot_selector)
    plots[[input$plot_selector]]
  })

  output$plot_title <- renderText({
    req(input$plot_selector)
    plot_info[[input$plot_selector]][1]
  })

  output$plot_subtitle <- renderText({
    req(input$plot_selector)
    plot_info[[input$plot_selector]][2]
  })

  output$main_plot <- renderPlotly({
    ggplotly(selected_plot(), height = 500) %>%
      layout(margin = list(l = 50, r = 20, t = 20, b = 50),
             autosize = TRUE)
  })

  output$gt_batting_table <- render_gt(gt_tab)
  output$gt_pitching_table <- render_gt(pitching_gt)
  output$discussion <- renderUI(discussion_html)
}

shinyApp(ui, server)
