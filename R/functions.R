###############################################################################
#  functions.R — Data processing, analysis, plotting, and gt table generation
#               for Thika Rangers Baseball Club batting statistics.
###############################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(gt)
# ═══════════════════════════════════════════════════════════════════════════
# 1. DATA INGESTION
# ═══════════════════════════════════════════════════════════════════════════

read_batting_data <- function(path = "tr-stats26.csv") {
  raw <- read.csv(path, header = FALSE, stringsAsFactors = FALSE,
                  na.strings = c("", " ", ".000"), strip.white = TRUE)

  colnames(raw) <- as.character(raw[2, ])
  players_raw <- raw[3:26, ]

  keep <- c("Number", "Last", "First", "GP", "PA", "AB", "AVG", "OBP", "SLG",
            "OPS", "H", "1B", "2B", "3B", "HR", "RBI", "R", "BB", "SO", "HBP",
            "TB", "XBH", "SF", "SAC")
  batting <- players_raw[, intersect(keep, colnames(players_raw))]

  numeric_cols <- setdiff(names(batting), c("Number", "Last", "First"))
  batting[numeric_cols] <- lapply(batting[numeric_cols], function(x) {
    x <- gsub("^\\s*$", NA, x)
    as.numeric(x)
  })

  batting <- batting %>% filter(PA > 0 & !is.na(PA))
  batting$Player <- paste(batting$First, batting$Last)
  batting$Player <- factor(batting$Player, levels = batting$Player)

  batting
}

# ═══════════════════════════════════════════════════════════════════════════
# 2. TEAM SUMMARY
# ═══════════════════════════════════════════════════════════════════════════

compute_team_summary <- function(batting) {
  list(
    n_players   = nrow(batting),
    total_PA    = sum(batting$PA, na.rm = TRUE),
    total_AB    = sum(batting$AB, na.rm = TRUE),
    total_H     = sum(batting$H, na.rm = TRUE),
    total_BB    = sum(batting$BB, na.rm = TRUE),
    total_SO    = sum(batting$SO, na.rm = TRUE),
    total_HBP   = sum(batting$HBP, na.rm = TRUE),
    total_TB    = sum(batting$TB, na.rm = TRUE),
    total_R     = sum(batting$R, na.rm = TRUE),
    total_RBI   = sum(batting$RBI, na.rm = TRUE),
    total_HR    = sum(batting$HR, na.rm = TRUE),
    total_XBH   = sum(batting$XBH, na.rm = TRUE),
    team_AVG    = round(sum(batting$H, na.rm = TRUE) / sum(batting$AB, na.rm = TRUE), 3),
    team_OBP    = round(
      (sum(batting$H, na.rm = TRUE) + sum(batting$BB, na.rm = TRUE) + sum(batting$HBP, na.rm = TRUE)) /
        (sum(batting$AB, na.rm = TRUE) + sum(batting$BB, na.rm = TRUE) + sum(batting$HBP, na.rm = TRUE)), 3),
    team_SLG    = round(sum(batting$TB, na.rm = TRUE) / sum(batting$AB, na.rm = TRUE), 3)
  ) %>% within({
    team_OPS <- round(team_OBP + team_SLG, 3)
  })
}

# ═══════════════════════════════════════════════════════════════════════════
# 3. PLOTTING FUNCTIONS
# ═══════════════════════════════════════════════════════════════════════════

# -- Helper for player labels (uses ggrepel if available) --

player_label_layer <- function(data, ...) {
  has_repel <- requireNamespace("ggrepel", quietly = TRUE)
  args <- list(mapping = aes(label = Player), size = 3, ...)
  if (has_repel) {
    args$max.overlaps <- 12
    args$box.padding  <- 0.35
    args$point.padding <- 0.3
    args$segment.color <- "grey60"
    args$segment.size  <- 0.3
    do.call(ggrepel::geom_text_repel, args)
  } else {
    args$vjust    <- -1.2
    args$check_overlap <- TRUE
    do.call(geom_text, args)
  }
}

# ── Figure 1: Offensive Profile Matrix ─────────────────────────────────────

plot_offensive_matrix <- function(batting) {
  rate_long <- batting %>%
    select(Player, AVG, OBP, SLG, OPS) %>%
    pivot_longer(-Player, names_to = "Metric", values_to = "Value") %>%
    mutate(Metric = factor(Metric, levels = c("AVG", "OBP", "SLG", "OPS")))

  team_avg <- rate_long %>%
    group_by(Metric) %>%
    summarise(TeamVal = mean(Value, na.rm = TRUE), .groups = "drop")

  ggplot(rate_long, aes(x = Value, y = Player, color = Metric)) +
    geom_segment(aes(xend = 0, yend = Player), linewidth = 0.6, alpha = 0.35) +
    geom_point(size = 3.5) +
    geom_vline(data = team_avg, aes(xintercept = TeamVal, color = Metric),
               linewidth = 0.8, linetype = "dashed", alpha = 0.7) +
    facet_wrap(~ Metric, ncol = 4, scales = "free_x") +
    scale_color_manual(values = c(AVG = team_palette$secondary,
                                  OBP = team_palette$info,
                                  SLG = team_palette$success,
                                  OPS = "#9B59B6")) +
    scale_x_continuous(labels = label_number(accuracy = 0.001)) +
    labs(subtitle = "Four-facet lollipop chart of core rate statistics  ·  Dashed line = team mean",
         x = NULL, y = NULL,
         caption = "Data: Nairobi Baseball Community  |  NBCS 2026 Regular Season") +
    theme_thika() +
    theme(strip.text = element_text(face = "bold", size = rel(1)),
          legend.position = "none",
          axis.text.y = element_text(size = rel(0.7)))
}

# ── Figure 2: OBP–SLG Quadrant ─────────────────────────────────────────────

plot_obp_slg_quadrant <- function(batting) {
  med_obp <- median(batting$OBP, na.rm = TRUE)
  med_slg <- median(batting$SLG, na.rm = TRUE)

  ggplot(batting, aes(x = OBP, y = SLG)) +
    annotate("rect", xmin = -Inf, xmax = med_obp, ymin = -Inf, ymax = med_slg,
             fill = team_palette$danger, alpha = 0.06) +
    annotate("rect", xmin = med_obp, xmax = Inf, ymin = -Inf, ymax = med_slg,
             fill = team_palette$warning, alpha = 0.06) +
    annotate("rect", xmin = -Inf, xmax = med_obp, ymin = med_slg, ymax = Inf,
             fill = team_palette$info, alpha = 0.06) +
    annotate("rect", xmin = med_obp, xmax = Inf, ymin = med_slg, ymax = Inf,
             fill = team_palette$success, alpha = 0.06) +
    annotate("text", x = 0.22, y = 0.90,
             label = "High SLG · Low OBP\n(Disciplined Power?)",
             size = 2.8, color = team_palette$info, fontface = "italic", alpha = 0.7) +
    annotate("text", x = 0.88, y = 0.90,
             label = "Sweet Spot\nElite Hitters",
             size = 3.2, color = team_palette$success, fontface = "bold", alpha = 0.8) +
    annotate("text", x = 0.22, y = 0.20,
             label = "Needs Work:\nLow OBP · Low Power",
             size = 2.8, color = team_palette$danger, fontface = "italic", alpha = 0.7) +
    annotate("text", x = 0.88, y = 0.20,
             label = "High OBP · Low Power\n(Table Setters)",
             size = 2.8, color = team_palette$warning, fontface = "italic", alpha = 0.7) +
    geom_vline(xintercept = med_obp, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    geom_hline(yintercept = med_slg, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    geom_point(aes(size = PA, fill = OPS), shape = 21,
               color = "grey20", stroke = 0.8, alpha = 0.9) +
    player_label_layer(batting) +
    scale_fill_viridis_c(option = "D", direction = 1) +
    scale_size_continuous(range = c(3, 12), name = "Plate Appearances") +
    scale_x_continuous(labels = label_number(accuracy = 0.001)) +
    scale_y_continuous(labels = label_number(accuracy = 0.001)) +
    labs(subtitle = sprintf("Dashed lines = team median (OBP=%.3f, SLG=%.3f).  Dot size = PA, colour = OPS.",
                            med_obp, med_slg),
         x = "On-Base Percentage (OBP)",
         y = "Slugging Percentage (SLG)",
         caption = "Data: Nairobi Baseball Community  |  NBCS 2026 Regular Season") +
    guides(fill = guide_colorbar(title = "OPS", barwidth = 10, barheight = 0.5)) +
    theme_thika() +
    theme(legend.box = "horizontal")
}

# ── Figure 3: Hit Composition ───────────────────────────────────────────────

plot_hit_composition <- function(batting) {
  hit_comp <- batting %>%
    filter(H > 0) %>%
    select(Player, `1B`, `2B`, `3B`, `HR`) %>%
    pivot_longer(-Player, names_to = "HitType", values_to = "Count") %>%
    mutate(HitType = factor(HitType, levels = c("HR", "3B", "2B", "1B")))

  ggplot(hit_comp, aes(x = Player, y = Count, fill = HitType)) +
    geom_col(position = "fill", width = 0.75, color = "white", linewidth = 0.3) +
    scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
    scale_fill_manual(
      values = c("1B" = team_palette$success,
                 "2B" = team_palette$info,
                 "3B" = "#9B59B6",
                 "HR" = team_palette$secondary),
      labels = c("HR", "3B", "2B", "1B")) +
    labs(subtitle = "Proportion of singles, doubles, triples & home runs among total hits",
         x = NULL, y = "Proportion of Hits",
         fill = "Hit Type",
         caption = "Players with zero hits excluded") +
    theme_thika() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1, size = rel(0.7)),
          panel.grid.major.x = element_blank())
}

# ── Figure 4: Plate Discipline ──────────────────────────────────────────────

plot_plate_discipline <- function(batting) {
  ggplot(batting, aes(x = BB, y = SO)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted",
                color = "grey60", linewidth = 0.6) +
    annotate("text", x = max(batting$BB, na.rm = TRUE) * 0.7,
             y = max(batting$SO, na.rm = TRUE) * 0.25,
             label = "More BB than SO", angle = 40, size = 3,
             color = "grey50", fontface = "italic") +
    annotate("text", x = max(batting$BB, na.rm = TRUE) * 0.25,
             y = max(batting$SO, na.rm = TRUE) * 0.7,
             label = "More SO than BB", angle = 40, size = 3,
             color = "grey50", fontface = "italic") +
    geom_point(aes(size = PA, fill = OBP), shape = 21,
               color = "grey20", stroke = 0.8, alpha = 0.85) +
    player_label_layer(batting) +
    scale_fill_viridis_c(option = "A", direction = 1) +
    scale_size_continuous(range = c(2, 11)) +
    labs(subtitle = "Above dotted line = more SO than BB  ·  Colour = OBP, size = PA",
         x = "Walks (BB)", y = "Strikeouts (SO)",
         caption = "Data: Nairobi Baseball Community  |  NBCS 2026 Regular Season") +
    guides(fill = guide_colorbar(title = "OBP", barwidth = 8, barheight = 0.4)) +
    theme_thika()
}

# ── Figure 5: Counting Stats Heatmap ────────────────────────────────────────

plot_counts_heatmap <- function(batting) {
  counts_long <- batting %>%
    select(Player, PA, AB, H, `1B`, `2B`, `3B`, HR, BB, SO, HBP, TB, RBI, R) %>%
    pivot_longer(-Player, names_to = "Stat", values_to = "Count") %>%
    mutate(Stat = factor(Stat, levels = c("PA", "AB", "H", "1B", "2B", "3B",
                                          "HR", "BB", "SO", "HBP", "TB", "RBI", "R")))

  ggplot(counts_long, aes(x = Stat, y = Player)) +
    geom_tile(aes(fill = Count), color = "white", linewidth = 1.2) +
    geom_text(aes(label = Count), size = 2.8, fontface = "bold",
              color = "grey20") +
    scale_fill_gradientn(
      colours = c("#FEF0D9", "#FDCC8A", "#FC8D59",
                  team_palette$secondary, team_palette$primary),
      na.value = "grey95") +
    labs(subtitle = "Darker tiles = higher counts  ·  Comprehensive view of all counting statistics",
         x = NULL, y = NULL,
         caption = "Data: Nairobi Baseball Community  |  NBCS 2026 Regular Season") +
    theme_thika(base_size = 10) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 0.5,
                                     size = rel(0.8), face = "bold"),
          axis.text.y = element_text(size = rel(0.7)))
}

# ── Figure 6: OPS Leaderboard ───────────────────────────────────────────────

plot_ops_leaderboard <- function(batting) {
  batting <- batting %>% arrange(desc(OPS)) %>%
    mutate(Player = factor(Player, levels = Player))

  ggplot(batting, aes(x = OPS, y = Player)) +
    geom_segment(aes(xend = 0, yend = Player), color = "grey75", linewidth = 0.5) +
    geom_point(aes(color = OPS), size = 5) +
    geom_text(aes(label = sprintf("%.3f", OPS)),
              hjust = -0.3, size = 3.3, fontface = "bold", color = "grey30") +
    scale_color_gradient(low = team_palette$warning, high = team_palette$secondary) +
    scale_x_continuous(limits = c(0, max(batting$OPS, na.rm = TRUE) * 1.2),
                       labels = label_number(accuracy = 0.001)) +
    labs(subtitle = "On-Base Plus Slugging: the gold-standard measure of overall offensive output",
         x = "OPS", y = NULL,
         caption = "Data: Nairobi Baseball Community  |  NBCS 2026 Regular Season") +
    theme_thika() +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = rel(0.8)))
}

# ── Figure 7: Run Production ────────────────────────────────────────────────

plot_run_production <- function(batting) {
  ggplot(batting, aes(x = R, y = RBI)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted",
                color = "grey60", linewidth = 0.5) +
    geom_point(aes(size = H, fill = OPS), shape = 21,
               color = "grey20", stroke = 0.7, alpha = 0.85) +
    player_label_layer(batting) +
    scale_fill_viridis_c(option = "E", direction = -1) +
    scale_size_continuous(range = c(2, 10)) +
    labs(subtitle = "Runs Scored vs Runs Batted In  ·  Size = hits, colour = OPS  ·  Above dotted = more RBI than R",
         x = "Runs Scored (R)", y = "Runs Batted In (RBI)",
         caption = "Data: Nairobi Baseball Community  |  NBCS 2026 Regular Season") +
    guides(fill = guide_colorbar(title = "OPS", barwidth = 8, barheight = 0.4)) +
    theme_thika()
}

# ═══════════════════════════════════════════════════════════════════════════
# 4A. Pitching Summary — tile heatmap for Isaiah, Tuju, Derrick, Caleb
# ═══════════════════════════════════════════════════════════════════════════

read_pitching_data <- function(path = "tr-stats26.csv") {
  raw <- read.csv(path, header = FALSE, stringsAsFactors = FALSE,
                  strip.white = TRUE)

  hdr <- as.character(raw[2, ])

  # Pitching starts at column "IP" (first occurrence; appears only once)
  ip_idx <- which(hdr == "IP")

  pitch_indices <- c(
    1, 2, 3,           # Number, Last, First
    ip_idx,             # 4:  IP
    ip_idx + 1,         # 5:  GP (games pitched)
    ip_idx + 2,         # 6:  GS (games started)
    ip_idx + 5,         # 7:  W
    ip_idx + 6,         # 8:  L
    ip_idx + 7,         # 9:  SV
    ip_idx + 11,        # 10: H (hits allowed)
    ip_idx + 12,        # 11: R (runs allowed)
    ip_idx + 13,        # 12: ER (earned runs)
    ip_idx + 14,        # 13: BB (walks)
    ip_idx + 15,        # 14: SO (strikeouts)
    ip_idx + 18,        # 15: ERA
    ip_idx + 19,        # 16: WHIP
    ip_idx + 27,        # 17: BAA
    ip_idx + 47,        # 18: K/BF
    ip_idx + 48         # 19: K/BB
  )

  pitch_names <- c("Number", "Last", "First", "IP", "GP", "GS",
                   "W", "L", "SV",
                   "H", "R", "ER", "BB", "SO",
                   "ERA", "WHIP", "BAA", "K_BF", "K_BB")

  players_raw <- raw[3:26, ]
  pd <- players_raw[, pitch_indices]
  colnames(pd) <- pitch_names

  num_cols <- setdiff(pitch_names, c("Number", "Last", "First"))
  pd[num_cols] <- lapply(pd[num_cols], function(x) {
    x <- gsub("^\\s*$", NA, x); as.numeric(x)
  })

  pd <- pd %>% filter(IP > 0 | !is.na(IP))
  pd$Player <- paste(pd$First, pd$Last)
  pd
}

# ═══════════════════════════════════════════════════════════════════════════
# 5. FIELDING DATA
# ═══════════════════════════════════════════════════════════════════════════

read_fielding_data <- function(path = "tr-stats26.csv") {
  raw <- read.csv(path, header = FALSE, stringsAsFactors = FALSE,
                  strip.white = TRUE)
  hdr <- as.character(raw[2, ])

  # Fielding starts at column "TC" (Total Chances); first non-empty after col 155
  tc_idx <- which(hdr == "TC")

  field_indices <- c(
    1, 2, 3,           # Number, Last, First
    tc_idx,             # 4:  TC (Total Chances)
    tc_idx + 1,         # 5:  A (Assists)
    tc_idx + 2,         # 6:  PO (Putouts)
    tc_idx + 3,         # 7:  FPCT (Fielding Percentage)
    tc_idx + 4,         # 8:  E (Errors)
    tc_idx + 5,         # 9:  DP (Double Plays)
    tc_idx + 7,         # 10: C (Catcher appearances)
    tc_idx + 8,         # 11: PB (Passed Balls)
    tc_idx + 9,         # 12: SB (Stolen Bases allowed)
    tc_idx + 10,        # 13: SBATT (Stolen Base Attempts)
    tc_idx + 11,        # 14: CS (Caught Stealing)
    tc_idx + 12         # 15: CS% (Caught Stealing %)
  )

  field_names <- c("Number", "Last", "First",
                   "TC", "A", "PO", "FPCT", "E", "DP",
                   "C", "PB", "SB", "SBATT", "CS", "CS_pct")

  players_raw <- raw[3:26, ]
  fd <- players_raw[, field_indices]
  colnames(fd) <- field_names

  num_cols <- setdiff(field_names, c("Number", "Last", "First"))
  fd[num_cols] <- lapply(fd[num_cols], function(x) {
    x <- gsub("^\\s*$", NA, x); as.numeric(x)
  })

  fd <- fd %>% filter(TC > 0 & !is.na(TC))
  fd$Player <- paste(fd$First, fd$Last)
  fd
}

plot_pitching_summary <- function(path = "tr-stats26.csv") {
  pd <- read_pitching_data(path)

  target <- c("Maina Isaiah", "Tuju Oduori", "Derrick Mwendwa", "Caleb Ochego")
  pd <- pd %>% filter(Player %in% target)
  if (nrow(pd) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5,
      label = "No pitching data found for the selected pitchers") +
      theme_void())
  }

  pd <- pd %>% arrange(desc(IP)) %>%
    mutate(Player = factor(Player, levels = Player))

  pd_tile <- pd %>%
    select(Player, IP, H, ER, BB, SO, K_BB, W, L, ERA, WHIP, BAA) %>%
    pivot_longer(-Player, names_to = "Stat", values_to = "Value")

  stat_labels <- c(
    IP   = "IP",    H   = "H",   ER  = "ER",  BB  = "BB",
    SO   = "SO",    K_BB= "K/BB", W   = "W",   L   = "L",
    ERA  = "ERA",   WHIP= "WHIP", BAA = "BAA"
  )
  stat_groups <- c(
    IP="Volume", H="Volume", ER="Volume", BB="Control",
    SO="Control", K_BB="Control", W="Results", L="Results",
    ERA="Results", WHIP="Results", BAA="Results"
  )
  lower_better <- c("H", "ER", "BB", "L", "ERA", "WHIP", "BAA")

  pd_tile <- pd_tile %>%
    mutate(Stat = recode(Stat, !!!stat_labels),
           Stat = factor(Stat, levels = rev(stat_labels)),
           Group = stat_groups[as.character(Stat)])

  # Compute within-group rank for colour (1 = best, 4 = worst)
  pd_tile <- pd_tile %>%
    group_by(Stat) %>%
    mutate(Rank = if (unique(Stat) %in% lower_better)
      rank(Value, na.last = "keep") else rank(desc(Value), na.last = "keep")) %>%
    ungroup()

  # Panel A: Tile heatmap
  p_tile <- ggplot(pd_tile, aes(x = Stat, y = Player)) +
    geom_tile(aes(fill = Rank), color = "white", linewidth = 2) +
    geom_text(aes(label = ifelse(Stat == "K/BB",
      sprintf("%.2f", Value), sprintf("%.1f", Value))),
      size = 3.5, fontface = "bold", color = "grey20") +
    scale_fill_gradientn(colours = c("#4A8C6F", "#F5F0EB", "#A04040"),
      values = scales::rescale(c(1, 2.5, 4)),
      na.value = "grey90", guide = "none") +
    labs(subtitle = "Maina Isaiah · Tuju Oduori · Derrick Mwendwa · Caleb Ochego  —  tile colour = rank among four (green = best)",
         x = NULL, y = NULL,
         caption = "Data: Nairobi Baseball Community  |  NBCS 2026 Regular Season") +
    theme_thika(base_size = 11) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 0.5,
                                     size = rel(0.85), face = "bold"),
          axis.text.y = element_text(size = rel(0.9), face = "bold"),
          plot.margin = margin(4, 8, 4, 4))

  if (!requireNamespace("patchwork", quietly = TRUE)) return(p_tile)

  # Panel B: Key rate stats comparison (ERA, WHIP, K/BB)
  rate_stats <- pd %>%
    select(Player, ERA, WHIP, K_BB) %>%
    pivot_longer(-Player, names_to = "Stat", values_to = "Value") %>%
    mutate(Stat = recode(Stat, ERA = "ERA", WHIP = "WHIP", K_BB = "K/BB"),
           Player = factor(Player, levels = rev(levels(pd$Player))))

  p_rates <- ggplot(rate_stats, aes(x = Value, y = Stat)) +
    geom_point(aes(fill = Player), shape = 21, size = 5,
               color = "grey30", stroke = 0.8) +
    geom_segment(aes(xend = 0, yend = Stat, color = Player),
                 linewidth = 0.6, alpha = 0.4) +
    scale_fill_manual(values = c(
      "Maina Isaiah"         = team_palette$primary,
      "Tuju Oduori"          = team_palette$secondary,
      "Derrick Mwendwa"      = team_palette$info,
      "Caleb Ochego"         = team_palette$accent
    )) +
    scale_color_manual(values = c(
      "Maina Isaiah"         = team_palette$primary,
      "Tuju Oduori"          = team_palette$secondary,
      "Derrick Mwendwa"      = team_palette$info,
      "Caleb Ochego"         = team_palette$accent
    ), guide = "none") +
    labs(subtitle = "Lower is better for ERA and WHIP; higher is better for K/BB",
         x = NULL, y = NULL, fill = "Pitcher") +
    theme_thika(base_size = 10) +
    theme(plot.subtitle = element_text(size = rel(0.6)),
          axis.text.y   = element_text(size = rel(0.9), face = "bold"),
          axis.text.x   = element_text(size = rel(0.7)),
          legend.text   = element_text(size = rel(0.6)),
          legend.title  = element_text(size = rel(0.65)),
          plot.margin   = margin(4, 4, 4, 4))

  library(patchwork)
  design <- "AAAAAAA
             AAAAAAA
             BBBBBBB
             BBBBBBB
             BBBBBBB"
  p_tile + p_rates + plot_layout(design = design, heights = c(2, 3))
}

# ═══════════════════════════════════════════════════════════════════════════
# 4B. Offensive Trinity Bubble (OBP vs SLG + AVG + PA)
# ═══════════════════════════════════════════════════════════════════════════

plot_offensive_trinity <- function(batting) {
  ggplot(batting, aes(x = OBP, y = SLG, size = PA, fill = AVG)) +
    geom_point(shape = 21, color = "grey20", stroke = 0.8, alpha = 0.85) +
    player_label_layer(batting) +
    scale_size_continuous(range = c(4, 16)) +
    scale_fill_viridis_c(option = "D", direction = 1) +
    labs(subtitle = "Bubble size = Plate Appearances, colour = Batting Average",
         x = "On-Base Percentage", y = "Slugging Percentage",
         fill = "AVG", size = "PA",
         caption = "Data: Nairobi Baseball Community  |  NBCS 2026 Regular Season") +
    theme_thika()
}

# ═══════════════════════════════════════════════════════════════════════════
# 4C. PA Distribution — playing time breakdown
# ═══════════════════════════════════════════════════════════════════════════

plot_pa_distribution <- function(batting) {
  df <- batting %>%
    mutate(Player = reorder(Player, PA))

  ggplot(df, aes(x = PA, y = Player)) +
    geom_segment(aes(xend = 0, yend = Player, color = PA),
                 linewidth = 0.7, alpha = 0.5) +
    geom_point(aes(fill = OBP), shape = 21, size = 4,
               color = "grey30", stroke = 0.7) +
    geom_text(aes(label = PA), hjust = -0.5, size = 3,
              color = "grey40", fontface = "bold") +
    scale_fill_viridis_c(option = "D", direction = 1) +
    scale_color_gradient(low = team_palette$light, high = team_palette$primary,
                         guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(subtitle = "Playing time volume — bar length = PA, colour = OBP",
         x = "Plate Appearances", y = NULL,
         fill = "OBP",
         caption = "Data: Nairobi Baseball Community  |  NBCS 2026 Regular Season") +
    guides(fill = guide_colorbar(title = "OBP", barwidth = 8, barheight = 0.4)) +
    theme_thika() +
    theme(panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = rel(0.75)))
}

# ═══════════════════════════════════════════════════════════════════════════
# 4D. Extra-Base Hit Rate — power assessment
# ═══════════════════════════════════════════════════════════════════════════

plot_xbh_rate <- function(batting) {
  df <- batting %>%
    filter(H > 0) %>%
    mutate(xbh_rate = XBH / H,
           singles   = H - XBH,
           Player   = reorder(Player, xbh_rate)) %>%
    select(Player, xbh_rate, XBH, H, SLG)

  ggplot(df, aes(x = xbh_rate, y = Player)) +
    geom_segment(aes(xend = 0, yend = Player, color = xbh_rate),
                 linewidth = 0.7, alpha = 0.4) +
    geom_point(aes(fill = SLG), shape = 21, size = 4.5,
               color = "grey30", stroke = 0.7) +
    geom_text(aes(label = sprintf("%.0f%%", xbh_rate * 100)),
              hjust = -0.4, size = 3, fontface = "bold", color = "grey30") +
    scale_fill_gradientn(colours = maroon_fade(7)) +
    scale_color_gradient(low = team_palette$light,
                         high = team_palette$primary, guide = "none") +
    scale_x_continuous(labels = label_percent(),
                       expand = expansion(mult = c(0, 0.25))) +
    labs(subtitle = "% of hits that went for extra bases (2B + 3B + HR)  ·  colour = SLG",
         x = "XBH Rate (% of Hits)", y = NULL,
         fill = "SLG",
         caption = "Data: GameChanger  |  Players with zero hits excluded") +
    guides(fill = guide_colorbar(title = "SLG", barwidth = 8, barheight = 0.4)) +
    theme_thika() +
    theme(panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = rel(0.75)))
}

# ═══════════════════════════════════════════════════════════════════════════
# 4E. Contact Quality — AVG vs SLG relationship
# ═══════════════════════════════════════════════════════════════════════════

plot_contact_quality <- function(batting) {
  ggplot(batting, aes(x = AVG, y = SLG)) +
    geom_smooth(method = "lm", se = TRUE, color = team_palette$primary,
                fill = "#F0E6D6", alpha = 0.3, linewidth = 0.7) +
    geom_point(aes(size = H, fill = OBP), shape = 21,
               color = "grey20", stroke = 0.7, alpha = 0.85) +
    player_label_layer(batting) +
    scale_fill_viridis_c(option = "D", direction = 1) +
    scale_size_continuous(range = c(2, 10)) +
    scale_x_continuous(labels = label_number(accuracy = 0.001)) +
    scale_y_continuous(labels = label_number(accuracy = 0.001)) +
    labs(subtitle = "Should a player's batting average translate to power?  ·  Size = hits, colour = OBP",
         x = "Batting Average (AVG)", y = "Slugging Percentage (SLG)",
         fill = "OBP", size = "Hits",
         caption = "Data: Nairobi Baseball Community  |  NBCS 2026 Regular Season") +
    guides(fill = guide_colorbar(title = "OBP", barwidth = 8, barheight = 0.4)) +
    theme_thika()
}

# ═══════════════════════════════════════════════════════════════════════════
# 5. GT TABLE
# ═══════════════════════════════════════════════════════════════════════════

build_gt_batting_table <- function(batting, team_summary) {
  batting %>%
    arrange(desc(OPS)) %>%
    select(Player, GP, PA, AB, H, `1B`, `2B`, `3B`, HR,
           BB, SO, AVG, OBP, SLG, OPS) %>%
    gt() %>%
    tab_header(
      title    = md("**NBCS 2026 — Thika Rangers Batting Statistics**"),
      subtitle = md("NBCS 2026 Regular Season")
    ) %>%
    fmt_number(columns = c(GP, PA, AB, H, `1B`, `2B`, `3B`, HR, BB, SO),
               decimals = 0) %>%
    fmt_number(columns = c(AVG, OBP, SLG, OPS), decimals = 3) %>%
    data_color(
      columns    = c(AVG, OBP, SLG, OPS),
      method     = "numeric",
      palette    = c(team_palette$light, team_palette$accent,
                     team_palette$secondary, team_palette$primary),
      domain     = c(0, 1.8)
    ) %>%
    cols_label(
      Player = "Player", GP = "GP", PA = "PA", AB = "AB",
      H = "H", `1B` = "1B", `2B` = "2B", `3B` = "3B", HR = "HR",
      BB = "BB", SO = "SO",
      AVG = "AVG", OBP = "OBP", SLG = "SLG", OPS = "OPS"
    ) %>%
    tab_spanner(label = "Counting Stats", columns = c(GP, PA, AB, H, `1B`, `2B`, `3B`, HR, BB, SO)) %>%
    tab_spanner(label = "Rate Stats", columns = c(AVG, OBP, SLG, OPS)) %>%
    tab_footnote(
      footnote = md(paste0(
        "Team totals — AVG: ", team_summary$team_AVG,
        ", OBP: ", team_summary$team_OBP,
        ", SLG: ", team_summary$team_SLG,
        ", OPS: ", team_summary$team_OPS
      )),
      locations = cells_column_spanners(spanners = "Rate Stats")
    ) %>%
    tab_source_note(source_note = "Data: Nairobi Baseball Community") %>%
    opt_interactive(use_page_size_select = TRUE, page_size_default = 10) %>%
    opt_row_striping() %>%
    tab_options(
      heading.title.font.size  = "22px",
      heading.subtitle.font.size = "14px",
      table.font.size          = "13px",
      source_notes.font.size   = "10px"
    )
}

# ═══════════════════════════════════════════════════════════════════════════
# 6. GT PITCHING TABLE
# ═══════════════════════════════════════════════════════════════════════════

build_gt_pitching_table <- function(path = "tr-stats26.csv") {
  pd <- read_pitching_data(path)
  pd <- pd %>% filter(IP > 0 | (!is.na(IP) & IP > 0))

  pd <- pd %>%
    mutate(W_L = paste0(W, "-", L)) %>%
    arrange(desc(IP)) %>%
    select(Player, GP, GS, IP, W, L, SV, H, R, ER, BB, SO, ERA, WHIP, BAA, K_BB)

  pd %>%
    gt() %>%
    tab_header(
      title    = md("**NBCS 2026 — Thika Rangers Pitching Statistics**"),
      subtitle = md("NBCS 2026 Regular Season")
    ) %>%
    fmt_number(columns = c(GP, GS, IP, W, L, SV, H, R, ER, BB, SO),
               decimals = 1) %>%
    fmt_number(columns = c(ERA, WHIP), decimals = 2) %>%
    fmt_number(columns = c(BAA), decimals = 3) %>%
    fmt_number(columns = c(K_BB), decimals = 2) %>%
    data_color(
      columns    = c(ERA, WHIP, BAA),
      method     = "numeric",
      palette    = c(team_palette$success, team_palette$light,
                     team_palette$accent, team_palette$primary),
      domain     = c(0, 15)
    ) %>%
    data_color(
      columns    = c(K_BB),
      method     = "numeric",
      palette    = c(team_palette$light, team_palette$info,
                     team_palette$primary),
      domain     = c(0, 4)
    ) %>%
    cols_label(
      Player = "Pitcher", GP = "G", GS = "GS", IP = "IP",
      W = "W", L = "L", SV = "SV",
      H = "H", R = "R", ER = "ER", BB = "BB", SO = "SO",
      ERA = "ERA", WHIP = "WHIP", BAA = "BAA", K_BB = "K/BB"
    ) %>%
    tab_spanner(label = "Volume", columns = c(GP, GS, IP)) %>%
    tab_spanner(label = "Counting", columns = c(W, L, SV, H, R, ER, BB, SO)) %>%
    tab_spanner(label = "Rate", columns = c(ERA, WHIP, BAA, K_BB)) %>%
    tab_source_note(source_note = "Data: Nairobi Baseball Community") %>%
    opt_interactive(use_page_size_select = TRUE, page_size_default = 10) %>%
    opt_row_striping() %>%
    tab_options(
      heading.title.font.size  = "22px",
      heading.subtitle.font.size = "14px",
      table.font.size          = "13px",
      source_notes.font.size   = "10px"
    )
}

# ═══════════════════════════════════════════════════════════════════════════
# 7. GENERATE ALL PLOTS (returns a named list)
# ═══════════════════════════════════════════════════════════════════════════

generate_all_plots <- function(batting, csv_path = "tr-stats26.csv") {
  batting_ord <- batting %>% arrange(desc(OPS)) %>%
    mutate(Player = factor(Player, levels = Player))

  list(
    matrix       = plot_offensive_matrix(batting),
    quadrant     = plot_obp_slg_quadrant(batting),
    composition  = plot_hit_composition(batting),
    discipline   = plot_plate_discipline(batting),
    heatmap      = plot_counts_heatmap(batting),
    leaderboard  = plot_ops_leaderboard(batting),
    production   = plot_run_production(batting),
    contributions = plot_pitching_summary(csv_path),
    triple       = plot_offensive_trinity(batting),
    pa_dist      = plot_pa_distribution(batting),
    xbh_rate     = plot_xbh_rate(batting),
    contact_slg  = plot_contact_quality(batting)
  )
}
