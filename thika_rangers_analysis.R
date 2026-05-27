###############################################################################
#  Thika Rangers Baseball Club — Batting Analysis for NBCS 2026 Quarterfinals
#  Data exported from GameChanger app
#  Quarterfinals: Saturday 30 May 2026
###############################################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# ── 1. Read & clean ──────────────────────────────────────────────────────────

raw <- read.csv("tr-stats26.csv",
                header = FALSE,
                stringsAsFactors = FALSE,
                na.strings = c("", " ", ".000"),
                strip.white = TRUE)

colnames(raw) <- as.character(raw[2, ])
players_raw <- raw[3:26, ]

keep <- c("Number", "Last", "First", "GP", "PA", "AB", "AVG", "OBP", "SLG",
          "OPS", "H", "1B", "2B", "3B", "HR", "RBI", "R", "BB", "SO", "HBP",
          "TB", "XBH")
batting <- players_raw[, intersect(keep, colnames(players_raw))]

numeric_cols <- setdiff(names(batting), c("Number", "Last", "First"))
batting[numeric_cols] <- lapply(batting[numeric_cols], function(x) {
  x <- gsub("^\\s*$", NA, x)
  as.numeric(x)
})

batting <- batting %>% filter(PA > 0 & !is.na(PA))
batting$Player <- paste(batting$First, batting$Last)

# ── 2. Verify rate stats ─────────────────────────────────────────────────────
batting <- batting %>% mutate(
  BA_calc  = round(H / AB, 3),
  OBP_calc = round((H + BB + HBP) / (AB + BB + HBP), 3),
  SLG_calc = round(TB / AB, 3),
  OPS_calc = round(OBP_calc + SLG_calc, 3)
)

# ── 3. Order by OPS ──────────────────────────────────────────────────────────
batting <- batting %>% arrange(desc(OPS)) %>%
  mutate(Player = factor(Player, levels = Player))

# ── 4. Label helper (ggrepel if installed, else plain text) ──────────────────
has_repel <- requireNamespace("ggrepel", quietly = TRUE)
label_geom <- if (has_repel) ggrepel::geom_text_repel else geom_text
label_args <- if (has_repel) {
  list(mapping = aes(label = Player), size = 3, max.overlaps = 12,
       box.padding = 0.35, point.padding = 0.3, segment.color = "grey60",
       segment.size = 0.3)
} else {
  list(mapping = aes(label = Player), size = 3, vjust = -1.2,
       check_overlap = TRUE)
}

# ─────────────────────────────────────────────────────────────────────────────
# 5. CREATIVE VISUALISATIONS
# ─────────────────────────────────────────────────────────────────────────────

# ═══════════════════════════════════════════════════════════════════════════
# FIGURE 1: Offensive Profile Matrix — Lollipop charts for AVG, OBP, SLG, OPS
# ═══════════════════════════════════════════════════════════════════════════

rate_long <- batting %>%
  select(Player, AVG, OBP, SLG, OPS) %>%
  pivot_longer(-Player, names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = factor(Metric, levels = c("AVG", "OBP", "SLG", "OPS")))

team_avg <- rate_long %>%
  group_by(Metric) %>%
  summarise(TeamVal = mean(Value, na.rm = TRUE), .groups = "drop")

p1 <- ggplot(rate_long, aes(x = Value, y = Player, color = Metric)) +
  geom_segment(aes(xend = 0, yend = Player), linewidth = 0.6, alpha = 0.4) +
  geom_point(size = 3.5) +
  geom_vline(data = team_avg, aes(xintercept = TeamVal, color = Metric),
             linewidth = 0.8, linetype = "dashed", alpha = 0.7) +
  facet_wrap(~ Metric, ncol = 4, scales = "free_x") +
  scale_color_manual(values = c(AVG = "#E74C3C", OBP = "#3498DB",
                                SLG = "#2ECC71", OPS = "#9B59B6")) +
  scale_x_continuous(labels = label_number(accuracy = 0.001)) +
  labs(
    title    = "Thika Rangers — Offensive Profile Matrix",
    subtitle = "Four-facet lollipop chart of core rate statistics  ·  Dashed line = team mean",
    x        = NULL, y = NULL,
    caption  = "Data: GameChanger  |  NBCS 2026 Quarterfinals · 30 May 2026"
  ) +
  theme_minimal(base_family = "sans", base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 16),
    plot.subtitle    = element_text(color = "grey40", size = 9),
    plot.caption     = element_text(color = "grey60", size = 7, hjust = 0),
    strip.text       = element_text(face = "bold", size = 11),
    legend.position  = "none",
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(size = 8)
  )

ggsave("01_offensive_profile_matrix.png", p1, width = 14, height = 7, dpi = 300)

# ═══════════════════════════════════════════════════════════════════════════
# FIGURE 2: OBP–SLG Quadrant Chart (the "sweet spot" plot)
# ═══════════════════════════════════════════════════════════════════════════

med_obp <- median(batting$OBP, na.rm = TRUE)
med_slg <- median(batting$SLG, na.rm = TRUE)

p2 <- ggplot(batting, aes(x = OBP, y = SLG)) +
  annotate("rect", xmin = -Inf, xmax = med_obp, ymin = -Inf, ymax = med_slg,
           fill = "#E74C3C", alpha = 0.06) +
  annotate("rect", xmin = med_obp, xmax = Inf, ymin = -Inf, ymax = med_slg,
           fill = "#F39C12", alpha = 0.06) +
  annotate("rect", xmin = -Inf, xmax = med_obp, ymin = med_slg, ymax = Inf,
           fill = "#3498DB", alpha = 0.06) +
  annotate("rect", xmin = med_obp, xmax = Inf, ymin = med_slg, ymax = Inf,
           fill = "#2ECC71", alpha = 0.06) +
  annotate("text", x = 0.22, y = 0.90, label = "High SLG · Low OBP\n(Disciplined Power?)",
           size = 2.8, color = "#3498DB", fontface = "italic", alpha = 0.7) +
  annotate("text", x = 0.88, y = 0.90, label = "Sweet Spot\nElite Hitters",
           size = 3.2, color = "#2ECC71", fontface = "bold", alpha = 0.8) +
  annotate("text", x = 0.22, y = 0.20, label = "Needs Work:\nLow OBP · Low Power",
           size = 2.8, color = "#E74C3C", fontface = "italic", alpha = 0.7) +
  annotate("text", x = 0.88, y = 0.20, label = "High OBP · Low Power\n(Table Setters)",
           size = 2.8, color = "#F39C12", fontface = "italic", alpha = 0.7) +
  geom_vline(xintercept = med_obp, linetype = "dashed", color = "grey50",
             linewidth = 0.5) +
  geom_hline(yintercept = med_slg, linetype = "dashed", color = "grey50",
             linewidth = 0.5) +
  geom_point(aes(size = PA, fill = OPS), shape = 21, color = "grey20",
             stroke = 0.8, alpha = 0.9) +
  do.call(label_geom, label_args) +
  scale_fill_viridis_c(option = "D", direction = 1) +
  scale_size_continuous(range = c(3, 12), name = "Plate Appearances") +
  scale_x_continuous(labels = label_number(accuracy = 0.001)) +
  scale_y_continuous(labels = label_number(accuracy = 0.001)) +
  labs(
    title    = "Thika Rangers — OBP vs SLG Quadrant Analysis",
    subtitle = paste0("Dashed lines = team median (OBP=", round(med_obp, 3),
                      ", SLG=", round(med_slg, 3),
                      ").  Dot size = PA, colour = OPS."),
    x        = "On-Base Percentage (OBP)",
    y        = "Slugging Percentage (SLG)",
    caption  = "Data: GameChanger  |  NBCS 2026 Quarterfinals · 30 May 2026"
  ) +
  guides(fill = guide_colorbar(title = "OPS", barwidth = 10, barheight = 0.5)) +
  theme_minimal(base_family = "sans", base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 16),
    plot.subtitle    = element_text(color = "grey40", size = 9),
    plot.caption     = element_text(color = "grey60", size = 7, hjust = 0),
    legend.position  = "bottom",
    legend.box       = "horizontal",
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3)
  )

ggsave("02_obp_slg_quadrant.png", p2, width = 11, height = 8, dpi = 300)

# ═══════════════════════════════════════════════════════════════════════════
# FIGURE 3: Hit Composition — 1B / 2B / 3B / HR proportion
# ═══════════════════════════════════════════════════════════════════════════

hit_comp <- batting %>%
  filter(H > 0) %>%
  select(Player, `1B`, `2B`, `3B`, `HR`) %>%
  pivot_longer(-Player, names_to = "HitType", values_to = "Count") %>%
  mutate(HitType = factor(HitType, levels = c("HR", "3B", "2B", "1B")))

p3 <- ggplot(hit_comp, aes(x = Player, y = Count, fill = HitType)) +
  geom_col(position = "fill", width = 0.75, color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = label_percent(), expand = c(0, 0)) +
  scale_fill_manual(
    values = c("1B" = "#2ECC71", "2B" = "#3498DB", "3B" = "#9B59B6", "HR" = "#E74C3C"),
    labels = c("HR", "3B", "2B", "1B")
  ) +
  labs(
    title    = "Thika Rangers — Hit Type Composition",
    subtitle = "Each bar shows proportion of singles, doubles, triples & home runs among total hits",
    x        = NULL, y = "Proportion of Hits",
    fill     = "Hit Type",
    caption  = "Players with zero hits excluded  |  Data: GameChanger"
  ) +
  theme_minimal(base_family = "sans", base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 16),
    plot.subtitle    = element_text(color = "grey40", size = 9),
    plot.caption     = element_text(color = "grey60", size = 7, hjust = 0),
    legend.position  = "bottom",
    axis.text.x      = element_text(angle = 35, hjust = 1, size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave("03_hit_composition.png", p3, width = 12, height = 6, dpi = 300)

# ═══════════════════════════════════════════════════════════════════════════
# FIGURE 4: Plate Discipline — BB vs SO
# ═══════════════════════════════════════════════════════════════════════════

p4 <- ggplot(batting, aes(x = BB, y = SO)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted",
              color = "grey60", linewidth = 0.6) +
  annotate("text", x = max(batting$BB)*0.7, y = max(batting$SO)*0.25,
           label = "More BB than SO", angle = 40, size = 3,
           color = "grey50", fontface = "italic") +
  annotate("text", x = max(batting$BB)*0.25, y = max(batting$SO)*0.7,
           label = "More SO than BB", angle = 40, size = 3,
           color = "grey50", fontface = "italic") +
  geom_point(aes(size = PA, fill = OBP), shape = 21, color = "grey20",
             stroke = 0.8, alpha = 0.85) +
  do.call(label_geom, label_args) +
  scale_fill_viridis_c(option = "A", direction = 1) +
  scale_size_continuous(range = c(2, 11)) +
  labs(
    title    = "Thika Rangers — Plate Discipline: Walks vs Strikeouts",
    subtitle = "Above dotted = more SO than BB  ·  Colour = OBP, size = PA",
    x        = "Walks (BB)",
    y        = "Strikeouts (SO)",
    caption  = "Data: GameChanger  |  NBCS 2026 Quarterfinals · 30 May 2026"
  ) +
  guides(fill = guide_colorbar(title = "OBP", barwidth = 8, barheight = 0.4)) +
  theme_minimal(base_family = "sans", base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 16),
    plot.subtitle    = element_text(color = "grey40", size = 9),
    plot.caption     = element_text(color = "grey60", size = 7, hjust = 0),
    legend.position  = "bottom",
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3)
  )

ggsave("04_discipline_walks_vs_so.png", p4, width = 10, height = 8, dpi = 300)

# ═══════════════════════════════════════════════════════════════════════════
# FIGURE 5: Counting Stats Heatmap
# ═══════════════════════════════════════════════════════════════════════════

counts_long <- batting %>%
  select(Player, PA, AB, H, `1B`, `2B`, `3B`, HR, BB, SO, HBP, TB, RBI, R) %>%
  pivot_longer(-Player, names_to = "Stat", values_to = "Count") %>%
  mutate(Stat = factor(Stat, levels = c("PA", "AB", "H", "1B", "2B", "3B",
                                        "HR", "BB", "SO", "HBP", "TB", "RBI", "R")))

p5 <- ggplot(counts_long, aes(x = Stat, y = Player)) +
  geom_tile(aes(fill = Count), color = "white", linewidth = 1.2) +
  geom_text(aes(label = Count), size = 2.8, fontface = "bold",
            color = "grey20") +
  scale_fill_gradientn(
    colours = c("#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", "#B30000"),
    na.value = "grey95"
  ) +
  labs(
    title    = "Thika Rangers — Batting Counts Heatmap",
    subtitle = "Darker tiles = higher counts  ·  Comprehensive view of all counting statistics",
    x        = NULL, y = NULL,
    caption  = "Data: GameChanger  |  NBCS 2026 Quarterfinals · 30 May 2026"
  ) +
  theme_minimal(base_family = "sans", base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", size = 16),
    plot.subtitle    = element_text(color = "grey40", size = 9),
    plot.caption     = element_text(color = "grey60", size = 7, hjust = 0),
    legend.position  = "right",
    panel.grid       = element_blank(),
    axis.text.x      = element_text(angle = 0, hjust = 0.5, size = 9,
                                    face = "bold"),
    axis.text.y      = element_text(size = 8)
  )

ggsave("05_counts_heatmap.png", p5, width = 12, height = 7, dpi = 300)

# ═══════════════════════════════════════════════════════════════════════════
# FIGURE 6: OPS Leaderboard
# ═══════════════════════════════════════════════════════════════════════════

p6 <- ggplot(batting, aes(x = OPS, y = Player)) +
  geom_segment(aes(xend = 0, yend = Player), color = "grey75", linewidth = 0.5) +
  geom_point(aes(color = OPS), size = 5) +
  geom_text(aes(label = sprintf("%.3f", OPS)),
            hjust = -0.3, size = 3.3, fontface = "bold", color = "grey30") +
  scale_color_gradient(low = "#F39C12", high = "#E74C3C") +
  scale_x_continuous(limits = c(0, max(batting$OPS, na.rm = TRUE) * 1.2),
                     labels = label_number(accuracy = 0.001)) +
  labs(
    title    = "Thika Rangers — OPS Leaderboard",
    subtitle = "On-Base Plus Slugging: the gold-standard measure of overall offensive output",
    x        = "OPS",
    y        = NULL,
    caption  = "Data: GameChanger  |  NBCS 2026 Quarterfinals · 30 May 2026"
  ) +
  theme_minimal(base_family = "sans", base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 16),
    plot.subtitle    = element_text(color = "grey40", size = 9),
    plot.caption     = element_text(color = "grey60", size = 7, hjust = 0),
    legend.position  = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(size = 9)
  )

ggsave("06_ops_leaderboard.png", p6, width = 10, height = 7, dpi = 300)

# ═══════════════════════════════════════════════════════════════════════════
# FIGURE 7: Run Production — R vs RBI
# ═══════════════════════════════════════════════════════════════════════════

p7 <- ggplot(batting, aes(x = R, y = RBI)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted",
              color = "grey60", linewidth = 0.5) +
  geom_point(aes(size = H, fill = OPS), shape = 21, color = "grey20",
             stroke = 0.7, alpha = 0.85) +
  do.call(label_geom, label_args) +
  scale_fill_viridis_c(option = "E", direction = -1) +
  scale_size_continuous(range = c(2, 10)) +
  labs(
    title    = "Thika Rangers — Run Production",
    subtitle = "Runs Scored vs Runs Batted In  ·  Size = hits, colour = OPS  ·  Above dotted = more RBI than R",
    x        = "Runs Scored (R)",
    y        = "Runs Batted In (RBI)",
    caption  = "Data: GameChanger  |  NBCS 2026 Quarterfinals · 30 May 2026"
  ) +
  guides(fill = guide_colorbar(title = "OPS", barwidth = 8, barheight = 0.4)) +
  theme_minimal(base_family = "sans", base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 16),
    plot.subtitle    = element_text(color = "grey40", size = 9),
    plot.caption     = element_text(color = "grey60", size = 7, hjust = 0),
    legend.position  = "bottom",
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3)
  )

ggsave("07_run_production.png", p7, width = 10, height = 8, dpi = 300)

# ─────────────────────────────────────────────────────────────────────────────
# 6. CONSOLE SUMMARY
# ─────────────────────────────────────────────────────────────────────────────

cat("\n", paste(rep("═", 72), collapse = ""), "\n")
cat("  THIKA RANGERS BASEBALL CLUB — BATTING SUMMARY (NBCS 2026)\n")
cat(paste(rep("═", 72), collapse = ""), "\n\n")

cat(sprintf("  %-22s %4s %4s %5s %5s %5s %5s %5s %5s %4s %4s %4s %4s\n",
            "Player", "GP", "PA", "AB", "H", "BB", "SO", "AVG", "OBP", "SLG",
            "OPS", "1B", "2B", "HR"))
cat(paste(rep("─", 72), collapse = ""), "\n")

for (i in seq_len(nrow(batting))) {
  p <- batting[i, ]
  cat(sprintf("  %-22s %4.0f %4.0f %4.0f %4.0f %4.0f %4.0f %5.3f %5.3f %5.3f %5.3f %4.0f %4.0f %4.0f\n",
              p$Player, p$GP, p$PA, p$AB, p$H, p$BB, p$SO,
              p$AVG, p$OBP, p$SLG, p$OPS, p$`1B`, p$`2B`, p$HR))
}

cat(paste(rep("─", 72), collapse = ""), "\n")

team_avg_calc <- with(batting, {
  ba  <- sum(H) / sum(AB)
  obp <- (sum(H) + sum(BB) + sum(HBP)) / (sum(AB) + sum(BB) + sum(HBP))
  slg <- sum(TB) / sum(AB)
  ops <- obp + slg
  c(BA = round(ba, 3), OBP = round(obp, 3), SLG = round(slg, 3), OPS = round(ops, 3))
})

cat(sprintf("  %-22s %4s %4.0f %4.0f %4.0f %4.0f %4.0f %5.3f %5.3f %5.3f %5.3f\n",
            "TEAM TOTAL", "", sum(batting$PA), sum(batting$AB), sum(batting$H),
            sum(batting$BB), sum(batting$SO),
            team_avg_calc["BA"], team_avg_calc["OBP"],
            team_avg_calc["SLG"], team_avg_calc["OPS"]))
cat(paste(rep("═", 72), collapse = ""), "\n\n")

cat("  7 visualisations saved as PNG:\n")
cat("    1. 01_offensive_profile_matrix.png   — Faceted lollipop: AVG / OBP / SLG / OPS\n")
cat("    2. 02_obp_slg_quadrant.png           — OBP vs SLG with sweet-spot quadrants\n")
cat("    3. 03_hit_composition.png            — Hit-type proportions (1B/2B/3B/HR)\n")
cat("    4. 04_discipline_walks_vs_so.png     — Walks vs strikeouts plate discipline\n")
cat("    5. 05_counts_heatmap.png             — Tile heatmap of all counting stats\n")
cat("    6. 06_ops_leaderboard.png            — OPS leaderboard dot chart\n")
cat("    7. 07_run_production.png             — Runs vs RBI production scatter\n\n")

cat("  Best of luck in the quarterfinals, Thika Rangers!\n")
cat(paste(rep("═", 72), collapse = ""), "\n\n")
