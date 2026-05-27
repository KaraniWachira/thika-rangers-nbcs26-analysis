###############################################################################
#  team_theme.R — Team colours, ggplot2 theme & Shiny bslib theme
#  Thika Rangers · NBCS 2026 Regular Season
#
#  Palette balances the maroon brand identity (#77121C) with soft neutrals
#  (warm ivory, slate) so the dashboard is easy on the eyes.
###############################################################################

library(ggplot2)
library(bslib)

# ── 1. Softened palette ────────────────────────────────────────────────────

extract_team_colours <- function(logo_path = getOption("thika_logo_path",
                                                       "shiny/www/rebranded-logo.png")) {
  default <- c(
    primary     = "#77121C",   # maroon — keep as accent, not main bg
    secondary   = "#D4A843",   # warm gold
    accent      = "#A04040",   # muted brick (easier than bright red)
    light       = "#F5F0EB",   # warm ivory (replaces harsh white)
    dark        = "#2C3E50",   # dark slate (replaces black, pairs with maroon)
    neutral     = "#8B8B8B",   # warm grey for secondary text
    bg_card     = "#FAFAF7",   # soft card background
    bg_plot     = "#FCFCFA",   # plot panel background
    success     = "#4A8C6F",   # muted sage green
    info        = "#5A7D9C",   # muted steel blue
    warning     = "#D4A843",   # gold
    danger      = "#A04040"    # muted brick red
  )

  if (!file.exists(logo_path)) {
    alt_path <- file.path(dirname(logo_path), "TR-Logo.png")
    if (file.exists(alt_path)) logo_path <- alt_path
  }
  if (!file.exists(logo_path)) {
    message("Logo not found — using soft maroon/grey palette.")
    return(default)
  }

  tryCatch({
    library(imager)
    img <- load.image(logo_path)
    thumb <- imresize(img, scale = 0.1)
    rgb_mat <- matrix(as.numeric(thumb), ncol = 3)
    set.seed(42)
    km <- kmeans(rgb_mat, centers = 6, nstart = 10)
    hex <- rgb(km$centers[, 1], km$centers[, 2], km$centers[, 3])
    lum <- apply(km$centers, 1, function(rgb)
      0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3])
    sat <- apply(km$centers, 1, function(rgb) max(rgb) - min(rgb))
    w_lum <- weighted.mean(lum, km$size)

    dark_idx <- which(lum <= w_lum)
    primary_h <- if (length(dark_idx)) {
      hex[dark_idx[which.max(km$size[dark_idx])]]
    } else hex[which.min(lum)]

    mid_idx <- which(lum > w_lum * 0.5 & lum < w_lum * 1.5)
    accent_h <- if (length(mid_idx)) {
      hex[mid_idx[which.max(sat[mid_idx])]]
    } else hex[which.max(sat)]

    light_idx <- which(lum >= w_lum)
    secondary_h <- if (length(light_idx)) {
      hex[light_idx[which.max(km$size[light_idx])]]
    } else hex[which.max(lum)]

    all_h <- setdiff(hex, c(primary_h, accent_h))
    best_lum <- order(lum[hex %in% all_h], decreasing = TRUE)
    light_h <- if (length(best_lum)) all_h[best_lum[1]] else "#F5F0EB"
    dark_h  <- if (length(best_lum) > 1) all_h[best_lum[length(best_lum)]] else "#2C3E50"

    c(
      primary   = primary_h,
      secondary = secondary_h,
      accent    = accent_h,
      light     = light_h,
      dark      = dark_h,
      neutral   = "#8B8B8B",
      bg_card   = "#FAFAF7",
      bg_plot   = "#FCFCFA",
      success   = "#4A8C6F",
      info      = "#5A7D9C",
      warning   = "#D4A843",
      danger    = "#A04040"
    )
  }, error = function(e) {
    message("Logo extraction failed: ", e$message)
    default
  })
}

team_cols <- extract_team_colours()
team_palette <- as.list(team_cols)

# ── 2. ggplot2 theme — soft & clean ───────────────────────────────────────

theme_thika <- function(base_size = 11, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title         = element_text(face = "bold", size = rel(1.45),
                                        color = team_palette$primary),
      plot.subtitle      = element_text(color = team_palette$neutral,
                                        size = rel(0.85)),
      plot.caption       = element_text(color = team_palette$neutral,
                                        size = rel(0.65), hjust = 0,
                                        face = "italic"),
      plot.background    = element_rect(fill = team_palette$bg_plot, color = NA),
      panel.background   = element_rect(fill = team_palette$bg_plot, color = NA),
      strip.text         = element_text(face = "bold", size = rel(1),
                                        color = team_palette$dark),
      strip.background   = element_rect(fill = team_palette$light, color = NA),
      panel.grid.major   = element_line(color = "#EAE6E0", linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      axis.title         = element_text(color = team_palette$dark,
                                        size = rel(0.85)),
      axis.text          = element_text(color = team_palette$dark,
                                        size = rel(0.75)),
      legend.position    = "bottom",
      legend.title       = element_text(size = rel(0.8),
                                        color = team_palette$dark),
      legend.text        = element_text(size = rel(0.75),
                                        color = team_palette$neutral)
    )
}

# ── 3. bslib theme for Shiny ───────────────────────────────────────────────

shiny_theme <- bs_theme(
  version    = 5,
  bg         = team_palette$bg_card,
  fg         = team_palette$dark,
  primary    = team_palette$primary,
  secondary  = team_palette$secondary,
  success    = team_palette$success,
  info       = team_palette$info,
  warning    = team_palette$warning,
  danger     = team_palette$danger,
  base_font  = font_google("Inter"),
  heading_font = font_google("Inter"),
  code_font  = font_google("JetBrains Mono"),
  navbar_bg        = team_palette$dark,
  navbar_color     = "#FFFFFF",
  "navbar-brand-color" = "#FFFFFF",
  "progress-bar-bg"    = team_palette$secondary
)

# ── 4. ggplot2 colour / fill helpers ───────────────────────────────────────

scale_fill_thika <- function(...) {
  scale_fill_gradientn(
    colours = c(team_palette$light, team_palette$secondary,
                team_palette$accent, team_palette$primary),
    ...
  )
}

scale_colour_thika <- function(...) {
  scale_color_gradientn(
    colours = c(team_palette$secondary, team_palette$accent, team_palette$primary),
    ...
  )
}

# Maroon → neutral gradient for special uses
maroon_fade <- function(n = 9) {
  scales::gradient_n_pal(
    c(team_palette$light, team_palette$primary)
  )(seq(0, 1, length.out = n))
}
