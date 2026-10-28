library(tidyverse)
library(cowplot)
library(gridExtra)

base_family <- if (exists("font_family", inherits = TRUE)) font_family else "Helvetica"

asir <- readr::read_tsv("data-ASIR.txt", show_col_types = FALSE) |>
  tidyr::separate(`2019-2`, into = c("ASIR", "ASIR_interval"), sep = " ", extra = "merge") |>
  dplyr::mutate(ASIR = as.numeric(ASIR)) |>
  dplyr::select(location, ASIR)

world_map <- ggplot2::map_data("world") |>
  dplyr::filter(region != "Antarctica") |>
  dplyr::rename(country = region) |>
  dplyr::mutate(
    country = stringr::str_replace(country, "Taiwan", "China"),
    country = stringr::str_replace(country, "North Korea", "Democratic People's Republic of Korea"),
    country = stringr::str_replace(country, "South Korea", "Republic of Korea"),
    country = stringr::str_replace(country, "Russia", "Russian Federation"),
    country = stringr::str_replace(country, "USA", "United States of America"),
    country = stringr::str_replace(country, "Iran", "Iran (Islamic Republic of)"),
    country = stringr::str_replace(country, "Vietnam", "Viet Nam"),
    country = stringr::str_replace(country, "Laos", "Lao People's Democratic Republic"),
    country = stringr::str_replace(country, "Syria", "Syrian Arab Republic"),
    country = stringr::str_replace(country, "Moldova", "Republic of Moldova"),
    country = stringr::str_replace(country, "Czech Republic", "Czechia"),
    country = stringr::str_replace(country, "UK", "United Kingdom"),
    country = stringr::str_replace(country, "Ivory Coast", "Cote d'Ivoire"),
    country = stringr::str_replace(country, "Democratic Republic of the Congo", "Congo"),
    country = stringr::str_replace(country, "Republic of Congo", "Congo"),
    country = stringr::str_replace(country, "Tanzania", "United Republic of Tanzania"),
    country = stringr::str_replace(country, "Venezuela", "Venezuela (Bolivarian Republic of)"),
    country = stringr::str_replace(country, "Bolivia", "Bolivia (Plurinational State of)")
  )

map_data <- world_map |>
  dplyr::left_join(asir, by = c("country" = "location")) |>
  dplyr::filter(country != "Kiribati")

palette_cols <- c("#5EA9F3", "#91B7BC", "#D9D476", "#FF9A46", "#FF1A0A")
palette_fun <- grDevices::colorRampPalette(palette_cols)

table_data <- asir |>
  dplyr::filter(location %in% c("Poland", "Australia", "United States of America", "Bangladesh", "Bhutan", "India")) |>
  dplyr::arrange(dplyr::desc(ASIR)) |>
  dplyr::mutate(ASIR = sprintf("%.2f", ASIR)) |>
  dplyr::rename(country = location)

table_theme <- gridExtra::ttheme_minimal(
  base_size = 8.2, base_family = base_family,
  core = list(
    bg_params = list(fill = rep(c("#B5CBE4", "#DCE8F4"), length.out = nrow(table_data)), col = NA),
    fg_params = list(col = "#111111", hjust = 0.5, x = 0.5, fontfamily = base_family)
  ),
  colhead = list(
    bg_params = list(fill = "#4C82BD", col = NA),
    fg_params = list(col = "white", fontfamily = base_family)
  )
)
table_grob <- gridExtra::tableGrob(table_data, rows = NULL, theme = table_theme)

map_plot <- ggplot(map_data, aes(long, lat, group = group, fill = ASIR)) +
  geom_polygon(colour = "#111111", linewidth = 0.16, na.rm = FALSE) +
  annotate(
    "text", x = 20, y = -50, label = "2019", family = base_family,
    size = 29, colour = "#9A9A9A", alpha = 0.23
  ) +
  scale_fill_gradientn(colours = palette_fun(101), limits = c(0, 52),
                       na.value = "#CFCFCF", guide = "none") +
  annotate("segment", x = -158, xend = -158, y = 65, yend = 82,
           colour = "#444444", linewidth = 0.55,
           arrow = grid::arrow(length = grid::unit(0.12, "in"), type = "closed")) +
  annotate("segment", x = -166, xend = -150, y = 73, yend = 73,
           colour = "#444444", linewidth = 0.45) +
  annotate("text", x = -158, y = 86, label = "N", family = base_family,
           size = 4.2, colour = "#222222") +
  coord_fixed(ratio = 1.12, xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
  theme_void(base_family = base_family) +
  theme(
    plot.margin = margin(0, 0, 0, 0),
    legend.position = "none"
  )

legend_bins <- tibble::tibble(
  xmin = seq(0, 52, length.out = 102)[-102],
  xmax = seq(0, 52, length.out = 102)[-1],
  fill = palette_fun(101)
)
legend_ticks <- tibble::tibble(x = c(10, 20, 30, 40, 50))
legend_plot <- ggplot() +
  geom_rect(data = legend_bins, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1),
            fill = legend_bins$fill, colour = NA) +
  geom_segment(data = legend_ticks, aes(x = x, xend = x, y = 0, yend = 1),
               colour = "white", linewidth = 0.35) +
  geom_text(data = legend_ticks, aes(x = x, y = -0.28, label = x),
            family = base_family, size = 3.1, colour = "black") +
  coord_cartesian(xlim = c(0, 52), ylim = c(-0.52, 1), expand = FALSE, clip = "off") +
  theme_void(base_family = base_family) +
  theme(plot.margin = margin(0, 0, 0, 0))

final_plot <- cowplot::ggdraw(map_plot) +
  cowplot::draw_grob(table_grob, x = 0, y = 0.105, width = 0.26, height = 0.31) +
  cowplot::draw_plot(legend_plot, x = 0.27, y = 0.035, width = 0.57, height = 0.075)

final_plot
