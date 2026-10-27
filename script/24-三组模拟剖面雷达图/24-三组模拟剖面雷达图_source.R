#!/usr/bin/env Rscript

# Three peptide/TCR radar panels reconstructed from cover_017.
# Values are deterministic simulated profiles; no source table was supplied.

suppressPackageStartupMessages({
  library(ggplot2)
  library(cowplot)
})

font_family <- "sans"
groups <- c("PG3:High", "PG2:Low", "PG1:Low")
group_cols <- c("PG3:High" = "#E6A04C", "PG2:Low" = "#63A8C5", "PG1:Low" = "#5BAA78")
dimensions <- LETTERS[1:8]

simulate_data <- function(seed = 20260908L) {
  set.seed(seed)
  means <- rbind(
    `PG3:High` = c(.83, .88, .12, .10, .08, .14, .87, .91),
    `PG2:Low` = c(.76, .78, .76, .70, .58, .64, .72, .20),
    `PG1:Low` = c(.55, .73, .82, .78, .62, .55, .34, .38)
  )
  d <- do.call(rbind, lapply(groups, function(g) {
    data.frame(group = g, dimension = dimensions,
               value = pmax(.04, pmin(.98, means[g, ] + rnorm(length(dimensions), 0, .035))))
  }))
  d$group <- factor(d$group, levels = groups)
  d$dimension <- factor(d$dimension, levels = dimensions)
  d
}

make_radar <- function(d, group_name, big = FALSE) {
  n <- length(dimensions)
  theta <- pi / 2 - seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
  grid <- do.call(rbind, lapply(seq(.2, 1, .2), function(r) {
    data.frame(level = r, angle = c(theta, theta[1]),
               x = r * cos(c(theta, theta[1])), y = r * sin(c(theta, theta[1])))
  }))
  grid$grid_id <- interaction(grid$level, drop = TRUE)
  spokes <- data.frame(x = 0, y = 0, xend = cos(theta), yend = sin(theta))
  poly <- d[d$group == group_name, ]
  poly$angle <- theta[as.numeric(poly$dimension)]
  poly$x <- poly$value * cos(poly$angle)
  poly$y <- poly$value * sin(poly$angle)
  poly <- rbind(poly, poly[1, ])
  labels <- data.frame(x = 1.18 * cos(theta), y = 1.18 * sin(theta), label = dimensions)
  col <- unname(group_cols[group_name])
  txt_size <- if (big) 3.0 else 1.55
  title_size <- if (big) 5.2 else 3.0
  ggplot() +
    geom_polygon(data = grid, aes(x, y, group = grid_id), fill = NA, colour = "#D7DDE0", linewidth = .2) +
    geom_segment(data = spokes, aes(x, y, xend = xend, yend = yend), colour = "#D7DDE0", linewidth = .2) +
    geom_polygon(data = poly, aes(x, y), fill = col, colour = col, alpha = .32, linewidth = if (big) .75 else .45) +
    geom_path(data = poly, aes(x, y), colour = col, linewidth = if (big) .85 else .5) +
    geom_point(data = poly[-nrow(poly), ], aes(x, y), colour = col, size = if (big) .65 else .3) +
    geom_text(data = labels, aes(x, y, label = label), size = txt_size, colour = "#51585D") +
    annotate("rect", xmin = -1.3, xmax = 1.3, ymin = 1.27, ymax = 1.48,
             fill = col, colour = "#444444", linewidth = if (big) .45 else .25) +
    annotate("text", x = 0, y = 1.375, label = group_name, size = title_size,
             colour = "white", family = font_family) +
    coord_equal(xlim = c(-1.32, 1.32), ylim = c(-1.32, 1.5), expand = FALSE, clip = "off") +
    theme_void(base_family = font_family) +
    theme(panel.border = element_rect(fill = NA, colour = "#444444", linewidth = if (big) .45 else .25),
          plot.margin = margin(2, 2, 2, 2), plot.background = element_rect(fill = "white", colour = NA))
}

info_panel <- function(header, body, fill) {
  ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = fill, colour = NA) +
    annotate("text", x = .5, y = .77, label = header, size = 4.2, colour = "#4C555A", family = font_family) +
    annotate("text", x = .5, y = .43, label = body, size = 2.6, lineheight = .95, colour = "#4C555A", family = font_family) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_void(base_family = font_family) +
    theme(plot.margin = margin(2, 6, 2, 6), panel.border = element_rect(fill = NA, colour = "#D6E1E4", linewidth = .25))
}

build_plot <- function(seed = 20260908L) {
  d <- simulate_data(seed)
  top <- cowplot::plot_grid(plotlist = lapply(groups, function(g) make_radar(d, g, big = FALSE)),
                            ncol = 3, rel_widths = rep(1, 3), align = "hv")
  notes <- cowplot::plot_grid(
    info_panel("Peptide antigen", "A: Anchor   H: HPhobic\nB: Charge\nC: Bulkiness   D: HPhobic   | Exposed", "#E4F6F8"),
    info_panel("TCRb", "E: TCRb_length\nF: Charge\nG: Bulkiness   H: HPhobic   | CDR3bmer", "#F0F2F2"),
    ncol = 2, rel_widths = c(1, 1), align = "h"
  )
  cowplot::plot_grid(top, notes, ncol = 1, rel_heights = c(2.45, .72), align = "v")
}
