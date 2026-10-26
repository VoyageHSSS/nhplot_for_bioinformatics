#!/usr/bin/env Rscript

# Radial network bar plot reconstructed from cover_019.
# Outer bar lengths are deterministic simulations because the source table is unavailable.

suppressPackageStartupMessages(library(ggplot2))

font_family <- "sans"
categories <- c("AA/Nucleotide\nMetabolism", "Miscellaneous\nMetabolism", "Stress\nResponse",
                "Carbon\nMetabolism", "Uncharacterized", "Cellular\nProcesses", "Phages",
                "Single\nGenes", "Other", "Virulence")
cat_cols <- c("AA/Nucleotide\nMetabolism" = "#7660A4", "Miscellaneous\nMetabolism" = "#E49B54",
              "Stress\nResponse" = "#E44C6C", "Carbon\nMetabolism" = "#5A9B78",
              "Uncharacterized" = "#716664", "Cellular\nProcesses" = "#4B92C0",
              "Phages" = "#DD4D55", "Single\nGenes" = "#C79E44", "Other" = "#B0B0A0",
              "Virulence" = "#A84D4E")

simulate_network <- function(seed = 20260908L) {
  set.seed(seed)
  counts <- c(10, 10, 9, 8, 10, 9, 8, 8, 7, 9)
  theta <- pi / 2 - seq(0, 2 * pi, length.out = length(categories) + 1)[-(length(categories) + 1)]
  centers <- data.frame(category = categories, theta = theta, r = .31,
                        x = .31 * cos(theta), y = .31 * sin(theta),
                        stringsAsFactors = FALSE)
  leaves <- do.call(rbind, lapply(seq_along(categories), function(i) {
    n <- counts[i]
    ang <- theta[i] + seq(-.24, .24, length.out = n) + runif(n, -.018, .018)
    inner <- runif(n, .86, .96)
    len <- pmax(.11, pmin(.38, rgamma(n, shape = 2.4, scale = .09)))
    outer <- inner + len
    data.frame(category = categories[i], theta = ang, inner = inner, outer = outer,
               x = inner * cos(ang), y = inner * sin(ang),
               xend = outer * cos(ang), yend = outer * sin(ang),
               label = paste0("Gene_", seq_len(n)), stringsAsFactors = FALSE)
  }))
  centers$category <- factor(centers$category, levels = categories)
  leaves$category <- factor(leaves$category, levels = categories)
  center_edges <- data.frame(x = 0, y = 0, xend = centers$x, yend = centers$y, category = centers$category)
  leaves$lx <- (leaves$outer + .075) * cos(leaves$theta)
  leaves$ly <- (leaves$outer + .075) * sin(leaves$theta)
  leaves$hjust <- ifelse(cos(leaves$theta) >= 0, 0, 1)
  leaves$label_angle <- ifelse(cos(leaves$theta) >= 0, leaves$theta * 180 / pi, leaves$theta * 180 / pi + 180)
  list(centers = centers, leaves = leaves, center_edges = center_edges)
}

make_bar_polygons <- function(leaves, width = .018) {
  do.call(rbind, lapply(seq_len(nrow(leaves)), function(i) {
    z <- leaves[i, ]
    a <- z$theta + c(-width, width, width, -width)
    r <- c(z$inner, z$inner, z$outer, z$outer)
    data.frame(category = z$category, id = i, x = r * cos(a), y = r * sin(a))
  }))
}

build_plot <- function(seed = 20260908L) {
  net <- simulate_network(seed)
  bars <- make_bar_polygons(net$leaves)
  ggplot() +
    geom_curve(data = net$center_edges, aes(x, y, xend = xend, yend = yend, colour = category),
               curvature = .1, linewidth = .35, alpha = .52, lineend = "round") +
    geom_curve(data = net$leaves, aes(x, y, xend = xend, yend = yend, colour = category),
               curvature = .18, linewidth = .28, alpha = .5, lineend = "round") +
    geom_polygon(data = bars, aes(x, y, group = id, fill = category, colour = category), linewidth = .25, alpha = .9) +
    geom_point(data = net$centers, aes(x, y, fill = category), shape = 21, colour = "white", size = 5.6, stroke = .5) +
    geom_text(data = net$centers, aes(x = 1.08 * x, y = 1.08 * y, label = category, colour = category),
              size = 2.6, fontface = "bold", lineheight = .82) +
    geom_text(data = net$leaves, aes(lx, ly, label = label, hjust = hjust, angle = label_angle, colour = category),
              size = 1.25, show.legend = FALSE) +
    scale_colour_manual(values = cat_cols, guide = "none") + scale_fill_manual(values = cat_cols, guide = "none") +
    coord_equal(xlim = c(-1.48, 1.48), ylim = c(-1.48, 1.48), expand = FALSE, clip = "off") +
    theme_void(base_family = font_family) +
    theme(plot.margin = margin(4, 4, 4, 4), plot.background = element_rect(fill = "white", colour = NA))
}
