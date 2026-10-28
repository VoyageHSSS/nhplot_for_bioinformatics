#!/usr/bin/env Rscript

# Bivariate density plot with marginal histograms reconstructed from cover_031.
# Data are deterministic simulations because the source data are unavailable.

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
  library(ggnewscale)
})

font_family <- "sans"
group_cols <- c(GroupA = "#6F9EB8", GroupB = "#D96767")
group_fills <- c(GroupA = "#D9ECEE", GroupB = "#E9B7B7")

simulate_data <- function(seed = 20260908L, n_per_group = 2200) {
  set.seed(seed)
  make_group <- function(n, mu, sd, rho, group) {
    z1 <- rnorm(n)
    z2 <- rnorm(n)
    data.frame(
      X_lab = mu[1] + sd[1] * z1,
      Y_lab = mu[2] + sd[2] * (rho * z1 + sqrt(1 - rho^2) * z2),
      group = group, stringsAsFactors = FALSE
    )
  }
  dat <- rbind(
    make_group(n_per_group, c(50, 60), c(3.15, 4.8), -.12, "GroupA"),
    make_group(n_per_group, c(60, 50), c(3.2, 4.6), .12, "GroupB")
  )
  dat <- dat[dat$X_lab >= 30 & dat$X_lab <= 80 & dat$Y_lab >= 30 & dat$Y_lab <= 80, ]
  dat$group <- factor(dat$group, levels = c("GroupA", "GroupB"))
  dat
}

build_plot <- function(seed = 20260908L) {
  dat <- simulate_data(seed)
  top <- ggplot(dat, aes(X_lab, fill = group)) +
    geom_histogram(aes(y = after_stat(density)), bins = 28, position = "identity",
                   alpha = .66, colour = "#6F6F6F", linewidth = .16) +
    stat_density(aes(colour = group), linewidth = .70, adjust = .9,
                 show.legend = FALSE) +
    scale_fill_manual(values = group_fills, name = NULL) +
    scale_colour_manual(values = group_cols, guide = "none") +
    scale_x_continuous(breaks = seq(30, 80, 10), expand = c(0, 0)) +
    coord_cartesian(xlim = c(30, 80), ylim = c(0, .16), expand = FALSE) +
    labs(y = "Density") +
    annotate("text", x = 31.1, y = .145, label = "a", fontface = "bold", size = 2.2) +
    theme_classic(base_family = font_family) +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), axis.title.y = element_text(size = 8),
          axis.text.y = element_text(size = 7, colour = "#222222"),
          axis.line = element_line(linewidth = .55, colour = "#111111"),
          legend.position = c(.83, .78), legend.background = element_rect(fill = "white", colour = NA),
          legend.key = element_rect(fill = "white", colour = NA), legend.text = element_text(size = 7),
          plot.margin = margin(2, 2, 0, 2))

  main <- ggplot() +
    stat_density_2d(data = dat[dat$group == "GroupA", ],
                    aes(X_lab, Y_lab, fill = after_stat(level)), geom = "polygon",
                    bins = 12, alpha = .78, colour = NA) +
    scale_fill_gradientn(colours = c("#F4FBFC", "#B8D6E5", "#5E8FB4"), guide = "none") +
    ggnewscale::new_scale_fill() +
    stat_density_2d(data = dat[dat$group == "GroupB", ],
                    aes(X_lab, Y_lab, fill = after_stat(level)), geom = "polygon",
                    bins = 12, alpha = .78, colour = NA) +
    scale_fill_gradientn(colours = c("#FFF7F4", "#F1B8AD", "#D35E58"), guide = "none") +
    scale_x_continuous(breaks = seq(30, 80, 10), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(30, 80, 10), expand = c(0, 0)) +
    coord_cartesian(xlim = c(30, 80), ylim = c(30, 80), expand = FALSE) +
    labs(x = "X_lab", y = "Y_lab") +
    annotate("text", x = 31.2, y = 78.2, label = "b", fontface = "bold", size = 2.2) +
    theme_classic(base_family = font_family) +
    theme(axis.title = element_text(size = 8), axis.text = element_text(size = 7, colour = "#222222"),
          axis.line = element_line(linewidth = .55, colour = "#111111"),
          panel.border = element_rect(fill = NA, colour = "#111111", linewidth = .45),
          plot.margin = margin(0, 0, 0, 2))

  right <- ggplot(dat, aes(Y_lab, fill = group)) +
    geom_histogram(aes(y = after_stat(density)), bins = 28, position = "identity",
                   alpha = .66, colour = "#6F6F6F", linewidth = .16) +
    scale_fill_manual(values = group_fills, guide = "none") +
    scale_x_continuous(breaks = seq(30, 80, 10), expand = c(0, 0)) +
    coord_flip(expand = FALSE) +
    labs(x = "", y = "Density") +
    theme_classic(base_family = font_family) +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = 7), axis.text.y = element_text(size = 6.5, colour = "#222222"),
          axis.line = element_line(linewidth = .55, colour = "#111111"),
          plot.margin = margin(0, 2, 0, 0))

  top / (main | right) + plot_layout(heights = c(1.1, 4.0), widths = c(4.1, 1.0))
}
