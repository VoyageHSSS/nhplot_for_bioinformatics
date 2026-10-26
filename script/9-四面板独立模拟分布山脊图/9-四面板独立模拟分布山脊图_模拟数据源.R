required_packages <- c("ggplot2", "ggridges", "patchwork")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Missing R packages: ", paste(missing_packages, collapse = ", "))

make_ridge <- function(seed, marine = FALSE, metric = c("year", "jif")) {
  metric <- match.arg(metric)
  set.seed(seed)
  if (marine) {
    regions <- c("Arctic", "Central Indo-Pacific", "Eastern Indo-Pacific", "Southern Ocean",
                 "Temperate Australasia", "Temperate Northern Atlantic", "Temperate Northern Pacific",
                 "Temperate South America", "Temperate Southern Africa", "Tropical Atlantic", "Tropical Eastern Pacific", "MarMultiple")
    ns <- c(6, 7, 1, 7, 4, 33, 17, 3, 1, 8, 1, 1)
    means <- c(1998, 2007, 2013, 2010, 2014, 2011, 2006, 2013, 2011, 2008, 2014, 2016)
  } else {
    regions <- c("Afrotropic", "Antarctic", "Australasia", "Indomalaya", "Nearctic", "Neotropic", "Oceania", "Palearctic", "TerrMultiple")
    ns <- c(47, 2, 49, 26, 228, 70, 13, 368, 2)
    means <- c(2012, 2006, 2011, 2010, 2012, 2012, 2015, 2010, 2011)
  }
  region_levels <- rev(paste0(regions, "    n = ", ns))
  dat <- do.call(rbind, lapply(seq_along(regions), function(i) {
    if (metric == "year") {
      x <- pmin(2025, pmax(1980, round(rnorm(max(ns[i], 6), means[i], ifelse(ns[i] < 10, 4, 6)))))
    } else {
      x <- pmin(100, pmax(-15, rbeta(max(ns[i], 6), 2.4 + i / 5, 2.2) * 105 - 5))
    }
    data.frame(x = x, region = paste0(regions[i], "    n = ", ns[i]), region_raw = regions[i])
  }))
  dat$region <- factor(dat$region, levels = region_levels)
  pal <- if (marine) c("#9BE66E", "#53D3B1", "#4EB4D6", "#5A91D1", "#7696D6", "#7B8BCF", "#8596D4", "#7FA9E2", "#50CBBF", "#61CA9F", "#79E18A", "#A6EA74") else c("#F08B4C", "#F0AE42", "#F0C94A", "#D7E352", "#D4E96E", "#C8EA6A", "#B4EA73", "#8BDC7B", "#74D7B6")
  xmax <- if (metric == "year") 2026 else 105
  xmin <- if (metric == "year") 1978 else -15
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = x, y = region, fill = region)) +
    ggridges::geom_density_ridges(scale = 1.1, rel_min_height = .01, alpha = .9,
                                  colour = "#5E5E5E", linewidth = .18) +
    ggplot2::geom_rug(sides = "b", length = grid::unit(.05, "npc"), alpha = .25, linewidth = .2) +
    ggplot2::scale_fill_manual(values = setNames(pal, region_levels), guide = "none") +
    ggplot2::scale_x_continuous(limits = c(xmin, xmax), breaks = if (metric == "year") seq(1980, 2020, 20) else c(0, 50, 100), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_classic(base_size = 6.1, base_family = "sans") +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 4.5), axis.text.x = ggplot2::element_text(size = 4.4),
                   axis.ticks.y = ggplot2::element_blank(), axis.line = ggplot2::element_line(linewidth = .3),
                   panel.grid.major.y = ggplot2::element_line(colour = "#D8D8D8", linewidth = .25, linetype = 3),
                   plot.margin = ggplot2::margin(3, 3, 3, 3))
  if (!marine && metric == "year") p <- p + ggplot2::labs(title = "Publication_Year")
  if (!marine && metric == "jif") p <- p + ggplot2::labs(title = "JIF.Category.Percentile.2020")
  if (marine && metric == "jif") p <- p + ggplot2::annotate("text", x = 103, y = 6.5, label = "Marine realms", angle = 90, size = 2.2)
  if (!marine && metric == "jif") p <- p + ggplot2::annotate("text", x = 103, y = 5.1, label = "Terrestrial realms", angle = 90, size = 2.2)
  p
}

build_plot <- function(seed = 20260909L) {
  a <- make_ridge(seed + 1, marine = FALSE, metric = "year")
  b <- make_ridge(seed + 2, marine = FALSE, metric = "jif")
  c <- make_ridge(seed + 3, marine = TRUE, metric = "year")
  d <- make_ridge(seed + 4, marine = TRUE, metric = "jif")
  patchwork::wrap_plots(a, b, c, d, ncol = 2, widths = c(1, 1))
}
