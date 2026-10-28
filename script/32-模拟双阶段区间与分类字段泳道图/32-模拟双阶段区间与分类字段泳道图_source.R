required_packages <- c("ggplot2", "patchwork")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Missing R packages: ", paste(missing_packages, collapse = ", "))

make_legend_panel <- function() {
  items <- data.frame(x = c(0.01, .18, .35, .52, .69, .85),
                      label = c("Treatment", "Best Response", "Treatment Status", "Prior Platinum", "Endocrine Tx", "CDK Inhibitor"),
                      detail = c("cdk | other | pap", "CR | PR | SD", "progression | ongoing\nswitch treatment", "No | Yes", "AI | SERD", "Abemaciclib | Palbociclib\nRibociclib"),
                      colour = c("#E26B72", "#E2E2E2", "#D6D6D6", "#86B58F", "#A88DC4", "#D78383"))
  ggplot2::ggplot() +
    ggplot2::geom_text(data = items, ggplot2::aes(x, .82, label = label), hjust = 0, size = 1.75, fontface = "bold") +
    ggplot2::geom_rect(data = items, ggplot2::aes(xmin = x, xmax = x + .025, ymin = .42, ymax = .64), fill = items$colour, colour = "#777777", linewidth = .18) +
    ggplot2::geom_text(data = items, ggplot2::aes(x, .24, label = detail), hjust = 0, size = 1.28, lineheight = .9) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
    ggplot2::theme_void() + ggplot2::theme(plot.margin = ggplot2::margin(0, 2, 0, 2))
}

build_plot <- function(seed = 20260909L) {
  set.seed(seed)
  n <- 34
  rows <- seq_len(n)
  left_cols <- c("#9BCEB5", "#E28FAA", "#D3D5E9", "#F1B3C3", "#EFC46F")
  tiles <- expand.grid(y = rows, x = 1:5)
  tiles$fill <- sample(left_cols, nrow(tiles), replace = TRUE, prob = c(.24, .23, .17, .22, .14))
  finish <- sort(sample(seq(12, 85, by = 2), n, replace = TRUE))
  base_end <- pmax(5, round(finish * runif(n, .12, .38)))
  segments <- rbind(
    data.frame(y = rows, x = 0, xend = finish, colour = "#D1D1D1", linewidth = 3.5),
    data.frame(y = rows, x = 0, xend = base_end, colour = "#ED6C70", linewidth = 2.2),
    data.frame(y = rows, x = base_end, xend = finish, colour = "#678EBF", linewidth = 2.0)
  )
  marks <- do.call(rbind, lapply(rows, function(i) {
    x <- sort(c(runif(1, 2, max(3, base_end[i] - 1)), runif(1, base_end[i], max(base_end[i] + 1, finish[i] - 2))))
    data.frame(y = i, x = x, mark = c("open", "filled"))
  }))
  marks$mark <- factor(marks$mark, levels = c("open", "filled"))
  main <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = tiles, ggplot2::aes(x = x - 8.5, y = y, fill = fill), width = .78, height = .72,
                       colour = "#777777", linewidth = .18) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_segment(data = segments, ggplot2::aes(x = x, xend = xend, y = y, yend = y, colour = colour, linewidth = linewidth),
                          lineend = "butt", show.legend = FALSE) +
    ggplot2::scale_colour_identity() + ggplot2::scale_linewidth_identity() +
    ggplot2::geom_point(data = marks, ggplot2::aes(x, y, shape = mark), size = 1.2, colour = "#4E6B82", stroke = .35) +
    ggplot2::scale_shape_manual(values = c(open = 1, filled = 16), guide = "none") +
    ggplot2::scale_x_continuous(breaks = c(0, 20, 40, 60, 80), labels = c(0, 20, 40, 60, 80), limits = c(-10, 88), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(.2, n + .8), expand = c(0, 0)) +
    ggplot2::labs(x = "Time (months)", y = NULL) +
    ggplot2::theme_classic(base_size = 7, base_family = "sans") +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size = 5), axis.title.x = ggplot2::element_text(size = 6),
                   axis.line = ggplot2::element_line(linewidth = .35), plot.margin = ggplot2::margin(3, 3, 0, 3))
  main / make_legend_panel() + patchwork::plot_layout(heights = c(5.4, 1.2))
}
