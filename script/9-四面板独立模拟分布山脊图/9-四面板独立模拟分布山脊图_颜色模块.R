# plot_nh shared color system
# Version: 1.0.0
#
# This file is the single project-level source of truth for colors used by
# executable R templates.  It intentionally has no required package imports;
# optional helpers use ggplot2, viridisLite, and colorspace only when called.

plot_nh_colors_version <- "1.0.0"

plot_nh_colors <- list(
  surface = list(
    panel = "#FFFFFF",
    paper = "#FFFFFF",
    grid = "#E6E9ED",
    border = "#1F2933",
    ink = "#1F2933",
    muted = "#667085",
    missing = "#B7BEC8"
  ),
  # Okabe-Ito / Wong inspired colors.  The core set is the default for
  # labels, thin lines, points, and small keys on a white background.
  categorical_core = c(
    blue = "#0072B2",
    vermillion = "#D55E00",
    teal = "#009E73",
    purple = "#CC79A7",
    black = "#000000"
  ),
  # Extended set for larger filled areas.  sky and yellow should normally
  # have a dark outline when used on a white background.
  categorical_extended = c(
    blue = "#0072B2",
    orange = "#E69F00",
    teal = "#009E73",
    vermillion = "#D55E00",
    purple = "#CC79A7",
    sky = "#56B4E9",
    yellow = "#F0E442",
    black = "#000000"
  ),
  # Paul Tol alternatives for figures that need more categories while
  # retaining a restrained, publication-oriented appearance.
  tol_bright = c(
    blue = "#4477AA",
    red = "#EE6677",
    green = "#228833",
    yellow = "#CCBB44",
    cyan = "#66CCEE",
    purple = "#AA3377",
    grey = "#BBBBBB"
  ),
  tol_muted = c(
    indigo = "#332288",
    cyan = "#88CCEE",
    teal = "#44AA99",
    green = "#117733",
    olive = "#999933",
    sand = "#DDCC77",
    rose = "#CC6677",
    wine = "#882255",
    purple = "#AA4499"
  ),
  diverging = list(
    blue_vermillion = c(low = "#2166AC", mid = "#F7F7F7", high = "#D55E00"),
    blue_red = c(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B")
  ),
  sequential_anchors = list(
    viridis = c("#440154", "#31688E", "#35B779", "#FDE725"),
    cividis = c("#00204C", "#666970", "#CAB969", "#FFEA46"),
    magma = c("#000004", "#721F81", "#F1605D", "#FCFDBF"),
    inferno = c("#000004", "#781C6D", "#ED6925", "#FCFFA4"),
    plasma = c("#0D0887", "#9C179E", "#ED7953", "#F0F921")
  )
)

plot_nh_color <- function(name, default = NULL) {
  parts <- strsplit(as.character(name), "\\.", fixed = FALSE)[[1L]]
  value <- plot_nh_colors
  for (part in parts) {
    if (!is.list(value) || is.null(value[[part]])) return(default)
    value <- value[[part]]
  }
  value
}

plot_nh_palette_categorical <- function(
  n = NULL,
  style = c("core", "extended", "tol_bright", "tol_muted"),
  labels = NULL,
  allow_fallback = FALSE
) {
  style <- match.arg(style)
  values <- switch(
    style,
    core = plot_nh_colors$categorical_core,
    extended = plot_nh_colors$categorical_extended,
    tol_bright = plot_nh_colors$tol_bright,
    tol_muted = plot_nh_colors$tol_muted
  )

  if (is.null(n)) {
    output <- values
  } else {
    n <- as.integer(n)
    if (length(n) != 1L || is.na(n) || n < 1L) {
      stop("n must be a positive integer.", call. = FALSE)
    }
    if (n <= length(values)) {
      output <- values[seq_len(n)]
    } else if (isTRUE(allow_fallback)) {
      warning(
        sprintf("%s has %d colors; using HCL fallback for %d groups.", style, length(values), n),
        call. = FALSE
      )
      output <- grDevices::hcl.colors(n, palette = "Dark 3")
    } else {
      stop(
        sprintf(
          "The %s palette has %d colors; use a supported palette or set allow_fallback = TRUE.",
          style,
          length(values)
        ),
        call. = FALSE
      )
    }
  }

  if (!is.null(labels)) {
    if (length(labels) != length(output)) {
      stop("labels must have the same length as the returned palette.", call. = FALSE)
    }
    names(output) <- as.character(labels)
  }
  output
}

plot_nh_palette_sequential <- function(
  option = c("viridis", "cividis", "magma", "inferno", "plasma"),
  n = 256L
) {
  option <- match.arg(option)
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 2L) {
    stop("n must be an integer greater than or equal to 2.", call. = FALSE)
  }
  if (requireNamespace("viridisLite", quietly = TRUE)) {
    return(viridisLite::viridis(n = n, option = option))
  }
  grDevices::colorRampPalette(
    plot_nh_colors$sequential_anchors[[option]],
    space = "Lab"
  )(n)
}

plot_nh_palette_diverging <- function(
  n = 3L,
  style = c("blue_vermillion", "blue_red")
) {
  style <- match.arg(style)
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 3L) {
    stop("n must be an integer greater than or equal to 3.", call. = FALSE)
  }
  anchors <- plot_nh_colors$diverging[[style]]
  if (n == 3L) return(anchors)
  unname(grDevices::colorRampPalette(unname(anchors), space = "Lab")(n))
}

plot_nh_color_context <- function() {
  options(
    plot_nh.colors = plot_nh_colors,
    plot_nh.colors.version = plot_nh_colors_version,
    plot_nh.palette.categorical = plot_nh_colors$categorical_core,
    plot_nh.palette.categorical.extended = plot_nh_colors$categorical_extended,
    plot_nh.palette.sequential = "viridis",
    plot_nh.palette.diverging = plot_nh_colors$diverging$blue_vermillion
  )
  invisible(plot_nh_colors)
}

# Compatibility aliases for existing parameterized adapters.  New code
# should call the functions above rather than hard-coding these aliases.
plot_nh_npg <- unname(plot_nh_colors$categorical_extended)
plot_nh_viridis <- plot_nh_palette_sequential("viridis", n = 4L)
plot_nh_rdbu <- unname(plot_nh_palette_diverging(3L, "blue_red"))

plot_nh_contrast_ratio <- function(foreground, background = "#FFFFFF") {
  luminance <- function(colour) {
    rgb <- grDevices::col2rgb(colour, alpha = FALSE) / 255
    rgb <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
    as.numeric(0.2126 * rgb[1L, ] + 0.7152 * rgb[2L, ] + 0.0722 * rgb[3L, ])
  }
  fg <- luminance(foreground)
  bg <- luminance(background)
  (pmax(fg, bg) + 0.05) / (pmin(fg, bg) + 0.05)
}

plot_nh_validate_palette <- function(
  colours,
  background = plot_nh_colors$surface$panel
) {
  colours <- as.character(colours)
  if (!length(colours)) {
    return(data.frame(
      colour = character(),
      contrast_to_background = numeric(),
      pass_graphic_3_to_1 = logical(),
      pass_text_4_5_to_1 = logical(),
      stringsAsFactors = FALSE
    ))
  }
  ratio <- plot_nh_contrast_ratio(colours, background = background)
  data.frame(
    colour = colours,
    contrast_to_background = round(ratio, 2L),
    pass_graphic_3_to_1 = ratio >= 3,
    pass_text_4_5_to_1 = ratio >= 4.5,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

plot_nh_cvd <- function(colours) {
  if (!requireNamespace("colorspace", quietly = TRUE)) {
    warning("Install colorspace to run color-vision-deficiency simulations.", call. = FALSE)
    return(NULL)
  }
  list(
    deutan = colorspace::deutan(colours),
    protan = colorspace::protan(colours),
    tritan = colorspace::tritan(colours)
  )
}

plot_nh_swatchplot <- function(colours, ...) {
  if (!requireNamespace("colorspace", quietly = TRUE)) {
    stop("Install colorspace to draw palette swatches.", call. = FALSE)
  }
  colorspace::swatchplot(colours, ...)
}

plot_nh_scale_colour_discrete <- function(values = NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Install ggplot2 to use plot_nh_scale_colour_discrete().", call. = FALSE)
  }
  if (is.null(values)) values <- plot_nh_palette_categorical()
  ggplot2::scale_colour_manual(values = values, ...)
}

plot_nh_scale_color_discrete <- plot_nh_scale_colour_discrete

plot_nh_scale_fill_discrete <- function(values = NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Install ggplot2 to use plot_nh_scale_fill_discrete().", call. = FALSE)
  }
  if (is.null(values)) values <- plot_nh_palette_categorical()
  ggplot2::scale_fill_manual(values = values, ...)
}

plot_nh_scale_colour_sequential <- function(option = "viridis", ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Install ggplot2 to use plot_nh_scale_colour_sequential().", call. = FALSE)
  }
  ggplot2::scale_colour_viridis_c(option = option, ...)
}

plot_nh_scale_color_sequential <- plot_nh_scale_colour_sequential

plot_nh_scale_fill_sequential <- function(option = "viridis", ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Install ggplot2 to use plot_nh_scale_fill_sequential().", call. = FALSE)
  }
  ggplot2::scale_fill_viridis_c(option = option, ...)
}

plot_nh_scale_colour_diverging <- function(
  low = plot_nh_colors$diverging$blue_vermillion[["low"]],
  mid = plot_nh_colors$diverging$blue_vermillion[["mid"]],
  high = plot_nh_colors$diverging$blue_vermillion[["high"]],
  midpoint = 0,
  ...
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Install ggplot2 to use plot_nh_scale_colour_diverging().", call. = FALSE)
  }
  ggplot2::scale_colour_gradient2(low = low, mid = mid, high = high, midpoint = midpoint, ...)
}

plot_nh_scale_color_diverging <- plot_nh_scale_colour_diverging

plot_nh_scale_fill_diverging <- function(
  low = plot_nh_colors$diverging$blue_vermillion[["low"]],
  mid = plot_nh_colors$diverging$blue_vermillion[["mid"]],
  high = plot_nh_colors$diverging$blue_vermillion[["high"]],
  midpoint = 0,
  ...
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Install ggplot2 to use plot_nh_scale_fill_diverging().", call. = FALSE)
  }
  ggplot2::scale_fill_gradient2(low = low, mid = mid, high = high, midpoint = midpoint, ...)
}

invisible(plot_nh_color_context())

# Formatting is an optional sibling module; it changes nothing until a template calls it.
local({
  format_module <- Sys.getenv("PLOT_NH_FORMAT_MODULE", unset = "")
  if (!nzchar(format_module)) {
    sources <- vapply(sys.frames(), function(frame) if (is.null(frame$ofile)) "" else frame$ofile, character(1L))
    sources <- sources[nzchar(sources)]
    if (length(sources)) format_module <- file.path(dirname(tail(sources, 1L)), "plot_nh_format.R")
  }
  if (nzchar(format_module) && file.exists(format_module)) source(format_module, local = .GlobalEnv)
})
