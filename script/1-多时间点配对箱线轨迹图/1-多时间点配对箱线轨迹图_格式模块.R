# Explicit visual adapter. It never changes source data, statistics or global ggplot methods.
plot_nh_format_pt_per_mm <- 72.27 / 25.4

.plot_nh_clone_proto <- function(parent) {
  force(parent)
  ggplot2::ggproto(NULL, parent)
}

plot_nh_format_output <- function(cfg) {
  args <- commandArgs(trailingOnly = TRUE)
  at <- match("--output-dir", args)
  if (!is.na(at) && at < length(args)) return(args[[at + 1L]])
  inline <- grep("^--output-dir=", args, value = TRUE)
  if (length(inline)) return(sub("^--output-dir=", "", inline[[1L]]))
  if (!is.null(cfg$output_dir)) return(cfg$output_dir)
  tempdir()
}

plot_nh_format_hex <- function(values) {
  if (!length(values)) return(character())
  vapply(as.character(values), function(value) {
    if (is.na(value) || !nzchar(value)) return(NA_character_)
    tryCatch({
      rgb <- grDevices::col2rgb(value, alpha = TRUE)
      grDevices::rgb(rgb[1L, 1L], rgb[2L, 1L], rgb[3L, 1L], maxColorValue = 255)
    }, error = function(e) NA_character_)
  }, character(1L), USE.NAMES = FALSE)
}

plot_nh_format_palette <- function(palette, groups, cfg) {
  style <- cfg$format
  if (!length(style$global) && !length(style$datasets)) return(palette)
  if (is.null(palette) || !length(groups)) return(palette)
  groups <- as.character(groups)
  original_names <- names(palette)
  if (length(palette) != length(groups) || anyDuplicated(groups)) return(palette)
  if (length(original_names) && all(groups %in% original_names)) palette <- palette[groups]
  if (!is.null(style$global$color)) palette[] <- style$global$color
  for (key in names(style$datasets)) {
    color <- style$datasets[[key]]$color
    if (!is.null(color)) {
      if (!key %in% groups) stop("Unknown visual dataset: ", key, call. = FALSE)
      palette[match(key, groups)] <- color
    }
  }
  if (length(original_names)) names(palette) <- if (all(groups %in% original_names)) groups else original_names
  palette
}

plot_nh_parameter_context <- function(cfg, output_dir, groups = NULL, palette = NULL) {
  if (!isTRUE(cfg$format_context_only)) return(invisible(NULL))
  keys <- unique(as.character(groups))
  keys <- keys[!is.na(keys) & nzchar(keys)]
  cols <- character()
  if (length(palette) == length(keys) && length(keys)) {
    if (length(names(palette)) && all(keys %in% names(palette))) palette <- palette[keys]
    cols <- plot_nh_format_hex(palette)
  }
  if (length(cols) != length(keys) || anyNA(cols)) keys <- character()
  supported <- unlist(cfg$format_control_ids, use.names = FALSE)
  if (!length(keys)) supported <- setdiff(supported, "color")
  dataset_controls <- if (length(keys)) "color" else character()
  defaults <- cfg$format_parameter_defaults
  if (is.null(defaults)) defaults <- list()
  result <- list(supported = as.list(supported), dataset_controls = as.list(dataset_controls),
                 datasets = lapply(seq_along(keys), function(i) list(key = keys[[i]], label = keys[[i]], color = cols[[i]])),
                 columns = list(), defaults = defaults)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(result, file.path(output_dir, "format_context.json"), auto_unbox = TRUE, null = "null", digits = 15)
  quit(save = "no", status = 0L)
}

.plot_nh_parts <- function(p) {
  if (inherits(p, "patchwork")) c(p$patches$plots, list(p)) else list(p)
}

.plot_nh_layer_kind <- function(layer) {
  classes <- class(layer$geom)
  if (any(grepl("Errorbar|Linerange|Pointrange|Crossbar", classes))) return("error")
  if ("GeomPoint" %in% classes) return("point")
  if (any(classes %in% c("GeomLine", "GeomPath", "GeomStep", "GeomSegment", "GeomSmooth"))) return("line")
  if ("GeomBoxplot" %in% classes) return("box")
  if ("GeomViolin" %in% classes) return("violin")
  if (any(classes %in% c("GeomBar", "GeomCol", "GeomRect"))) return("bar")
  "other"
}

.plot_nh_discrete_palette <- function(built) {
  result <- list()
  for (aes in c("colour", "fill")) {
    scale <- built$plot$scales$get_scales(aes)
    if (!is.null(scale) && isTRUE(scale$is_discrete())) {
      keys <- as.character(scale$get_limits())
      keys <- keys[!is.na(keys)]
      if (length(keys)) result[[aes]] <- stats::setNames(as.character(scale$map(keys)), keys)
    }
  }
  result
}

.plot_nh_context_single <- function(p) {
  built <- ggplot2::ggplot_build(p)
  kinds <- vapply(p$layers, .plot_nh_layer_kind, character(1L))
  palettes <- .plot_nh_discrete_palette(built)
  groups <- unique(unlist(lapply(palettes, names), use.names = FALSE))
  colors <- stats::setNames(rep(NA_character_, length(groups)), groups)
  for (palette in palettes) colors[names(palette)] <- plot_nh_format_hex(palette)
  supported <- c("font_family", "axis_title_size", "axis_text_size", "legend_title_size", "legend_text_size",
                 "show_legend", "legend_position", "legend_spacing", "background", "grid", "axis_line_width", "tick_length",
                 "width_mm", "height_mm", "dpi")
  if (any(kinds != "other")) supported <- c(supported, "color", "alpha")
  if (any(kinds %in% c("point", "box", "violin", "bar"))) supported <- c(supported, "border_color", "border_width")
  if ("point" %in% kinds) supported <- c(supported, "point_shape", "point_size")
  if ("line" %in% kinds) supported <- c(supported, "line_type", "line_width")
  if ("bar" %in% kinds) supported <- c(supported, "bar_width", "fill")
  if ("box" %in% kinds) supported <- c(supported, "box_width", "fill")
  if ("violin" %in% kinds) supported <- c(supported, "violin_width", "fill")
  if ("error" %in% kinds) supported <- c(supported, "error_show", "error_color", "error_line_width", "error_cap_width")
  axes <- list()
  if (inherits(p$coordinates, "CoordCartesian")) {
    for (axis in c("x", "y")) {
      scale <- built$plot$scales$get_scales(axis)
      discrete <- !is.null(scale) && isTRUE(scale$is_discrete())
      supported <- c(supported, paste0(axis, "_label"))
      is_date <- !is.null(scale) && inherits(scale, c("ScaleContinuousDate", "ScaleContinuousDatetime"))
      if (!discrete && !is_date) {
        supported <- c(supported, paste0(axis, c("_min", "_max", "_reverse", "_tick_step")))
        if (!inherits(p$coordinates, c("CoordFlip", "CoordFixed", "CoordTrans", "CoordTransform")) &&
            (is.null(scale) || identical(scale$trans$name, "identity"))) supported <- c(supported, paste0(axis, "_scale"))
        values <- unlist(lapply(built$data, function(data) unlist(data[intersect(names(data), c(axis, paste0(axis, c("min", "max", "end"))))], use.names = FALSE)), use.names = FALSE)
        values <- values[is.finite(values)]
        axes[[axis]] <- list(nonpositive = any(values <= 0), minimum = if (length(values)) min(values) else NULL,
                             maximum = if (length(values)) max(values) else NULL)
      }
    }
  }
  # Continuous aesthetic mappings retain their scientific encoding.
  encoded <- character()
  for (aes in c("colour", "fill", "size", "alpha", "linewidth", "shape", "linetype")) {
    scale <- built$plot$scales$get_scales(aes)
    if (!is.null(scale) && !isTRUE(scale$is_discrete())) encoded <- c(encoded, aes)
  }
  blocked <- unlist(list(if ("colour" %in% encoded) c("color", "border_color"), if ("fill" %in% encoded) c("color", "fill"),
                         if ("size" %in% encoded) "point_size", if ("alpha" %in% encoded) "alpha",
                         if ("linewidth" %in% encoded) "line_width"), use.names = FALSE)
  supported <- setdiff(supported, blocked)
  dataset_controls <- intersect(supported, c("color", "fill", "point_shape", "point_size", "alpha", "line_type", "line_width", "border_color", "border_width"))
  if (!length(groups) || any(vapply(palettes, function(x) anyDuplicated(tolower(x)) > 0L, logical(1L)))) dataset_controls <- character()
  defaults <- list()
  scalar <- function(values) {
    values <- unique(values[!is.na(values)])
    if (length(values) == 1L) values[[1L]] else NULL
  }
  geometry_values <- function(kind, aes) unlist(lapply(which(kinds == kind), function(i) built$data[[i]][[aes]]), use.names = FALSE)
  point_size <- scalar(geometry_values("point", "size"))
  if (!is.null(point_size)) defaults$point_size <- point_size * plot_nh_format_pt_per_mm
  linewidth <- scalar(geometry_values("line", "linewidth"))
  if (!is.null(linewidth)) defaults$line_width <- linewidth * plot_nh_format_pt_per_mm
  point_shape <- scalar(geometry_values("point", "shape"))
  shapes <- c(`16` = "circle", `15` = "square", `17` = "triangle", `18` = "diamond", `4` = "cross")
  if (!is.null(point_shape) && as.character(point_shape) %in% names(shapes)) defaults$point_shape <- unname(shapes[as.character(point_shape)])
  theme <- ggplot2::theme_get() + p$theme
  visible_axis_text <- vapply(c("axis.text.x", "axis.text.y"), function(name) inherits(ggplot2::calc_element(name, theme), "element_text"), logical(1L))
  if (!any(visible_axis_text)) supported <- setdiff(supported, "axis_text_size")
  for (entry in list(c("axis_title_size", "axis.title.x"), c("axis_text_size", "axis.text.x"), c("legend_text_size", "legend.text"), c("legend_title_size", "legend.title"))) {
    element <- tryCatch(ggplot2::calc_element(entry[[2L]], theme), error = function(e) NULL)
    if (inherits(element, "element_text") && !is.null(element$size)) defaults[[entry[[1L]]]] <- element$size
  }
  labels <- ggplot2::get_labs(p)
  for (axis in c("x", "y")) if (is.character(labels[[axis]]) && length(labels[[axis]]) == 1L) defaults[[paste0(axis, "_label")]] <- labels[[axis]]
  list(supported = unique(supported), dataset_controls = dataset_controls, groups = groups, colors = colors,
       palettes = palettes, axes = axes, built = built, defaults = defaults, columns = names(p$data))
}

.plot_nh_apply_rows <- function(data, style, kind, palettes) {
  if (!nrow(data)) return(data)
  keys <- rep(NA_character_, nrow(data))
  for (aes in intersect(names(palettes), names(data))) {
    palette <- palettes[[aes]]
    matched <- match(tolower(as.character(data[[aes]])), tolower(unname(palette)))
    found <- !is.na(matched)
    keys[found] <- names(palette)[matched[found]]
  }
  apply_value <- function(cid, column, convert = identity) {
    base <- style$global[[cid]]
    if (!is.null(base)) data[[column]] <<- rep(convert(base), nrow(data))
    for (key in names(style$datasets)) {
      value <- style$datasets[[key]][[cid]]
      rows <- !is.na(keys) & keys == key
      if (!is.null(value) && any(rows)) {
        if (is.null(data[[column]])) next
        data[[column]][rows] <<- convert(value)
      }
    }
  }
  as_mm <- function(x) as.numeric(x) / plot_nh_format_pt_per_mm
  if (kind == "point") {
    apply_value("point_size", "size", as_mm)
    apply_value("point_shape", "shape", function(x) c(circle = 16L, square = 15L, triangle = 17L, diamond = 18L, cross = 4L)[[x]])
    apply_value("border_width", "stroke", as_mm)
  }
  if (kind == "line") {
    apply_value("line_width", "linewidth", as_mm)
    apply_value("line_type", "linetype")
  }
  if (kind %in% c("box", "violin", "bar")) {
    apply_value("color", "fill")
    apply_value("fill", "fill")
    apply_value("border_color", "colour")
    apply_value("border_width", "linewidth", as_mm)
  } else if (kind != "other") {
    # Set both point channels: solid symbols use colour while shapes 21–25
    # use fill. A separate border override still wins below.
    apply_value("color", "colour")
    if (kind == "point") apply_value("color", "fill")
    if (kind == "point") apply_value("border_color", "colour")
  }
  if (kind != "other") apply_value("alpha", "alpha")
  if (kind == "error") {
    apply_value("error_color", "colour")
    apply_value("error_line_width", "linewidth", as_mm)
  }
  data
}

.plot_nh_apply_single <- function(p, style, context) {
  values <- style$global
  requested <- unique(c(names(values), unlist(lapply(style$datasets, names), use.names = FALSE)))
  invalid <- setdiff(requested, context$supported)
  if (length(invalid)) stop("Current plot cannot apply visual control: ", paste(invalid, collapse = ", "), call. = FALSE)
  for (i in seq_along(p$layers)) {
    original_layer <- p$layers[[i]]
    kind <- .plot_nh_layer_kind(original_layer)
    if (kind == "other") next
    layer <- .plot_nh_clone_proto(original_layer)
    geom <- original_layer$geom
    palettes <- context$palettes
    if (kind == "error" && identical(values$error_show, FALSE)) {
      layer$geom <- ggplot2::ggproto("PlotNhHiddenError", geom, draw_panel = function(...) grid::nullGrob(), draw_key = function(...) grid::nullGrob())
    } else {
      layer$geom <- local({
        original <- geom; current_kind <- kind; current_palettes <- palettes; current_style <- style
        ggplot2::ggproto("PlotNhStyledGeom", original,
          draw_panel = function(data, panel_params, coord, ...) {
            data <- .plot_nh_apply_rows(data, current_style, current_kind, current_palettes)
            original$draw_panel(data, panel_params, coord, ...)
          },
          draw_key = function(data, params, size) {
            original$draw_key(.plot_nh_apply_rows(data, current_style, current_kind, current_palettes), params, size)
          })
      })
    }
    width_key <- switch(kind, bar = "bar_width", box = "box_width", violin = "violin_width", error = "error_cap_width", NULL)
    if (!is.null(width_key) && !is.null(values[[width_key]])) {
      layer$geom_params <- c(layer$geom_params, list())
      layer$stat_params <- c(layer$stat_params, list())
      layer$geom_params$width <- values[[width_key]]
      layer$stat_params$width <- values[[width_key]]
    }
    p$layers[[i]] <- layer
  }
  theme_args <- list()
  if (!is.null(values$font_family)) theme_args$text <- ggplot2::element_text(family = values$font_family)
  original_theme <- ggplot2::theme_get() + p$theme
  for (entry in list(c("axis_title_size", "axis.title.x"), c("axis_title_size", "axis.title.y"), c("axis_text_size", "axis.text.x"), c("axis_text_size", "axis.text.y"), c("legend_title_size", "legend.title"), c("legend_text_size", "legend.text"))) {
    if (startsWith(entry[[2L]], "axis.") && !inherits(ggplot2::calc_element(entry[[2L]], original_theme), "element_text")) next
    if (!is.null(values[[entry[[1L]]]])) theme_args[[entry[[2L]]]] <- ggplot2::element_text(size = values[[entry[[1L]]]])
  }
  if (!is.null(values$show_legend) && !values$show_legend) theme_args$legend.position <- "none"
  else if (!is.null(values$legend_position)) theme_args$legend.position <- values$legend_position
  else if (isTRUE(values$show_legend) && identical(original_theme$legend.position, "none")) theme_args$legend.position <- "right"
  if (!is.null(values$legend_spacing)) theme_args$legend.spacing <- grid::unit(values$legend_spacing, "mm")
  if (!is.null(values$background)) {
    theme_args$plot.background <- ggplot2::element_rect(fill = values$background, colour = NA)
    theme_args$panel.background <- ggplot2::element_rect(fill = values$background, colour = NA)
  }
  if (!is.null(values$grid)) {
    theme_args$panel.grid.major <- if (values$grid == "none") ggplot2::element_blank() else ggplot2::element_line(colour = "#E6E9ED", linewidth = 0.2)
    theme_args$panel.grid.minor <- if (values$grid != "both") ggplot2::element_blank() else ggplot2::element_line(colour = "#F0F2F4", linewidth = 0.1)
  }
  if (!is.null(values$axis_line_width)) {
    theme_args$axis.line <- ggplot2::element_line(linewidth = values$axis_line_width / plot_nh_format_pt_per_mm)
    theme_args$axis.ticks <- ggplot2::element_line(linewidth = values$axis_line_width / plot_nh_format_pt_per_mm)
  }
  if (!is.null(values$tick_length)) theme_args$axis.ticks.length <- grid::unit(values$tick_length, "mm")
  if (length(theme_args)) p <- p + do.call(ggplot2::theme, theme_args)
  for (axis in c("x", "y")) {
    if (!is.null(values[[paste0(axis, "_label")]])) p <- p + do.call(ggplot2::labs, stats::setNames(list(values[[paste0(axis, "_label")]]), axis))
  }
  if (inherits(p$coordinates, "CoordCartesian")) {
    coord <- .plot_nh_clone_proto(p$coordinates)
    coord$limits <- as.list(coord$limits)
    for (axis in c("x", "y")) {
      limits <- coord$limits[[axis]]
      if (!is.null(values[[paste0(axis, "_min")]]) || !is.null(values[[paste0(axis, "_max")]])) {
        if (is.null(limits)) limits <- c(NA_real_, NA_real_)
        for (i in 1:2) {
          value <- values[[paste0(axis, c("_min", "_max")[[i]])]]
          if (!is.null(value)) limits[[i]] <- value
        }
        coord$limits[[axis]] <- limits
      }
      interval <- values[[paste0(axis, "_tick_step")]]
      if (!is.null(interval)) {
        breaks <- local({ step <- interval; function(limits) {
          first <- ceiling(min(limits) / step) * step; last <- floor(max(limits) / step) * step
          if (!is.finite(first) || !is.finite(last) || last < first) return(numeric())
          if ((last - first) / step > 2000) stop("Too many axis ticks", call. = FALSE)
          seq(first, last, by = step)
        }})
        scale <- p$scales$get_scales(axis)
        if (is.null(scale)) p <- p + (if (axis == "x") ggplot2::scale_x_continuous(breaks = breaks) else ggplot2::scale_y_continuous(breaks = breaks))
        else { scale <- scale$clone(); scale$breaks <- breaks; p$scales$add(scale) }
      }
    }
    if (!is.null(values$x_reverse) || !is.null(values$y_reverse)) {
      old_reverse <- if (is.null(coord$reverse)) "none" else coord$reverse
      reverse_x <- if (is.null(values$x_reverse)) grepl("x", old_reverse) else isTRUE(values$x_reverse)
      reverse_y <- if (is.null(values$y_reverse)) grepl("y", old_reverse) else isTRUE(values$y_reverse)
      coord$reverse <- paste0(if (reverse_x) "x" else "", if (reverse_y) "y" else "")
      if (!nzchar(coord$reverse)) coord$reverse <- "none"
    }
    p$coordinates <- coord
    log_x <- identical(values$x_scale, "log10"); log_y <- identical(values$y_scale, "log10")
    if (log_x || log_y) {
      for (axis in c(if (log_x) "x", if (log_y) "y")) {
        if (isTRUE(context$axes[[axis]]$nonpositive)) stop("Log10 coordinate requires positive data and interval endpoints: ", axis, call. = FALSE)
        scale <- context$built$plot$scales$get_scales(axis)
        if (!is.null(scale) && !identical(scale$trans$name, "identity")) stop("Cannot apply log10 twice to an already transformed axis", call. = FALSE)
      }
      p$coordinates <- ggplot2::coord_transform(x = if (log_x) "log10" else "identity", y = if (log_y) "log10" else "identity", xlim = coord$limits$x, ylim = coord$limits$y, clip = coord$clip)
      if (!is.null(coord$reverse)) p$coordinates$reverse <- coord$reverse
    }
  }
  p
}

plot_nh_apply_format <- function(p, cfg, output_dir) {
  style <- cfg$format
  if (!isTRUE(cfg$format_context_only) && !length(style$global) && !length(style$datasets)) return(p)
  if (!inherits(p, "ggplot")) stop("Explicit format adapter requires a ggplot object", call. = FALSE)
  parts <- .plot_nh_parts(p)
  contexts <- lapply(parts, .plot_nh_context_single)
  if (isTRUE(cfg$format_context_only)) {
    groups <- unique(unlist(lapply(contexts, `[[`, "groups"), use.names = FALSE))
    palette <- stats::setNames(rep(NA_character_, length(groups)), groups)
    for (context in contexts) palette[names(context$colors)] <- context$colors
    supported <- unique(unlist(lapply(contexts, `[[`, "supported"), use.names = FALSE))
    if (length(parts) > 1L) supported <- supported[!grepl("^[xy]_", supported)]
    dataset_controls <- unique(unlist(lapply(contexts, `[[`, "dataset_controls"), use.names = FALSE))
    defaults <- cfg$format_parameter_defaults
    if (is.null(defaults)) defaults <- list()
    if (length(contexts) == 1L) defaults <- utils::modifyList(defaults, contexts[[1L]]$defaults)
    result <- list(supported = as.list(supported), dataset_controls = as.list(dataset_controls),
                   datasets = lapply(groups, function(key) list(key = key, label = key, color = unname(palette[[key]]))),
                   columns = as.list(unique(unlist(lapply(contexts, `[[`, "columns"), use.names = FALSE))), defaults = defaults, axes = contexts[[1L]]$axes)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(result, file.path(output_dir, "format_context.json"), auto_unbox = TRUE, null = "null", digits = 15)
    quit(save = "no", status = 0L)
  }
  if (inherits(p, "patchwork")) {
    part_style <- function(context) {
      list(global = style$global[intersect(names(style$global), context$supported)],
           datasets = lapply(style$datasets, function(x) x[intersect(names(x), context$dataset_controls)]))
    }
    for (i in seq_along(p$patches$plots)) {
      p$patches$plots[[i]] <- .plot_nh_apply_single(p$patches$plots[[i]], part_style(contexts[[i]]), contexts[[i]])
    }
    style <- part_style(contexts[[length(contexts)]])
  }
  .plot_nh_apply_single(p, style, contexts[[length(contexts)]])
}
