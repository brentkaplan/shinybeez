box::use(
  ggplot2,
  grid,
  png,
  grDevices,
)

#' @export
get_png_br <- function(filename, height_pt = 50, alpha = 0.9) {
  grid$rasterGrob(
    png$readPNG(filename),
    interpolate = TRUE,
    x = grid$unit(1, "npc"),
    y = grid$unit(0, "npc") + grid$unit(35, "pt"),
    height = grid$unit(height_pt, "pt"),
    hjust = 1,
    vjust = 1,
    gp = grid$gpar(alpha = alpha)
  )
}

#' @export
get_png_tr <- function(filename, height_pt = 50, alpha = 0.9) {
  grid$rasterGrob(
    png$readPNG(filename),
    interpolate = TRUE,
    x = grid$unit(1, "npc") - grid$unit(10, "pt"),
    y = grid$unit(1, "npc"),
    height = grid$unit(height_pt, "pt"),
    hjust = 1,
    vjust = 1,
    gp = grid$gpar(alpha = alpha)
  )
}

#' @export
add_shiny_logo <- function(logo) {
  list(
    ggplot2$annotation_custom(logo),
    ggplot2$coord_cartesian(clip = "off"),
    ggplot2$theme(plot.margin = ggplot2$unit(c(1, 1, 3, 1), "lines"))
  )
}

# Eagerly load the decorative watermark grobs. Wrapped in tryCatch so the
# module stays importable when the working directory is not the app root
# (e.g. testthat runs from tests/testthat, where the relative path won't
# resolve). The grobs are only consumed by render paths, which always run
# with the app root as the working directory, so production behaviour is
# unchanged; only out-of-app contexts get NULL.
#' @export
watermark_br <- tryCatch(
  get_png_br("./app/static/img/shinybeez-watermark-alpha.png"),
  error = function(e) NULL
)

#' @export
watermark_tr <- tryCatch(
  get_png_tr("./app/static/img/shinybeez-watermark-alpha.png"),
  error = function(e) NULL
)

# -----------------------------------------------------------------------------
# Palette helpers (discrete)
# -----------------------------------------------------------------------------

#' Get a vector of colors for a named discrete palette
#'
#' @param name Character palette name. Supported: "Codedbx" (default, brand),
#'   "Okabe-Ito" (colorblind-safe), "HCL Light", "HCL Dark". The name is matched
#'   case-insensitively for the brand palette.
#' @param n Integer number of colors required.
#' @return Character vector of hex colors of length n.
#' @export
get_palette_colors <- function(name = "Codedbx", n = 2L) {
  if (is.null(n) || is.na(n) || n <= 0) {
    return(character(0))
  }

  if (is.null(name) || is.na(name) || !nzchar(name)) {
    name <- "Codedbx"
  }
  name <- as.character(name)

  # codedbx "Refined Contemporary" brand palette (6 colors). Default so that
  # grouped curves carry the brand identity; recycles for n > 6.
  codedbx <- c(
    "#534B7A",
    "#A25F5F",
    "#5D8AA8",
    "#7D9C7F",
    "#2B4560",
    "#B08C6A"
  )

  if (identical(tolower(name), "codedbx")) {
    return(rep(codedbx, length.out = n))
  }

  # Okabe-Ito colorblind-safe base (8 colors)
  okabe_ito <- c(
    "#000000",
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7"
  )

  if (identical(name, "Okabe-Ito")) {
    # Recycle if more than 8 categories
    return(rep(okabe_ito, length.out = n))
  }

  # Generic HCL-based palettes for arbitrary n
  hues <- seq(15, 375, length.out = n + 1)[1:n]
  if (identical(name, "HCL Light")) {
    return(grDevices$hcl(h = hues, c = 45, l = 85))
  } else if (identical(name, "HCL Dark")) {
    return(grDevices$hcl(h = hues, c = 100, l = 45))
  } else {
    # Fallback to HCL Light
    return(grDevices$hcl(h = hues, c = 45, l = 85))
  }
}

#' Apply dark mode styling to a ggplot object
#'
#' Paints plot backgrounds with the app's dark page color and adjusts
#' text/grid colors for dark mode contexts. Applies no-op when dark_mode is
#' "light". A solid fill (rather than transparent) is used because the plots
#' render to an opaque PNG canvas, so a transparent fill would show through as
#' white; the fill matches the app's dark `--bs-body-bg`/card background so the
#' image blends seamlessly with the page.
#'
#' @param p A ggplot object
#' @param dark_mode Character, "dark" or "light"
#' @return Modified ggplot object
#' @export
apply_dark_mode_theme <- function(p, dark_mode = "light") {
  if (!identical(dark_mode, "dark")) {
    return(p)
  }

  bg_color <- "#2d2d2d"
  text_color <- "#dee2e6"
  grid_color <- "#495057"

  p +
    ggplot2$theme(
      plot.background = ggplot2$element_rect(fill = bg_color, color = NA),
      panel.background = ggplot2$element_rect(fill = bg_color, color = NA),
      legend.background = ggplot2$element_rect(fill = bg_color, color = NA),
      legend.key = ggplot2$element_rect(fill = bg_color, color = NA),
      text = ggplot2$element_text(color = text_color),
      axis.text = ggplot2$element_text(color = text_color),
      axis.title = ggplot2$element_text(color = text_color),
      plot.title = ggplot2$element_text(color = text_color),
      plot.subtitle = ggplot2$element_text(color = text_color),
      legend.text = ggplot2$element_text(color = text_color),
      legend.title = ggplot2$element_text(color = text_color),
      panel.grid.major = ggplot2$element_line(color = grid_color),
      panel.grid.minor = ggplot2$element_line(color = grid_color),
      axis.ticks = ggplot2$element_line(color = grid_color)
    )
}

#' @export
geomean <- function(x) {
  return(round(exp(mean(log((x + 1)))) - 1, 2))
}
