box::use(
  ggplot2,
  grid,
  png,
  grDevices,
)

#' @export
get_png_br <- function(filename, height_pt = 70, alpha = 0.9) {
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
get_png_tr <- function(filename, height_pt = 70, alpha = 0.9) {
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

#' @export
watermark_br <- get_png_br("./app/static/img/shinybeez-watermark-alpha.png")

#' @export
watermark_tr <- get_png_tr("./app/static/img/shinybeez-watermark-alpha.png")

# -----------------------------------------------------------------------------
# Palette helpers (discrete)
# -----------------------------------------------------------------------------

#' Get a vector of colors for a named discrete palette
#'
#' @param name Character palette name. Supported: "Okabe-Ito", "HCL Light",
#'   "HCL Dark".
#' @param n Integer number of colors required.
#' @return Character vector of hex colors of length n.
#' @export
get_palette_colors <- function(name = "Okabe-Ito", n = 2L) {
  if (is.null(n) || is.na(n) || n <= 0) return(character(0))

  if (is.null(name) || is.na(name) || !nzchar(name)) name <- "Okabe-Ito"
  name <- as.character(name)

  # Okabe-Ito colorblind-safe base (8 colors)
  okabe_ito <- c(
    "#000000", "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
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

#' @export
geomean <- function(x) {
  return(round(exp(mean(log((x + 1)))) - 1, 2))
}
