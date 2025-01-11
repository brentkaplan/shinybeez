box::use(
  ggplot2,
  grid,
  png,
)

#' @export
get_png_br <- function(filename) {
  grid$rasterGrob(png$readPNG(filename),
                   interpolate = TRUE,
                   x = grid$unit(1, "npc"),
                   y = grid$unit(0, "npc") + grid$unit(35, "pt"),
                   height = grid$unit(50, "pt"),
                   hjust = 1,
                   vjust = 1
  )
}

#' @export
get_png_tr <- function(filename) {
  grid$rasterGrob(png$readPNG(filename),
                  interpolate = TRUE,
                  x = grid$unit(1, "npc") - grid$unit(10, "pt"),
                  y = grid$unit(1, "npc"),
                  height = grid$unit(50, "pt"),
                  hjust = 1,
                  vjust = 1
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

#' @export
geomean <- function(x) {
  return(round(exp(mean(log((x + 1)))) - 1, 2))
}
