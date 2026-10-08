#' Plot style: schema, defaults, validation and engine adapters
#'
#' Pure functions shared by every plotting tab. A *style* is a nested list
#' (see `style_defaults()`); hosts assemble one from their inputs, validate it
#' once, and hand it to `plot_args_from_style()` to splice package-specific
#' arguments into `plot()`. Nothing here touches Shiny.

ENGINES <- c("beezdemand_nlme", "beezdemand_tmb") # nolint: object_name_linter

# Package defaults, pinned rather than read from the installed package so app
# behaviour does not change under a silent package upgrade. The contract test in
# tests/testthat/test-plot_style.R fails loudly if beezdemand drifts.
engine_defaults <- list(
  beezdemand_nlme = list(
    population = list(show = TRUE, alpha = 0.9, width = 1.0),
    individual = list(show = FALSE, alpha = 0.3, width = 0.6),
    observed = list(show = TRUE, alpha = 0.6, size = 2.0)
  ),
  beezdemand_tmb = list(
    population = list(show = TRUE, alpha = 1.0, width = 1.2),
    individual = list(show = FALSE, alpha = 0.3, width = 0.5),
    observed = list(show = TRUE, alpha = 0.3, size = 1.5)
  )
)

# Slider start positions (0-100) that reproduce each engine's defaults through
# prominence_to_layer(). The tmb individual width 0.5 is not exactly reachable;
# 20 gives 0.26 / 0.52 and tmb has no live caller yet.
prominence_starts <- list(
  beezdemand_nlme = list(population = 75, individual = 25),
  beezdemand_tmb = list(population = 100, individual = 20)
)

LIMITS <- list(alpha = c(0.05, 1), width = c(0.1, 4), size = c(0.5, 8)) # nolint: object_name_linter

check_engine <- function(engine) {
  if (!is.character(engine) || length(engine) != 1L || !engine %in% ENGINES) {
    stop(
      "Unknown plot engine: ", deparse(engine),
      ". Known: ", paste(ENGINES, collapse = ", "),
      call. = FALSE
    )
  }
  engine
}

#' Known plot engines
#' @export
engines <- function() {
  ENGINES
}

#' Clamp ranges for the numeric style fields
#' @export
limits <- function() {
  LIMITS
}

#' The style an untouched sidebar produces for an engine
#'
#' @param engine One of `engines()`.
#' @return The canonical style list.
#' @export
style_defaults <- function(engine = "beezdemand_nlme") {
  engine <- check_engine(engine)
  list(layers = engine_defaults[[engine]])
}

#' Slider start positions (0-100) per engine
#' @export
prominence_defaults <- function(engine = "beezdemand_nlme") {
  engine <- check_engine(engine)
  prominence_starts[[engine]]
}

#' Map a prominence value in [0, 1] to a layer's alpha and width
#'
#' Population: alpha = 0.3 + 0.8p (capped at 1), width = 0.4 + 0.8p.
#' Individual: alpha = 0.1 + 0.8p, width = 0.2 + 1.6p.
#' @export
prominence_to_layer <- function(p, layer = c("population", "individual")) {
  layer <- match.arg(layer)
  if (!is.numeric(p) || length(p) != 1L || is.na(p)) {
    stop("`p` must be a single number in [0, 1]", call. = FALSE)
  }
  p <- min(max(p, 0), 1)
  if (layer == "population") {
    return(list(alpha = min(0.3 + 0.8 * p, 1), width = 0.4 + 0.8 * p))
  }
  list(alpha = 0.1 + 0.8 * p, width = 0.2 + 1.6 * p)
}

clamp_num <- function(x, default, range) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || !is.finite(x)) {
    return(default)
  }
  min(max(x, range[1]), range[2])
}

validate_layer <- function(layer, default, size_key) {
  if (!is.list(layer)) {
    layer <- list()
  }
  out <- list(
    show = if (is.null(layer[["show"]])) default[["show"]] else isTRUE(layer[["show"]]),
    alpha = clamp_num(layer[["alpha"]], default[["alpha"]], LIMITS[["alpha"]])
  )
  out[[size_key]] <- clamp_num(layer[[size_key]], default[[size_key]], LIMITS[[size_key]])
  out
}

#' Coerce any user-supplied style into the canonical shape
#'
#' Missing or invalid numerics fall back to the engine default for that field
#' and are clamped to `limits()`; a missing `show` takes the engine default; a
#' present one is `isTRUE()`d; unknown keys are dropped. The result has a fixed
#' key order and types, so it is safe as a `bindCache()` key.
#' @export
validate_style <- function(style, engine = "beezdemand_nlme") {
  defaults <- style_defaults(engine)[["layers"]]
  layers <- if (is.list(style) && is.list(style[["layers"]])) style[["layers"]] else list()
  list(
    layers = list(
      population = validate_layer(layers[["population"]], defaults[["population"]], "width"),
      individual = validate_layer(layers[["individual"]], defaults[["individual"]], "width"),
      observed = validate_layer(layers[["observed"]], defaults[["observed"]], "size")
    )
  )
}
