#' Plot style: schema, defaults, validation and engine adapters
#'
#' Pure functions shared by every plotting tab. A *style* is a nested list
#' (see `style_defaults()`); hosts assemble one from their inputs, validate it
#' once, and hand it to `plot_args_from_style()` to splice package-specific
#' arguments into `plot()`. Nothing here touches Shiny.

box::use(
  rlang[hash],
)

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

#' TRUE when at least one layer is shown
#' @export
has_content <- function(style) {
  l <- style[["layers"]]
  isTRUE(l[["population"]][["show"]]) || isTRUE(l[["individual"]][["show"]]) || isTRUE(l[["observed"]][["show"]])
}

# The show_pred value beezdemand's plot() expects: FALSE, one name, or both.
pred_lines_arg <- function(show_population, show_individual) {
  lines <- c("population", "individual")[c(isTRUE(show_population), isTRUE(show_individual))]
  if (length(lines) == 0L) {
    return(FALSE)
  }
  lines
}

#' Arguments to splice into the package plot() call for an engine
#'
#' Use as `do.call(plot, c(list(fit), plot_args_from_style(style), other_args))`.
#' @export
plot_args_from_style <- function(style, engine = "beezdemand_nlme") {
  engine <- check_engine(engine)
  if (engine == "beezdemand_tmb") {
    stop("plot_args_from_style() is not wired for beezdemand_tmb yet (planned for v1.3)", call. = FALSE)
  }
  l <- validate_style(style, engine)[["layers"]]
  list(
    show_observed = l[["observed"]][["show"]],
    show_pred = pred_lines_arg(l[["population"]][["show"]], l[["individual"]][["show"]]),
    observed_point_alpha = l[["observed"]][["alpha"]],
    observed_point_size = l[["observed"]][["size"]],
    pop_line_alpha = l[["population"]][["alpha"]],
    pop_line_size = l[["population"]][["width"]],
    ind_line_alpha = l[["individual"]][["alpha"]],
    ind_line_size = l[["individual"]][["width"]]
  )
}

#' Flat, stable-named payload for a configuration_snapshot telemetry event
#' @export
style_telemetry_payload <- function(style, extras = list(), engine = "beezdemand_nlme") {
  l <- validate_style(style, engine)[["layers"]]
  c(
    list(
      pop_show = l[["population"]][["show"]],
      pop_alpha = round(l[["population"]][["alpha"]], 2),
      pop_width = round(l[["population"]][["width"]], 2),
      ind_show = l[["individual"]][["show"]],
      ind_alpha = round(l[["individual"]][["alpha"]], 2),
      ind_width = round(l[["individual"]][["width"]], 2),
      obs_show = l[["observed"]][["show"]],
      obs_alpha = round(l[["observed"]][["alpha"]], 2),
      obs_size = round(l[["observed"]][["size"]], 2)
    ),
    extras
  )
}

#' Build a recorder that forwards a payload only when it (or the generation) changed
#'
#' Used to log one telemetry snapshot per distinct rendered plot: the render
#' expression can run again without a new plot (the export downloads call it, and
#' a pixel-ratio change re-draws), so the same configuration would otherwise be
#' logged repeatedly.
#' @param record_fn `function(payload)` that performs the logging.
#' @return `function(payload, generation = 0L)`; returns TRUE when `record_fn` ran.
#' @export
snapshot_recorder <- function(record_fn) {
  state <- new.env(parent = emptyenv())
  function(payload, generation = 0L) {
    key <- hash(list(payload, generation))
    if (identical(key, state[["last_key"]])) {
      return(FALSE)
    }
    state[["last_key"]] <- key
    record_fn(payload)
    TRUE
  }
}
