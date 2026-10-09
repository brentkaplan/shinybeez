#' Plotting Utilities for Mixed Effects Demand
#'
#' Pure functions for plot configuration and aesthetic validation.

box::use(
  ggplot2,
  ggprism,
  stats
)

#' Validate an aesthetic selection against valid factors
#'
#' @param selection The user's selection (may be NULL or empty string)
#' @param valid_factors Character vector of valid factor names from the model
#' @return The selection if valid, NULL otherwise
#' @export
validate_aesthetic <- function(selection, valid_factors) {
  if (
    is.null(selection) ||
      !nzchar(selection) ||
      !(selection %in% valid_factors)
  ) {
    return(NULL)
  }

  selection
}

#' Compute smart defaults for plot aesthetics
#'
#' When a model has factors, sets sensible defaults for color and linetype.
#'
#' @param current_color Current color selection (may be "" for None)
#' @param current_linetype Current linetype selection (may be "" for None)
#' @param current_facet Current facet selection (may be "" for None)
#' @param factors_in_model Character vector of factors in the fitted model
#' @return List with color, linetype, and facet selections
#' @export
compute_aesthetic_defaults <- function(
  current_color,
  current_linetype,
  current_facet,
  factors_in_model
) {
  # Validate current selections - reset if not valid
  if (!current_color %in% factors_in_model) {
    current_color <- ""
  }
  if (!current_linetype %in% factors_in_model) {
    current_linetype <- ""
  }
  if (!current_facet %in% factors_in_model) {
    current_facet <- ""
  }

  # Set smart defaults if model has factors and selections are empty

  if (length(factors_in_model) > 0) {
    if (current_color == "" && current_linetype != factors_in_model[1]) {
      current_color <- factors_in_model[1]
    }
    if (
      length(factors_in_model) > 1 &&
        current_linetype == "" &&
        current_color != factors_in_model[2]
    ) {
      current_linetype <- factors_in_model[2]
    }
  }

  list(
    color = current_color,
    linetype = current_linetype,
    facet = current_facet
  )
}

#' Build facet formula string from selection
#'
#' @param facet_var The facet variable name (may be NULL or empty)
#' @param valid_factors Character vector of valid factor names
#' @return Formula object or NULL
#' @export
build_facet_formula <- function(facet_var, valid_factors) {
  if (
    is.null(facet_var) ||
      !nzchar(facet_var) ||
      !(facet_var %in% valid_factors)
  ) {
    return(NULL)
  }

  stats$as.formula(paste("~", facet_var))
}

#' Apply theme to a ggplot object
#'
#' @param p A ggplot object
#' @param theme_name Theme name: "prism", "classic", "minimal", or other
#' @param font_size Base font size
#' @return Modified ggplot object
#' @export
apply_plot_theme <- function(p, theme_name, font_size = 14) {
  switch(
    theme_name,
    "prism" = p + ggprism$theme_prism(base_size = font_size),
    "classic" = p + ggplot2$theme_classic(base_size = font_size),
    "minimal" = p + ggplot2$theme_minimal(base_size = font_size),
    p # default: keep existing theme
  )
}

#' Apply legend position to a ggplot object
#'
#' @param p A ggplot object
#' @param position Legend position: "right", "left", "top", "bottom", "none"
#' @return Modified ggplot object
#' @export
apply_legend_position <- function(p, position = "right") {
  if (is.null(position)) {
    position <- "right"
  }
  p + ggplot2$theme(legend.position = position)
}

# The colour levels present in one frame. A factor whose observed values are all NA still
# declares its levels, so fall back to those rather than reporting none.
#' @export
color_levels_in <- function(color_var, df) {
  if (is.null(df) || !color_var %in% names(df)) {
    return(character(0))
  }
  col <- df[[color_var]]
  if (is.factor(col)) {
    observed <- levels(droplevels(col[!is.na(col)]))
    if (length(observed) > 0L) {
      return(observed)
    }
    return(levels(col))
  }
  as.character(unique(col[!is.na(col)]))
}

#' Apply color palette to plot
#'
#' @param p A ggplot object
#' @param color_var The color variable name (may be NULL)
#' @param fit_data The fitted data containing the color variable
#' @param palette_name Name of the palette
#' @param get_palette_fn Function returning palette colors, called as `function(name, n, dark)`.
#' @param dark Logical, passed to `get_palette_fn` (TRUE in dark mode). The callback contract
#'   is `function(name, n, dark)`.
#' @return Modified ggplot object
#' @export

apply_color_palette <- function(
  p,
  color_var,
  fit_data,
  palette_name,
  get_palette_fn,
  dark = FALSE
) {
  if (is.null(color_var)) {
    return(p)
  }

  # Size the palette from the union of the frame the model was FIT on and the frame the plot
  # is DRAWN from. Those are not the same object: plot() builds a prediction grid (conditioned
  # via `at`), which can carry colour levels that model_fit$data does not — and $data's column
  # can even be entirely NA. Sizing from $data alone produced production error bce0bb1d,
  # "Insufficient values in manual scale. 3 needed but only 0 provided.", because ggplot2
  # aborts when a manual scale is short. Supplying MORE values than needed is harmless, so the
  # union can never come up short, while either frame alone can.
  levels_seen <- union(
    color_levels_in(color_var, fit_data),
    color_levels_in(color_var, p$data)
  )

  # Nothing to colour: add no scale at all rather than a zero-length one.
  if (length(levels_seen) == 0L) {
    return(p)
  }

  p +
    ggplot2$scale_color_manual(
      values = get_palette_fn(palette_name, length(levels_seen), dark = dark)
    )
}

#' Build validated plot aesthetics from user inputs
#'
#' Validates color, linetype, shape, and facet selections against the model's
#' factors.
#'
#' @param color_input Raw color input from UI
#' @param linetype_input Raw linetype input from UI
#' @param facet_input Raw facet input from UI
#' @param valid_factors Character vector of valid factor names from model
#' @param shape_input Raw shape input from UI (optional)
#' @return List with validated color, linetype, shape, and facet_formula
#' @export
build_validated_aesthetics <- function(
  color_input,
  linetype_input,
  facet_input,
  valid_factors,
  shape_input = NULL
) {
  list(
    color = validate_aesthetic(color_input, valid_factors),
    linetype = validate_aesthetic(linetype_input, valid_factors),
    shape = validate_aesthetic(shape_input, valid_factors),
    facet_formula = build_facet_formula(facet_input, valid_factors)
  )
}
