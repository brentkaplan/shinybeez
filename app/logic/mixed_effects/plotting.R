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

#' Build the show_pred_lines argument for plot()
#'
#' @param show_population Logical, show population-level prediction lines
#' @param show_individual Logical, show individual-level prediction lines
#' @return Character vector, logical FALSE, or specific values for show_pred_lines
#' @export
build_pred_lines_arg <- function(show_population, show_individual) {
  pred_lines <- c()
  if (isTRUE(show_population)) {
    pred_lines <- c(pred_lines, "population")
  }
  if (isTRUE(show_individual)) {
    pred_lines <- c(pred_lines, "individual")
  }

  if (length(pred_lines) == 0) {
    return(FALSE)
  } else if (length(pred_lines) == 2) {
    return(c("population", "individual"))
  } else {
    return(pred_lines)
  }
}

#' Check if plot has content to display
#'
#' @param show_population Logical, show population lines
#' @param show_individual Logical, show individual lines
#' @param show_observed Logical, show observed points
#' @return Logical, TRUE if something will be displayed
#' @export
has_plot_content <- function(show_population, show_individual, show_observed) {
  isTRUE(show_population) || isTRUE(show_individual) || isTRUE(show_observed)
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

#' Apply dark mode styling to a ggplot object
#'
#' Makes plot backgrounds transparent and adjusts text/grid colors for dark
#' mode contexts. Applies no-op when dark_mode is "light".
#'
#' @param p A ggplot object
#' @param dark_mode Character, "dark" or "light"
#' @return Modified ggplot object
#' @export
apply_dark_mode_theme <- function(p, dark_mode = "light") {
  if (!identical(dark_mode, "dark")) {
    return(p)
  }

  text_color <- "#dee2e6"
  grid_color <- "#495057"

  p + ggplot2$theme(
    plot.background = ggplot2$element_rect(fill = "transparent", color = NA),
    panel.background = ggplot2$element_rect(fill = "transparent", color = NA),
    legend.background = ggplot2$element_rect(fill = "transparent", color = NA),
    legend.key = ggplot2$element_rect(fill = "transparent", color = NA),
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

#' Apply color palette to plot
#'
#' @param p A ggplot object
#' @param color_var The color variable name (may be NULL)
#' @param fit_data The fitted data containing the color variable
#' @param palette_name Name of the palette
#' @param get_palette_fn Function to get palette colors (n_levels -> colors)
#' @return Modified ggplot object
#' @export
apply_color_palette <- function(
  p,
  color_var,
  fit_data,
  palette_name,
  get_palette_fn
) {
  if (is.null(color_var)) {
    return(p)
  }

  if (!is.null(fit_data) && color_var %in% names(fit_data)) {
    n_levels <- length(unique(stats$na.omit(fit_data[[color_var]])))
    p <- p +
      ggplot2$scale_color_manual(
        values = get_palette_fn(palette_name, n_levels)
      )
  }

  p
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
