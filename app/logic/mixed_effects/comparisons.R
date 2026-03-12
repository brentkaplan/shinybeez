#' Comparisons Utilities for Mixed Effects Demand
#'
#' Pure functions for building comparison specifications and formatting results.

box::use(
  stats
)

#' Build comparison specs string from factors
#'
#' @param main_factor The primary factor to compare
#' @param by_factor Optional factor to stratify comparisons by
#' @param model_factors Character vector of valid factors in the model
#' @return Character string for emmeans specs formula
#' @export
build_specs_string <- function(main_factor, by_factor, model_factors) {
  if (is.null(main_factor) || !nzchar(main_factor)) {
    return(NULL)
  }

  specs_str <- main_factor
  if (
    !is.null(by_factor) &&
      nzchar(by_factor) &&
      by_factor %in% model_factors
  ) {
    specs_str <- paste(main_factor, "*", by_factor)
  }

  specs_str
}

#' Get the contrast_by argument for comparisons
#'
#' @param by_factor The factor to stratify by (may be NULL or empty)
#' @param model_factors Character vector of valid factors in the model
#' @return The by_factor if valid, NULL otherwise
#' @export
get_contrast_by_arg <- function(by_factor, model_factors) {
  if (
    !is.null(by_factor) &&
      nzchar(by_factor) &&
      by_factor %in% model_factors
  ) {
    return(by_factor)
  }
  NULL
}

#' Validate main comparison factor selection
#'
#' @param main_factor The selected main factor
#' @param model_factors Character vector of valid factors in the model
#' @return Logical, TRUE if valid
#' @export
is_valid_comparison_factor <- function(main_factor, model_factors) {
  !is.null(main_factor) &&
    nzchar(main_factor) &&
    main_factor %in% model_factors
}

#' Get comparison data based on display type
#'
#' @param comparison_result The comparison result object (e.g., comps$Q0)
#' @param display_type Either "ratio" or "log10"
#' @return Data frame of comparison results
#' @export
get_comparison_data <- function(comparison_result, display_type) {
  if (is.null(comparison_result)) {
    return(NULL)
  }

  if (display_type == "ratio") {
    comparison_result$contrasts_ratio
  } else {
    comparison_result$contrasts_log10
  }
}

#' Build caption text for comparison table
#'
#' @param param_name Parameter name ("Q0" or "alpha")
#' @param display_type Either "ratio" or "log10"
#' @return Character string caption
#' @export
build_comparison_caption <- function(param_name, display_type) {
  if (display_type == "ratio") {
    paste0("Pairwise Comparisons for ", param_name, " (Natural Scale Ratios)")
  } else {
    paste0("Pairwise Comparisons for ", param_name, " (log10 difference)")
  }
}

#' Check if comparison data is empty
#'
#' @param data The comparison data frame
#' @return Logical, TRUE if data is empty/NULL/invalid
#' @export
is_empty_comparison <- function(data) {
  is.null(data) ||
    !is.data.frame(data) ||
    nrow(data) == 0
}

#' Build empty comparison message
#'
#' @param param_name Parameter name ("Q0" or "alpha")
#' @return Character string message explaining why comparisons are unavailable
#' @export
build_empty_comparison_message <- function(param_name) {
  paste0(
    "No ",
    param_name,
    " comparisons available (",
    param_name,
    " may be intercept-only due to collapsed levels)."
  )
}

#' Get other factors for contrast_by selection
#'
#' @param model_factors Character vector of all model factors
#' @param main_factor The currently selected main comparison factor
#' @return Character vector of other factors (excluding main_factor)
#' @export
get_other_factors <- function(model_factors, main_factor) {
  if (is.null(model_factors) || length(model_factors) <= 1) {
    return(character(0))
  }
  setdiff(model_factors, main_factor)
}

#' Build the specs formula object for comparisons
#'
#' @param specs_string The specs string (e.g., "factor1" or "factor1 * factor2")
#' @return Formula object
#' @export
build_specs_formula <- function(specs_string) {
  if (is.null(specs_string) || !nzchar(specs_string)) {
    return(NULL)
  }
  stats$as.formula(paste("~", specs_string))
}

#' Check if model has factors for comparison
#'
#' @param model_factors Character vector of factors from model
#' @return Logical, TRUE if comparisons are possible
#' @export
can_compare <- function(model_factors) {
  !is.null(model_factors) && length(model_factors) > 0
}

#' Round numeric columns in comparison data
#'
#' @param data Data frame with comparison results
#' @param digits Number of decimal places
#' @return Data frame with rounded numeric columns
#' @export
round_comparison_data <- function(data, digits = 4) {
  if (is.null(data) || !is.data.frame(data)) {
    return(data)
  }

  # Round all numeric columns
  numeric_cols <- sapply(data, is.numeric)
  data[numeric_cols] <- lapply(data[numeric_cols], round, digits = digits)
  data
}

#' Prepare comparison display data
#'
#' Extracts, validates, and formats comparison data for display.
#'
#' @param comp_result Comparison result for a single parameter (e.g., comps$Q0)
#' @param display_type Display type ("ratio" or "log10")
#' @param param_name Parameter name for caption ("Q0" or "alpha")
#' @return List with display_data and caption_text, or NULL if no data
#' @export
prepare_comparison_display <- function(comp_result, display_type, param_name) {
  if (is.null(comp_result)) {
    return(NULL)
  }

  raw_data <- get_comparison_data(comp_result, display_type)

  if (is_empty_comparison(raw_data)) {
    return(NULL)
  }

  list(
    display_data = round_comparison_data(raw_data),
    caption_text = build_comparison_caption(param_name, display_type)
  )
}

#' Check comparison UI state
#'
#' Determines what to show in the comparison UI based on data availability.
#'
#' @param comp_result Comparison result for a single parameter
#' @param display_type Display type ("ratio" or "log10")
#' @return Character: "show_table", "show_empty_message", or "hide"
#' @export
get_comparison_ui_state <- function(comp_result, display_type) {
  if (is.null(comp_result)) {
    return("hide")
  }

  display_data <- get_comparison_data(comp_result, display_type)

  if (is_empty_comparison(display_data)) {
    return("show_empty_message")
  }

  "show_table"
}
