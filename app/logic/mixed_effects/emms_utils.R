#' EMM (Estimated Marginal Means) Utilities
#'
#' Helper functions for processing and formatting EMM data from mixed effects models.

box::use(
  dplyr,
  stats
)

#' Extract Q0-related columns from EMM data
#'
#' @param emms_data Data frame from get_observed_demand_param_emms
#' @param factor_cols Character vector of factor column names
#' @return Data frame with Q0 columns only, or NULL if none found
#' @export
extract_q0_columns <- function(emms_data, factor_cols = NULL) {
  if (is.null(emms_data) || nrow(emms_data) == 0) {
    return(NULL)
  }

  # Identify Q0-related columns (case-insensitive)
  q0_cols <- names(emms_data)[
    grepl("q0|Q0", names(emms_data), ignore.case = TRUE) &
      !grepl("_q0$|_Q0$", names(emms_data), ignore.case = TRUE)
  ]

  if (length(q0_cols) == 0) {
    return(NULL)
  }

  # Include factor columns if provided
  if (is.null(factor_cols)) {
    factor_cols <- names(emms_data)[!sapply(emms_data, is.numeric)]
  }

  cols_to_use <- intersect(c(factor_cols, q0_cols), names(emms_data))
  result <- emms_data[, cols_to_use, drop = FALSE]
  dplyr$distinct(result)
}

#' Extract Alpha-related columns from EMM data
#'
#' @param emms_data Data frame from get_observed_demand_param_emms
#' @param factor_cols Character vector of factor column names
#' @return Data frame with Alpha columns only, or NULL if none found
#' @export
extract_alpha_columns <- function(emms_data, factor_cols = NULL) {
  if (is.null(emms_data) || nrow(emms_data) == 0) {
    return(NULL)
  }

  # Identify Alpha-related columns
  alpha_cols <- names(emms_data)[
    grepl("alpha", names(emms_data), ignore.case = TRUE) &
      !grepl("_alpha$", names(emms_data), ignore.case = TRUE)
  ]

  if (length(alpha_cols) == 0) {
    return(NULL)
  }

  # Include factor columns if provided
  if (is.null(factor_cols)) {
    factor_cols <- names(emms_data)[!sapply(emms_data, is.numeric)]
  }

  cols_to_use <- intersect(c(factor_cols, alpha_cols), names(emms_data))
  result <- emms_data[, cols_to_use, drop = FALSE]
  dplyr$distinct(result)
}

#' Extract EV-related columns from EMM data
#'
#' @param emms_data Data frame from get_observed_demand_param_emms
#' @param factor_cols Character vector of factor column names
#' @return Data frame with EV columns only, or NULL if none found
#' @export
extract_ev_columns <- function(emms_data, factor_cols = NULL) {
  if (is.null(emms_data) || nrow(emms_data) == 0) {
    return(NULL)
  }

  # Identify EV-related columns
  ev_cols <- names(emms_data)[
    grepl("\\bEV\\b", names(emms_data), ignore.case = FALSE)
  ]

  if (length(ev_cols) == 0) {
    return(NULL)
  }

  # Include factor columns if provided
  if (is.null(factor_cols)) {
    factor_cols <- names(emms_data)[!sapply(emms_data, is.numeric)]
  }

  cols_to_use <- intersect(c(factor_cols, ev_cols), names(emms_data))
  result <- emms_data[, cols_to_use, drop = FALSE]
  dplyr$distinct(result)
}

#' Round numeric columns in a data frame
#'
#' @param df Data frame
#' @param digits Number of decimal places
#' @return Data frame with rounded numeric columns
#' @export
round_numeric_columns <- function(df, digits = 4) {
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }

  numeric_cols <- sapply(df, is.numeric)
  df[, numeric_cols] <- lapply(
    df[, numeric_cols, drop = FALSE],
    function(x) round(x, digits)
  )
  df
}

#' Format comparison results for display
#'
#' @param comparisons List with comparison results (from get_demand_comparisons)
#' @param param Parameter name ("Q0" or "alpha")
#' @param format Format type ("ratio" or "log10")
#' @param digits Number of decimal places for rounding
#' @return Formatted data frame, or NULL if not available
#' @export
format_comparison_results <- function(
  comparisons,
  param,
  format = "ratio",
  digits = 4
) {
  if (is.null(comparisons) || is.null(comparisons[[param]])) {
    return(NULL)
  }

  key <- paste0("contrasts_", format)
  result <- comparisons[[param]][[key]]

  if (is.null(result) || nrow(result) == 0) {
    return(NULL)
  }

  round_numeric_columns(result, digits)
}

#' Check if EMM data has any content
#'
#' @param emms_data Data frame from get_observed_demand_param_emms
#' @return Logical indicating if data has content
#' @export
has_emm_content <- function(emms_data) {
  !is.null(emms_data) && is.data.frame(emms_data) && nrow(emms_data) > 0
}

#' Get factor columns from EMM data
#'
#' @param emms_data Data frame from get_observed_demand_param_emms
#' @return Character vector of factor column names
#' @export
get_factor_columns <- function(emms_data) {
  if (is.null(emms_data) || ncol(emms_data) == 0) {
    return(character(0))
  }
  names(emms_data)[!sapply(emms_data, is.numeric)]
}

#' Get factor columns for Q0 display (excludes _alpha suffix columns)
#'
#' @param emms_data Data frame from get_observed_demand_param_emms
#' @return Character vector of Q0-appropriate factor column names
#' @export
get_q0_factor_columns <- function(emms_data) {
  factor_cols <- get_factor_columns(emms_data)
  # For Q0 table, use original factor name (not _alpha suffix)
  factor_cols[!grepl("_alpha$", factor_cols)]
}

#' Get factor columns for Alpha/EV display (handles asymmetric collapse)
#'
#' When asymmetric collapse is used, some factors have _alpha suffix versions.
#' This function returns the appropriate columns: _alpha suffixed versions where
#' they exist, plus any uncollapsed factors.
#'
#' @param emms_data Data frame from get_observed_demand_param_emms
#' @return Character vector of alpha-appropriate factor column names
#' @export
get_alpha_factor_columns <- function(emms_data) {
  factor_cols <- get_factor_columns(emms_data)

  # Columns ending with _alpha (collapsed factors like dose_alpha)

  alpha_suffix_cols <- factor_cols[grepl("_alpha$", factor_cols)]

  if (length(alpha_suffix_cols) == 0) {
    # No differential collapse - use all original factor columns
    return(factor_cols)
  }

  # Get original factor names that were collapsed (e.g., "dose" from "dose_alpha")
  collapsed_original_names <- sub("_alpha$", "", alpha_suffix_cols)

  # Get uncollapsed factor columns (not ending in _alpha AND not the original of a collapsed factor)
  uncollapsed_factor_cols <- factor_cols[
    !factor_cols %in% collapsed_original_names &
      !grepl("_alpha$", factor_cols)
  ]

  # Combine: collapsed + uncollapsed
  c(alpha_suffix_cols, uncollapsed_factor_cols)
}

#' Check if EMM data has EV columns
#'
#' @param emms_data Data frame from get_observed_demand_param_emms
#' @return Logical indicating if EV columns exist
#' @export
has_ev_columns <- function(emms_data) {
  if (is.null(emms_data) || ncol(emms_data) == 0) {
    return(FALSE)
  }
  any(grepl("^EV$|^EV_|_EV$", names(emms_data)))
}

#' Build empty EMM message data frame
#'
#' @param param_name Parameter name ("Q0", "Alpha", "EV")
#' @return Data frame with a single Message column
#' @export
build_empty_emm_message <- function(param_name) {
  data.frame(Message = paste0("No ", param_name, " EMMs available"))
}

#' Extract and prepare Q0 data for display
#'
#' @param emms_data Data frame from get_observed_demand_param_emms
#' @param digits Number of decimal places for rounding
#' @return Data frame ready for display, or NULL
#' @export
prepare_q0_display_data <- function(emms_data, digits = 4) {
  if (!has_emm_content(emms_data)) {
    return(NULL)
  }

  q0_factor_cols <- get_q0_factor_columns(emms_data)
  q0_cols <- names(emms_data)[grepl("Q0", names(emms_data))]
  table_cols <- intersect(c(q0_factor_cols, q0_cols), names(emms_data))

  if (length(table_cols) == 0) {
    return(NULL)
  }

  result <- emms_data[, table_cols, drop = FALSE]
  result <- dplyr$distinct(result)
  round_numeric_columns(result, digits)
}

#' Extract and prepare Alpha data for display
#'
#' @param emms_data Data frame from get_observed_demand_param_emms
#' @param digits Number of decimal places for standard columns
#' @param digits_natural Number of decimal places for alpha_natural columns
#' @return Data frame ready for display, or NULL
#' @export
prepare_alpha_display_data <- function(
  emms_data,
  digits = 4,
  digits_natural = 8
) {
  if (!has_emm_content(emms_data)) {
    return(NULL)
  }

  alpha_factor_cols <- get_alpha_factor_columns(emms_data)
  alpha_cols <- names(emms_data)[
    grepl("alpha", names(emms_data)) & !grepl("_alpha$", names(emms_data))
  ]
  table_cols <- intersect(c(alpha_factor_cols, alpha_cols), names(emms_data))

  if (length(table_cols) == 0) {
    return(NULL)
  }

  result <- emms_data[, table_cols, drop = FALSE]
  result <- dplyr$distinct(result)

  # Round standard numeric columns
  numeric_cols <- names(result)[sapply(result, is.numeric)]
  natural_cols <- numeric_cols[grepl("alpha_natural", numeric_cols)]
  standard_cols <- setdiff(numeric_cols, natural_cols)

  if (length(standard_cols) > 0) {
    result[standard_cols] <- lapply(
      result[standard_cols],
      round,
      digits = digits
    )
  }
  if (length(natural_cols) > 0) {
    result[natural_cols] <- lapply(
      result[natural_cols],
      round,
      digits = digits_natural
    )
  }

  result
}

#' Extract and prepare EV data for display
#'
#' @param emms_data Data frame from get_observed_demand_param_emms
#' @param digits Number of decimal places for rounding
#' @return Data frame ready for display, or NULL
#' @export
prepare_ev_display_data <- function(emms_data, digits = 4) {
  if (!has_emm_content(emms_data) || !has_ev_columns(emms_data)) {
    return(NULL)
  }

  ev_factor_cols <- get_alpha_factor_columns(emms_data) # EV uses same factors as alpha
  ev_cols <- names(emms_data)[grepl("EV", names(emms_data))]
  table_cols <- intersect(c(ev_factor_cols, ev_cols), names(emms_data))

  if (length(table_cols) == 0) {
    return(NULL)
  }

  result <- emms_data[, table_cols, drop = FALSE]
  result <- dplyr$distinct(result)
  round_numeric_columns(result, digits)
}
