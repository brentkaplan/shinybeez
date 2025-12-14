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
