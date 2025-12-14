#' Validation Utilities for Mixed Effects Demand
#'
#' Helper functions for validating data structures in reactive contexts.

box::use()

#' Check if data is a valid non-empty data frame
#'
#' @param data The data to validate
#' @return TRUE if data is a valid non-empty data frame, FALSE otherwise
#' @export
is_valid_dataframe <- function(data) {
  !is.null(data) && is.data.frame(data) && nrow(data) > 0
}

#' Check if comparison data is valid for display
#'
#' Validates that comparison data from beezdemand is a non-empty data frame.
#' This handles edge cases where contrast calculations fail and return
#' non-data.frame objects or empty results.
#'
#' @param data The comparison data to validate (e.g., contrasts_log10, contrasts_ratio)
#' @return TRUE if data is a valid non-empty data frame, FALSE otherwise
#' @export
is_valid_comparison_data <- function(data) {
  is_valid_dataframe(data)
}

#' Check if EMMs data is valid for display
#'
#' @param data The EMMs data to validate
#' @return TRUE if data is a valid non-empty data frame, FALSE otherwise
#' @export
is_valid_emms_data <- function(data) {
  is_valid_dataframe(data)
}

#' Check if model fit object is valid
#'
#' @param model_fit The model fit object from beezdemand
#' @return TRUE if model fit has a valid model, FALSE otherwise
#' @export
is_valid_model_fit <- function(model_fit) {
  !is.null(model_fit) && !is.null(model_fit$model)
}

#' Check if reactive data is ready for analysis
#'
#' Validates that data has the required columns for analysis.
#'
#' @param data The data to validate
#' @param required_cols Character vector of required column names
#' @return TRUE if data is valid and has all required columns, FALSE otherwise
#' @export
is_ready_for_analysis <- function(data, required_cols = character(0)) {
  if (!is_valid_dataframe(data)) {
    return(FALSE)
  }
  if (length(required_cols) > 0) {
    return(all(required_cols %in% names(data)))
  }
  TRUE
}

#' Safely get nrow, returning 0 for invalid data
#'
#' @param data The data to check
#' @return Number of rows, or 0 if data is invalid
#' @export
safe_nrow <- function(data) {
  if (!is.null(data) && is.data.frame(data)) {
    nrow(data)
  } else {
    0L
  }
}
