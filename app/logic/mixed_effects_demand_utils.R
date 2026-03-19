#' @export
# Utility and logic functions for the Mixed Effects Demand module will be moved here.

box::use(
  dplyr,
  rlang
)

#' Prepare input data for beezdemand::check_systematic_demand
#'
#' This helper selects and renames the provided id/x/y columns to the
#' standardized names expected by check_systematic_demand, and coerces x and y
#' to numeric. It also drops rows with NA in y.
#'
#' @param df data.frame containing the columns
#' @param id_col character; name of the id column
#' @param x_col character; name of the price/x column
#' @param y_col character; name of the consumption/y column
#' @return data.frame with columns id (character), x (numeric), y (numeric)
#' @export
prepare_systematic_input <- function(df, id_col, x_col, y_col) {
  if (is.null(df) || !is.data.frame(df)) {
    stop("df must be a data.frame")
  }
  missing <- setdiff(c(id_col, x_col, y_col), names(df))
  if (length(missing) > 0) {
    stop(sprintf(
      "Missing required columns: %s",
      paste(missing, collapse = ", ")
    ))
  }

  out <- df[, c(id_col, x_col, y_col)]
  names(out) <- c("id", "x", "y")

  # Coerce types
  out$id <- as.character(out$id)
  x_numeric <- suppressWarnings(as.numeric(out$x))
  y_numeric <- suppressWarnings(as.numeric(out$y))

  # Detect non-numeric values introduced as NA by coercion
  x_coerced_na <- !is.na(out$x) & is.na(x_numeric)
  y_coerced_na <- !is.na(out$y) & is.na(y_numeric)
  if (any(x_coerced_na) || any(y_coerced_na)) {
    bad_rows <- which(x_coerced_na | y_coerced_na)
    warning(sprintf(
      "Non-numeric values found in %d row(s) (rows: %s) and coerced to NA.",
      length(bad_rows),
      paste(utils::head(bad_rows, 10), collapse = ", ")
    ))
  }
  out$x <- x_numeric
  out$y <- y_numeric

  # Drop rows with NA in y
  out <- out[!is.na(out$y), , drop = FALSE]

  out
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
  !is.null(data) && is.data.frame(data) && nrow(data) > 0
}
