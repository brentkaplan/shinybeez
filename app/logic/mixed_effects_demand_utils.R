#' @export
# Utility and logic functions for the Mixed Effects Demand module will be moved here.

box::use(
  dplyr,
  rlang
)

#' Prepare input data for beezdemand::CheckUnsystematic
#'
#' This helper selects and renames the provided id/x/y columns to the
#' standardized names expected by CheckUnsystematic, and coerces x and y
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
    stop(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))
  }

  out <- df[, c(id_col, x_col, y_col)]
  names(out) <- c("id", "x", "y")

  # Coerce types
  out$id <- as.character(out$id)
  suppressWarnings({
    out$x <- as.numeric(out$x)
    out$y <- as.numeric(out$y)
  })

  # Drop rows with NA in y
  out <- out[!is.na(out$y), , drop = FALSE]

  out
}
