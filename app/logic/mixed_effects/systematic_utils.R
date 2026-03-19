#' Systematic Criteria Utilities
#'
#' Helper functions for computing systematic criteria on demand data.

box::use(
  beezdemand[check_systematic_demand],
  dplyr,
)

#' Compute systematic criteria with optional grouping
#'
#' @param df_raw Raw data frame with id, x, y columns
#' @param id_col Name of the id column
#' @param x_col Name of the x (price) column
#' @param y_col Name of the y (consumption) column
#' @param group_vars Character vector of grouping variable names (optional)
#' @param deltaq Passed as trend_threshold to check_systematic_demand
#' @param bounce Passed as bounce_threshold to check_systematic_demand
#' @param reversals Passed as max_reversals to check_systematic_demand
#' @param ncons0 Passed as consecutive_zeros to check_systematic_demand
#' @param prepare_fn Function to prepare systematic input (from mixed_effects_demand_utils)
#' @return Data frame with systematic criteria results, or NULL on error
#' @export
compute_systematic_criteria <- function(
  df_raw,
  id_col,
  x_col,
  y_col,
  group_vars = NULL,
  deltaq = 0.025,
  bounce = 0.10,
  reversals = 0,
  ncons0 = 2,
  prepare_fn = NULL
) {
  if (is.null(df_raw) || nrow(df_raw) == 0) {
    return(NULL)
  }

  # Validate required columns exist

  if (!all(c(id_col, x_col, y_col) %in% names(df_raw))) {
    return(NULL)
  }

  # Compute with grouping if group_vars provided and non-empty
  if (!is.null(group_vars) && length(group_vars) > 0) {
    # Validate group vars exist in data
    missing_groups <- setdiff(group_vars, names(df_raw))
    if (length(missing_groups) > 0) {
      return(NULL)
    }

    df_sys <- df_raw[, c(id_col, x_col, y_col, group_vars), drop = FALSE]
    names(df_sys)[1:3] <- c("id", "x", "y")

    x_numeric <- suppressWarnings(as.numeric(df_sys$x))
    y_numeric <- suppressWarnings(as.numeric(df_sys$y))
    x_coerced_na <- !is.na(df_sys$x) & is.na(x_numeric)
    y_coerced_na <- !is.na(df_sys$y) & is.na(y_numeric)
    if (any(x_coerced_na) || any(y_coerced_na)) {
      bad_rows <- which(x_coerced_na | y_coerced_na)
      warning(sprintf(
        "Non-numeric values found in %d row(s) (rows: %s) and coerced to NA.",
        length(bad_rows),
        paste(utils::head(bad_rows, 10), collapse = ", ")
      ))
    }
    df_sys$x <- x_numeric
    df_sys$y <- y_numeric
    df_sys <- df_sys[!is.na(df_sys$y), , drop = FALSE]

    systematic <- df_sys |>
      dplyr$group_by(dplyr$across(dplyr$all_of(group_vars))) |>
      dplyr$group_modify(
        ~ check_systematic_demand(
          data = .x[, c("id", "x", "y")],
          trend_threshold = deltaq,
          bounce_threshold = bounce,
          max_reversals = reversals,
          consecutive_zeros = ncons0
        )$results
      )
  } else {
    # No grouping - use prepare function if provided
    if (!is.null(prepare_fn)) {
      df_sys <- prepare_fn(
        df = df_raw,
        id_col = id_col,
        x_col = x_col,
        y_col = y_col
      )
    } else {
      df_sys <- df_raw[, c(id_col, x_col, y_col), drop = FALSE]
      names(df_sys) <- c("id", "x", "y")
      x_numeric <- suppressWarnings(as.numeric(df_sys$x))
      y_numeric <- suppressWarnings(as.numeric(df_sys$y))
      x_coerced_na <- !is.na(df_sys$x) & is.na(x_numeric)
      y_coerced_na <- !is.na(df_sys$y) & is.na(y_numeric)
      if (any(x_coerced_na) || any(y_coerced_na)) {
        bad_rows <- which(x_coerced_na | y_coerced_na)
        warning(sprintf(
          "Non-numeric values found in %d row(s) (rows: %s) and coerced to NA.",
          length(bad_rows),
          paste(utils::head(bad_rows, 10), collapse = ", ")
        ))
      }
      df_sys$x <- x_numeric
      df_sys$y <- y_numeric
      df_sys <- df_sys[!is.na(df_sys$y), , drop = FALSE]
    }

    systematic <- check_systematic_demand(
      data = df_sys,
      trend_threshold = deltaq,
      bounce_threshold = bounce,
      max_reversals = reversals,
      consecutive_zeros = ncons0
    )$results
  }

  systematic
}

#' Validate group variables exist in data
#'
#' @param group_vars Character vector of grouping variable names
#' @param df Data frame to check
#' @return Character vector of missing variables (empty if all exist)
#' @export
validate_group_vars <- function(group_vars, df) {
  if (is.null(group_vars) || length(group_vars) == 0) {
    return(character(0))
  }
  setdiff(group_vars, names(df))
}
