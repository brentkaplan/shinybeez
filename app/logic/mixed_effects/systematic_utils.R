#' Systematic Criteria Utilities
#'
#' Helper functions for computing systematic criteria on demand data.

box::use(
  dplyr
)

#' Compute systematic criteria with optional grouping
#'
#' @param df_raw Raw data frame with id, x, y columns
#' @param id_col Name of the id column
#' @param x_col Name of the x (price) column
#' @param y_col Name of the y (consumption) column
#' @param group_vars Character vector of grouping variable names (optional)
#' @param deltaq DeltaQ criterion value
#' @param bounce Bounce criterion value
#' @param reversals Reversals criterion value
#' @param ncons0 Consecutive zeros criterion value
#' @param beezdemand_ref Reference to beezdemand module
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
  beezdemand_ref,
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

    suppressWarnings({
      df_sys$x <- as.numeric(df_sys$x)
      df_sys$y <- as.numeric(df_sys$y)
    })
    df_sys <- df_sys[!is.na(df_sys$y), , drop = FALSE]

    systematic <- df_sys |>
      dplyr$group_by(dplyr$across(dplyr$all_of(group_vars))) |>
      dplyr$group_modify(
        ~ beezdemand_ref$CheckUnsystematic(
          dat = .x[, c("id", "x", "y")],
          deltaq = deltaq,
          bounce = bounce,
          reversals = reversals,
          ncons0 = ncons0
        )
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
      suppressWarnings({
        df_sys$x <- as.numeric(df_sys$x)
        df_sys$y <- as.numeric(df_sys$y)
      })
      df_sys <- df_sys[!is.na(df_sys$y), , drop = FALSE]
    }

    systematic <- beezdemand_ref$CheckUnsystematic(
      dat = df_sys,
      deltaq = deltaq,
      bounce = bounce,
      reversals = reversals,
      ncons0 = ncons0
    )
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
