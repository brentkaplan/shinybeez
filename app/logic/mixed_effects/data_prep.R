#' Data Preparation Helpers for Mixed Effects Demand
#'
#' Pure functions for column guessing, data validation, and preparation.

#' Case-insensitive column name matching
#'
#' Searches for the first matching column name from a list of candidates.
#' Matching is case-insensitive but returns the original column name.
#'
#' @param candidates Character vector of candidate column names to search for (lowercase)
#' @param cols Character vector of actual column names
#' @return The first matching column name (original case), or NA_character_ if no match
#' @export
guess_first_match <- function(candidates, cols) {
  if (length(cols) == 0) {
    return(NA_character_)
  }
  cols_lower <- tolower(cols)
  for (cand in candidates) {
    idx <- which(cols_lower == tolower(cand))
    if (length(idx) > 0) {
      return(cols[idx[1]])
    }
  }
  return(NA_character_)
}

#' Guess the ID column from column names
#'
#' @param col_names Character vector of column names
#' @param fallback_index Index to use if no match found (default: 1)
#' @return Best guess for ID column name
#' @export
guess_id_column <- function(col_names, fallback_index = 1L) {
  candidates <- c(
    "monkey",
    "id",
    "subject",
    "participant",
    "subjid",
    "subj_id",
    "responseid"
  )
  result <- guess_first_match(candidates, col_names)
  if (is.na(result) && length(col_names) >= fallback_index) {
    result <- col_names[fallback_index]
  }
  result
}

#' Guess the X/price column from column names
#'
#' @param col_names Character vector of column names
#' @param fallback_index Index to use if no match found (default: 2)
#' @return Best guess for X column name
#' @export
guess_x_column <- function(col_names, fallback_index = 2L) {
  candidates <- c("x", "price", "ratio")
  result <- guess_first_match(candidates, col_names)
  if (is.na(result) && length(col_names) >= fallback_index) {
    result <- col_names[fallback_index]
  }
  result
}

#' Guess the Y/consumption column from column names
#'
#' @param col_names Character vector of column names
#' @param fallback_index Index to use if no match found (default: 3)
#' @return Best guess for Y column name
#' @export
guess_y_column <- function(col_names, fallback_index = 3L) {
  candidates <- c("y", "y_ll4", "consumption", "response")
  result <- guess_first_match(candidates, col_names)
  if (is.na(result) && length(col_names) >= fallback_index) {
    result <- col_names[fallback_index]
  }
  result
}

#' Guess all variable columns from a dataframe
#'
#' @param df Data frame to analyze
#' @return Named list with id, x, y column names
#' @export
guess_variable_columns <- function(df) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) {
    return(list(id = NA_character_, x = NA_character_, y = NA_character_))
  }
  col_names <- names(df)
  list(
    id = guess_id_column(col_names),
    x = guess_x_column(col_names),
    y = guess_y_column(col_names)
  )
}

#' Identify potential factor columns
#'
#' Returns columns that could be used as grouping factors.
#' Excludes ID, X, Y, and numeric columns with many unique values.
#'
#' @param df Data frame to analyze
#' @param exclude_cols Character vector of column names to exclude
#' @param max_unique_numeric Maximum unique values for a numeric column to be considered a factor
#' @return Character vector of potential factor column names
#' @export
identify_factor_columns <- function(
  df,
  exclude_cols = character(0),
  max_unique_numeric = 10L
) {
  if (is.null(df) || !is.data.frame(df)) {
    return(character(0))
  }

  col_names <- names(df)
  potential_factors <- character(0)

  for (col in col_names) {
    if (col %in% exclude_cols) {
      next
    }

    col_data <- df[[col]]

    # Character or factor columns are potential factors
    if (is.character(col_data) || is.factor(col_data)) {
      potential_factors <- c(potential_factors, col)
      next
    }

    # Numeric columns with few unique values could be factors
    if (is.numeric(col_data)) {
      n_unique <- length(unique(stats::na.omit(col_data)))
      if (n_unique <= max_unique_numeric && n_unique > 1) {
        potential_factors <- c(potential_factors, col)
      }
    }
  }

  potential_factors
}

#' Identify numeric columns suitable for covariates
#'
#' @param df Data frame to analyze
#' @param exclude_cols Character vector of column names to exclude
#' @return Character vector of numeric column names
#' @export
identify_covariate_columns <- function(df, exclude_cols = character(0)) {
  if (is.null(df) || !is.data.frame(df)) {
    return(character(0))
  }

  col_names <- names(df)
  numeric_cols <- character(0)

  for (col in col_names) {
    if (col %in% exclude_cols) {
      next
    }

    col_data <- df[[col]]
    if (is.numeric(col_data)) {
      numeric_cols <- c(numeric_cols, col)
    }
  }

  numeric_cols
}

#' Convert a column to factor with specified levels order
#'
#' @param x Vector to convert
#' @param levels Optional character vector of level order
#' @return Factor
#' @export
as_ordered_factor <- function(x, levels = NULL) {
  if (is.null(levels)) {
    levels <- unique(as.character(x))
  }
  factor(x, levels = levels)
}
