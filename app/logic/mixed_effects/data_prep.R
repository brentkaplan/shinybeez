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

#' Test whether a column is numeric or cleanly coercible to numeric
#'
#' Factors and logicals are never numeric-like: a factor's as.numeric() yields
#' level codes, not values, and grouping columns must not be offered as X or
#' covariate candidates.
#'
#' @param v Vector to test
#' @param threshold Minimum fraction of non-NA values that must survive
#'   as.numeric() coercion for a character column to qualify. Use 1 for the
#'   X variable (every row enters the model) and 0.95 for covariates.
#' @return TRUE if the column qualifies
#' @export
is_numeric_like <- function(v, threshold = 0.95) {
  if (is.numeric(v) || is.integer(v) || inherits(v, "integer64")) {
    return(TRUE)
  }
  if (is.character(v)) {
    num <- suppressWarnings(as.numeric(v))
    non_na_orig <- sum(!is.na(v))
    if (non_na_orig == 0) {
      return(FALSE)
    }
    non_na_conv <- sum(!is.na(num))
    return((non_na_conv / non_na_orig) >= threshold)
  }
  FALSE
}

#' Columns eligible for the X (price/ratio) dropdown
#'
#' Strict 100% clean coercion: the X column feeds fit_demand_mixed directly,
#' so a column the picker offers must never be rejected at fit time.
#'
#' @param df Data frame to analyze
#' @param exclude Character vector of column names to exclude
#' @return Character vector of eligible column names
#' @export
numeric_x_candidates <- function(df, exclude = character(0)) {
  if (is.null(df) || !is.data.frame(df)) {
    return(character(0))
  }
  candidates <- setdiff(names(df), exclude)
  candidates[
    vapply(
      candidates,
      function(col) is_numeric_like(df[[col]], threshold = 1),
      logical(1)
    )
  ]
}

#' Pick the X selection given the guessed column and the eligible candidates
#'
#' Keeps the guess when it is eligible; otherwise falls back to the first
#' candidate that is not the id or y column (a numeric subject id must not be
#' silently promoted to X); otherwise "".
#'
#' @param guessed Guessed X column name (may be NA)
#' @param candidates Character vector of eligible X columns
#' @param id Selected id column name
#' @param y Selected y column name
#' @return Column name to select, or "" if none qualifies
#' @export
choose_x_selection <- function(guessed, candidates, id, y) {
  if (!is.na(guessed) && guessed %in% candidates) {
    return(guessed)
  }
  fallback <- setdiff(candidates, c(id, y))
  if (length(fallback) > 0) {
    return(fallback[1])
  }
  ""
}

#' Coerce an X column to numeric for model fitting
#'
#' Numeric columns pass through untouched — pre-existing NA or Inf are
#' beezdemand's contract to enforce, not ours. Character columns must coerce
#' with zero newly-introduced NAs. Anything else (factor, logical) is rejected.
#'
#' @param v The X column vector
#' @return list(ok, values, n_bad): ok = usable, values = numeric vector when
#'   ok, n_bad = count of values that failed coercion
#' @export
coerce_x_numeric <- function(v) {
  if (is.numeric(v) || is.integer(v)) {
    return(list(ok = TRUE, values = as.numeric(v), n_bad = 0L))
  }
  if (is.character(v)) {
    num <- suppressWarnings(as.numeric(v))
    n_bad <- sum(is.na(num) & !is.na(v))
    if (n_bad == 0L) {
      return(list(ok = TRUE, values = num, n_bad = 0L))
    }
    return(list(ok = FALSE, values = NULL, n_bad = as.integer(n_bad)))
  }
  list(ok = FALSE, values = NULL, n_bad = as.integer(sum(!is.na(v))))
}
