#' Model Fitting Utilities for Mixed Effects Demand
#'
#' Pure functions for building model configurations and processing covariates.

box::use(
  stats
)

#' Transform a covariate column with centering and/or scaling
#'
#' @param x Numeric vector to transform
#' @param center Logical, whether to center (subtract mean)
#' @param scale Logical, whether to scale (divide by SD)
#' @return List with `values` (transformed vector), `mu` (mean), `sigma` (SD),
#'         and `transform_applied` (character describing transformation)
#' @export
transform_covariate <- function(x, center = FALSE, scale = FALSE) {
  x <- as.numeric(x)
  mu <- mean(x, na.rm = TRUE)
  sigma <- stats$sd(x, na.rm = TRUE)

  # Check if scaling is possible
  can_scale <- scale && !is.na(sigma) && is.finite(sigma) && sigma > 0

  if (can_scale) {
    values <- (x - mu) / sigma
    transform_applied <- "centered_scaled"
  } else if (center) {
    values <- x - mu
    transform_applied <- "centered"
  } else {
    values <- x
    transform_applied <- "none"
  }

  list(
    values = values,
    mu = mu,
    sigma = sigma,
    transform_applied = transform_applied,
    scale_failed = scale && !can_scale
  )
}

#' Transform a natural-scale value using the same transformation as the covariate
#'
#' @param at_value Numeric value to transform (on natural scale)
#' @param mu Mean used for centering
#' @param sigma SD used for scaling
#' @param center Logical, whether centering was applied
#' @param scale Logical, whether scaling was applied
#' @return Transformed value
#' @export
transform_at_value <- function(
  at_value,
  mu,
  sigma,
  center = FALSE,
  scale = FALSE
) {
  if (!is.numeric(at_value) || !is.finite(at_value)) {
    return(NA_real_)
  }

  can_scale <- scale && !is.na(sigma) && is.finite(sigma) && sigma > 0

  if (can_scale) {
    return((at_value - mu) / sigma)
  } else if (center) {
    return(at_value - mu)
  }
  at_value
}

#' Build covariate column name based on transformation
#'
#' @param base_name Original column name
#' @param center Logical, whether centering is applied
#' @param scale Logical, whether scaling is applied
#' @return New column name with appropriate suffix
#' @export
build_covariate_column_name <- function(
  base_name,
  center = FALSE,
  scale = FALSE
) {
  if (scale) {
    return(paste0(base_name, "_cs"))
  } else if (center) {
    return(paste0(base_name, "_c"))
  }
  base_name
}

#' Process a dataframe to add transformed covariate column
#'
#' @param df Data frame
#' @param covariate_col Name of the covariate column
#' @param center Logical, whether to center
#' @param scale Logical, whether to scale
#' @param at_value Optional natural-scale value for conditioning
#' @return List with `df` (modified dataframe), `model_covariate_name`,
#'         `at_list` (for beezdemand APIs), and `transform_info`
#' @export
process_covariate <- function(
  df,
  covariate_col,
  center = FALSE,
  scale = FALSE,
  at_value = NULL
) {
  # Return early if no covariate specified
  if (
    is.null(covariate_col) ||
      !nzchar(covariate_col) ||
      !(covariate_col %in% names(df))
  ) {
    return(list(
      df = df,
      model_covariate_name = NULL,
      at_list = NULL,
      transform_info = NULL
    ))
  }

  # Get base vector, converting character to numeric if needed
  base_vec <- df[[covariate_col]]
  base_name <- covariate_col

  if (is.character(base_vec)) {
    base_name <- paste0(covariate_col, "_num")
    df[[base_name]] <- suppressWarnings(as.numeric(base_vec))
    base_vec <- df[[base_name]]
  }

  # Apply transformation
  transform_result <- transform_covariate(base_vec, center, scale)

  # Determine effective column name
  effective_name <- build_covariate_column_name(
    base_name,
    center,
    scale && !transform_result$scale_failed
  )

  # Add transformed column to dataframe
  if (center || (scale && !transform_result$scale_failed)) {
    df[[effective_name]] <- transform_result$values
  }

  # Build 'at' list for beezdemand APIs
  at_list <- NULL
  if (!is.null(at_value)) {
    at_val_num <- suppressWarnings(as.numeric(at_value))
    if (!is.na(at_val_num) && is.finite(at_val_num)) {
      transformed_at <- transform_at_value(
        at_val_num,
        transform_result$mu,
        transform_result$sigma,
        center,
        scale && !transform_result$scale_failed
      )
      at_list <- list(transformed_at)
      names(at_list) <- effective_name
    }
  }

  list(
    df = df,
    model_covariate_name = effective_name,
    at_list = at_list,
    transform_info = transform_result
  )
}

#' Build nlme control settings from parameters
#'
#' @param preset Character preset name ("default", "relaxed", "aggressive", "custom")
#' @param max_iter Maximum iterations (for custom)
#' @param pnls_max_iter PNLS maximum iterations (for custom)
#' @param ms_max_iter MS maximum iterations (for custom)
#' @param tolerance Tolerance (for custom)
#' @param pnls_tol PNLS tolerance (for custom)
#' @param min_scale Minimum scale (for custom)
#' @param n_iter_em Number of EM iterations (for custom)
#' @return List suitable for nlme::nlmeControl()
#' @export
build_nlme_control <- function(
  preset = "default",
  max_iter = 50L,
  pnls_max_iter = 7L,
  ms_max_iter = 50L,
  tolerance = 1e-6,
  pnls_tol = 0.001,
  min_scale = 0.001,
  n_iter_em = 25L
) {
  # Preset configurations matching the shinybeez UI
  presets <- list(
    default = list(
      maxIter = 50L,
      pnlsMaxIter = 7L,
      msMaxIter = 50L,
      tolerance = 1e-6,
      pnlsTol = 0.001,
      minScale = 0.001,
      niterEM = 25L
    ),
    # Shinybeez UI presets
    Balanced = list(
      maxIter = 100L,
      pnlsMaxIter = 7L,
      msMaxIter = 100L,
      tolerance = 1e-3,
      pnlsTol = 1e-3,
      minScale = 1e-3,
      niterEM = 50L
    ),
    Faster = list(
      maxIter = 60L,
      pnlsMaxIter = 10L,
      msMaxIter = 80L,
      tolerance = 3e-3,
      pnlsTol = 3e-3,
      minScale = 5e-3,
      niterEM = 30L
    ),
    Stricter = list(
      maxIter = 200L,
      pnlsMaxIter = 30L,
      msMaxIter = 200L,
      tolerance = 1e-5,
      pnlsTol = 1e-4,
      minScale = 1e-4,
      niterEM = 80L
    ),
    # Legacy aliases
    relaxed = list(
      maxIter = 200L,
      pnlsMaxIter = 20L,
      msMaxIter = 200L,
      tolerance = 1e-4,
      pnlsTol = 0.01,
      minScale = 1e-5,
      niterEM = 50L
    ),
    aggressive = list(
      maxIter = 500L,
      pnlsMaxIter = 50L,
      msMaxIter = 500L,
      tolerance = 1e-8,
      pnlsTol = 1e-4,
      minScale = 1e-6,
      niterEM = 100L
    )
  )

  if (preset %in% names(presets)) {
    return(presets[[preset]])
  }

  # Custom settings
  list(
    maxIter = as.integer(max_iter),
    pnlsMaxIter = as.integer(pnls_max_iter),
    msMaxIter = as.integer(ms_max_iter),
    tolerance = as.numeric(tolerance),
    pnlsTol = as.numeric(pnls_tol),
    minScale = as.numeric(min_scale),
    niterEM = as.integer(n_iter_em)
  )
}

#' Validate factor columns exist and have multiple levels
#'
#' @param df Data frame
#' @param factor_cols Character vector of factor column names
#' @return List with `valid` (logical), `errors` (character vector of error messages)
#' @export
validate_factors <- function(df, factor_cols) {
  if (is.null(factor_cols) || length(factor_cols) == 0) {
    return(list(valid = TRUE, errors = character(0)))
  }

  errors <- character(0)

  for (col in factor_cols) {
    if (col == "None" || !nzchar(col)) {
      next
    }

    if (!(col %in% names(df))) {
      errors <- c(errors, paste0("Factor column '", col, "' not found in data"))
      next
    }

    n_levels <- length(unique(stats::na.omit(df[[col]])))
    if (n_levels < 2) {
      errors <- c(errors, paste0("Factor '", col, "' has fewer than 2 levels"))
    }
  }

  list(valid = length(errors) == 0, errors = errors)
}
