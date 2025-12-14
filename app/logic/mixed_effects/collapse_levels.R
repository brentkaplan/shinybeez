#' Collapse Levels Logic for Mixed Effects Demand
#'
#' Pure functions for building and validating collapse level specifications.
#' These are used to group factor levels for Q0 and Alpha parameters.

box::use(
  rlang[`%||%`]
)

#' Build a collapse mapping list from group names and levels
#'
#' @param g1_name Character name for group 1
#' @param g1_levels Character vector of levels for group 1
#' @param g2_name Character name for group 2
#' @param g2_levels Character vector of levels for group 2
#' @return Named list mapping group names to their levels, or NULL if empty
#' @export
build_collapse_list <- function(g1_name, g1_levels, g2_name, g2_levels) {
  collapse_list <- list()

  if (!is.null(g1_name) && nzchar(g1_name) && length(g1_levels) > 0) {
    collapse_list[[g1_name]] <- g1_levels
  }
  if (!is.null(g2_name) && nzchar(g2_name) && length(g2_levels) > 0) {
    collapse_list[[g2_name]] <- g2_levels
  }

  if (length(collapse_list) == 0) {
    return(NULL)
  }
  collapse_list
}

#' Check for overlapping levels between two groups
#'
#' @param g1_levels Character vector of levels for group 1
#' @param g2_levels Character vector of levels for group 2
#' @return Character vector of overlapping levels, or NULL if no overlap
#' @export
find_overlap <- function(g1_levels, g2_levels) {
  if (length(g1_levels) == 0 || length(g2_levels) == 0) {
    return(NULL)
  }
  overlap <- intersect(g1_levels, g2_levels)
  if (length(overlap) > 0) {
    return(overlap)
  }
  NULL
}

#' Validate no overlap between group levels
#'
#' @param g1_levels Character vector of levels for group 1
#' @param g2_levels Character vector of levels for group 2
#' @param factor_name Name of the factor being checked (for error message)
#' @param param_label Parameter label ("Q0" or "Alpha") (for error message)
#' @return List with `valid` (logical) and `overlap` (character vector or NULL),
#'         or `error_message` if invalid
#' @export
validate_no_overlap <- function(
  g1_levels,
  g2_levels,
  factor_name = NULL,
  param_label = NULL
) {
  overlap <- find_overlap(g1_levels, g2_levels)

  if (!is.null(overlap)) {
    msg <- if (!is.null(factor_name) && !is.null(param_label)) {
      paste0(
        "Overlap in ",
        param_label,
        " collapse for '",
        factor_name,
        "': ",
        paste(overlap, collapse = ", ")
      )
    } else {
      paste0("Overlapping levels: ", paste(overlap, collapse = ", "))
    }

    return(list(
      valid = FALSE,
      overlap = overlap,
      error_message = msg
    ))
  }

  list(valid = TRUE, overlap = NULL, error_message = NULL)
}

#' Process collapse configuration for one parameter of one factor
#'
#' @param collapse_enabled Logical, whether collapse is enabled
#' @param g1_name Character name for group 1
#' @param g1_levels Character vector of levels for group 1
#' @param g2_name Character name for group 2
#' @param g2_levels Character vector of levels for group 2
#' @param factor_name Name of the factor (for error context)
#' @param param_label Parameter label "Q0" or "Alpha" (for error context)
#' @return List with `collapse` (the collapse mapping or NULL) and
#'         `error` (error info or NULL)
#' @export
process_param_collapse <- function(
  collapse_enabled,
  g1_name,
  g1_levels,
  g2_name,
  g2_levels,
  factor_name = NULL,
  param_label = NULL
) {
  # Guard: Only process if explicitly enabled

  if (!isTRUE(collapse_enabled)) {
    return(list(collapse = NULL, error = NULL))
  }

  # Validate no overlap
  validation <- validate_no_overlap(
    g1_levels,
    g2_levels,
    factor_name,
    param_label
  )

  if (!validation$valid) {
    return(list(collapse = NULL, error = validation))
  }

  # Build and return the collapse mapping
  collapse <- build_collapse_list(g1_name, g1_levels, g2_name, g2_levels)
  list(collapse = collapse, error = NULL)
}

#' Build the complete collapse levels structure for beezdemand
#'
#' @param factor1_config List with factor1 collapse configuration
#' @param factor2_config List with factor2 collapse configuration (optional)
#' @return List with Q0 and alpha collapse specifications, or error string
#' @export
build_collapse_structure <- function(
  factor1_config = NULL,
  factor2_config = NULL
) {
  q0_collapse <- list()
  alpha_collapse <- list()

  # Process Factor 1
  if (!is.null(factor1_config)) {
    f1_name <- factor1_config$name

    if (!is.null(factor1_config$q0$collapse)) {
      q0_collapse[[f1_name]] <- factor1_config$q0$collapse
    }
    if (!is.null(factor1_config$alpha$collapse)) {
      alpha_collapse[[f1_name]] <- factor1_config$alpha$collapse
    }
  }

  # Process Factor 2
  if (!is.null(factor2_config)) {
    f2_name <- factor2_config$name

    if (!is.null(factor2_config$q0$collapse)) {
      q0_collapse[[f2_name]] <- factor2_config$q0$collapse
    }
    if (!is.null(factor2_config$alpha$collapse)) {
      alpha_collapse[[f2_name]] <- factor2_config$alpha$collapse
    }
  }

  # Build final structure
  result <- list()
  if (length(q0_collapse) > 0) {
    result$Q0 <- q0_collapse
  }
  if (length(alpha_collapse) > 0) {
    result$alpha <- alpha_collapse
  }

  if (length(result) == 0) {
    return(NULL)
  }
  result
}
