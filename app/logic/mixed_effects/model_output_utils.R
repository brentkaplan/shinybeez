#' Model Output Utilities
#'
#' Helper functions for extracting and formatting model output data.

box::use(
  dplyr,
  rlang,
  stats
)

#' Extract fixed effects as a data frame
#'
#' @param model_fit A beezdemand_nlme object
#' @param digits Number of decimal places for rounding
#' @return Data frame with Parameter and Value columns, or NULL if extraction fails
#' @export
get_fixed_effects_df <- function(model_fit, digits = 4) {
  if (is.null(model_fit) || is.null(model_fit$model)) {
    return(NULL)
  }

  fe <- tryCatch(
    nlme::fixef(model_fit),
    error = function(e) NULL
  )

  if (is.null(fe)) {
    return(NULL)
  }

  data.frame(
    Parameter = names(fe),
    Value = round(fe, digits)
  )
}

#' Extract random effects as a data frame
#'
#' @param model_fit A beezdemand_nlme object
#' @param digits Number of decimal places for rounding
#' @return Data frame with id column first and numeric columns rounded, or NULL
#' @export
get_random_effects_df <- function(model_fit, digits = 4) {
  if (is.null(model_fit) || is.null(model_fit$model)) {
    return(NULL)
  }

  re_coefs <- tryCatch(
    stats$coef(model_fit),
    error = function(e) NULL
  )

  if (is.null(re_coefs)) {
    return(NULL)
  }

  re_df <- as.data.frame(re_coefs)
  id_column_name <- model_fit$param_info$id_var

  if (is.null(id_column_name)) {
    return(NULL)
  }

  re_df[[id_column_name]] <- rownames(re_df)

  re_df |>
    dplyr$select(
      !!rlang$sym(id_column_name),
      dplyr$everything()
    ) |>
    dplyr$mutate(dplyr$across(where(is.numeric), ~ round(., digits)))
}

#' Extract individual coefficients as a data frame
#'
#' @param model_fit A beezdemand_nlme object
#' @param beezdemand_ref Reference to beezdemand module (for box::use context)
#' @param params Parameters to extract (default: c("Q0", "alpha"))
#' @param format Output format ("wide" or "long")
#' @param digits Number of decimal places for rounding
#' @return Data frame with individual coefficients, or NULL
#' @export
get_individual_coefficients_df <- function(
  model_fit,
  beezdemand_ref,
  params = c("Q0", "alpha"),
  format = "wide",
  digits = 4
) {
  if (is.null(model_fit) || is.null(model_fit$model)) {
    return(NULL)
  }

  # Check if there are any factors
  if (is.null(model_fit$param_info$factors)) {
    return(NULL)
  }

  coefs <- tryCatch(
    beezdemand_ref$get_individual_coefficients(
      model_fit,
      params = params,
      format = format
    ),
    error = function(e) NULL
  )

  if (is.null(coefs) || nrow(coefs) == 0) {
    return(NULL)
  }

  # Round numeric columns (skip first column which is typically id)
  coefs[, -1] <- round(coefs[, -1], digits)
  coefs
}

#' Check if model has valid output
#'
#' @param model_fit A beezdemand_nlme object
#' @return TRUE if model has valid fitted model, FALSE otherwise
#' @export
has_valid_model <- function(model_fit) {
  !is.null(model_fit) && !is.null(model_fit$model)
}
