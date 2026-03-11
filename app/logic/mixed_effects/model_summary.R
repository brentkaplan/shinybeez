#' Model Summary Utilities
#'
#' Functions to extract structured summary data from beezdemand_nlme objects
#' for display in the UI (instead of raw R console output).

box::use(
  nlme,
  stats
)

#' Extract model specification info
#'
#' @param model_fit A beezdemand_nlme object
#' @return Named list with equation, y_var, x_var, id_var, factors
#' @export
get_model_spec <- function(model_fit) {
  if (is.null(model_fit) || is.null(model_fit$param_info)) {
    return(NULL)
  }

  pi <- model_fit$param_info
  eq_label <- switch(
    pi$equation_form %||% "unknown",
    "zben" = "Zero-Bounded Exponential (ZBEn)",
    "simplified" = "Simplified Exponential",
    pi$equation_form
  )

  factors_label <- if (
    is.null(pi$factors) || length(pi$factors) == 0
  ) {
    "None (intercept-only)"
  } else {
    paste(pi$factors, collapse = ", ")
  }

  list(
    equation = eq_label,
    y_var = pi$y_var %||% "y_for_model",
    x_var = pi$x_var %||% "x",
    id_var = pi$id_var %||% "id",
    factors = factors_label,
    n_observations = tryCatch(nrow(model_fit$data), error = function(e) NA),
    n_groups = tryCatch(
      length(unique(model_fit$data[[pi$id_var]])),
      error = function(e) NA
    )
  )
}

#' Extract fit statistics from model
#'
#' @param model_fit A beezdemand_nlme object
#' @return Named list with AIC, BIC, logLik, sigma
#' @export
get_fit_statistics <- function(model_fit) {
  if (is.null(model_fit) || is.null(model_fit$model)) {
    return(NULL)
  }

  mdl <- model_fit$model
  summ <- tryCatch(summary(mdl), error = function(e) NULL)
  if (is.null(summ)) return(NULL)

  list(
    AIC = round(stats$AIC(mdl), 2),
    BIC = round(stats$BIC(mdl), 2),
    logLik = round(as.numeric(stats$logLik(mdl)), 2),
    sigma = round(summ$sigma, 4),
    df_residual = tryCatch(summ$dims$N - summ$dims$p, error = function(e) NA)
  )
}

#' Extract random effects variance components
#'
#' @param model_fit A beezdemand_nlme object
#' @return Data frame with Group, Name, StdDev columns, or NULL
#' @export
get_variance_components <- function(model_fit) {
  if (is.null(model_fit) || is.null(model_fit$model)) {
    return(NULL)
  }

  vc <- tryCatch(
    nlme$VarCorr(model_fit$model),
    error = function(e) NULL
  )

  if (is.null(vc)) return(NULL)

  vc_df <- as.data.frame(vc, stringsAsFactors = FALSE)

  # VarCorr returns a matrix-like object; convert to clean data frame
  if (ncol(vc_df) >= 2) {
    result <- data.frame(
      Component = rownames(vc_df),
      Variance = vc_df[, 1],
      StdDev = vc_df[, 2],
      stringsAsFactors = FALSE
    )
    return(result)
  }

  NULL
}
