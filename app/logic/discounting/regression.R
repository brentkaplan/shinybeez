#' Discounting Regression Fitting and Formatting
#'
#' Pure functions for fitting discounting regression models and
#' formatting results.

box::use(
  beezdiscounting[fit_dd, results_dd],
  dplyr,
)

#' Format regression results with rounding and id sorting
#'
#' @param results Data frame from results_dd()
#' @param id_levels Character vector of original id levels for factor ordering
#' @return Data frame with numeric cols rounded to 4dp, id sorted
#' @export
format_regression_results <- function(results, id_levels) {
  results |>
    dplyr$mutate(dplyr$across(dplyr$where(is.numeric), \(x) round(x, 4))) |>
    dplyr$mutate(id = factor(id, levels = id_levels)) |>
    dplyr$arrange(id)
}

#' Fit discounting regression and return formatted results
#'
#' @param data Data frame with id, x, y columns
#' @param equation Character equation type for fit_dd
#' @param method Character aggregation method for fit_dd
#' @return List with dd_fit (raw fit object) and results (formatted data frame)
#' @export
fit_and_format_regression <- function(data, equation, method) {
  dd_fit <- fit_dd(data, equation = equation, method = method)
  results <- results_dd(dd_fit) |>
    format_regression_results(id_levels = unique(data$id))

  list(
    dd_fit = dd_fit,
    results = results
  )
}
