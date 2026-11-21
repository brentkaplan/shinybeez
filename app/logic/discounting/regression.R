#' Discounting Regression Fitting and Formatting
#'
#' Pure functions for fitting discounting regression models and
#' formatting results.

box::use(
  beezdiscounting[fit_dd, results_dd],
  dplyr,
)

# Significant digits, not decimal places: a discounting rate, its standard error and
# interval, and a p-value are often far below 0.0001, which four decimal places turned
# into 0 in the table and its exports. The same goes for the optimiser's final tolerance
# (about 1e-8) and for the deviance and residual sigma of a close fit.
results_cols_4sig <- c(
  "estimate", "std.error", "conf_low", "conf_high", "p.value", "finTol", "deviance", "sigma"
)

#' Format regression results with rounding and id sorting
#'
#' Pooled and Mean fits return a single aggregate row with no `id` column, so
#' the id relevel/sort is applied only when an `id` column is present (i.e. for
#' per-subject Two Stage fits).
#'
#' @param results Data frame from results_dd()
#' @param id_levels Character vector of original id levels for factor ordering
#' @return Data frame with the estimate, its error, its interval, p, finTol, deviance and
#'   sigma to 4 significant digits and other numeric cols to 4dp; id sorted when present
#' @export
format_regression_results <- function(results, id_levels) {
  results <- results |>
    dplyr$mutate(
      dplyr$across(
        dplyr$where(is.numeric) & !dplyr$any_of(results_cols_4sig), \(x) round(x, 4)
      ),
      dplyr$across(dplyr$any_of(results_cols_4sig), \(x) signif(x, 4))
    )

  if (!"id" %in% names(results)) {
    return(results)
  }

  results |>
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
