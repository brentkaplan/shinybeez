#' MCQ Scoring and Formatting
#'
#' Pure functions for scoring the 27-Item MCQ and formatting results.

box::use(
  beezdiscounting[score_mcq27, summarize_mcq],
  dplyr,
)

#' Resolve imputation settings from UI input
#'
#' @param imputation Character imputation method from UI ("none", or a method)
#' @return List with impute_method (NULL or character) and random (logical)
#' @export
resolve_imputation <- function(imputation) {
  if (is.null(imputation)) imputation <- "none"
  list(
    impute_method = imputation,
    random = grepl("random", imputation, ignore.case = TRUE)
  )
}

#' Format MCQ results by rounding standard columns
#'
#' @param results Data frame from score_mcq27()$results
#' @return Data frame with _k cols rounded to 6dp, _prop to 3dp, _cons to 3dp
#' @export
format_mcq_results <- function(results) {
  results |>
    dplyr$mutate(dplyr$across(dplyr$contains("_k"), \(x) round(x, 6))) |>
    dplyr$mutate(dplyr$across(dplyr$contains("_prop"), \(x) round(x, 3))) |>
    dplyr$mutate(dplyr$across(dplyr$contains("_cons"), \(x) round(x, 3)))
}

#' Score MCQ and return formatted results
#'
#' @param data Data frame with MCQ response data
#' @param imputation Character imputation method from UI
#' @param trans Character transformation method
#' @return List with results (formatted data frame), data (raw scored data),
#'   and summary (summary statistics)
#' @export
score_and_format_mcq <- function(data, imputation = "none", trans = "none") {
  imp <- resolve_imputation(imputation)

  calc_results <- score_mcq27(
    data,
    impute_method = imp$impute_method,
    random = imp$random,
    return_data = TRUE,
    trans = trans
  )

  list(
    results = format_mcq_results(calc_results$results),
    data = calc_results$data,
    summary = summarize_mcq(calc_results$results)
  )
}
