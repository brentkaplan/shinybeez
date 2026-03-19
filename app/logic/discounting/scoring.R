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
  random <- grepl("random", imputation, ignore.case = TRUE)
  impute_method <- sub("_random$", "", imputation, ignore.case = TRUE)
  list(
    impute_method = impute_method,
    random = random
  )
}

#' Translate raw R error messages to user-friendly messages
#'
#' @param msg Character error message from R
#' @return Character user-friendly error message
#' @export
friendly_discounting_error <- function(msg) {
  patterns <- list(
    list(
      pattern = "In index: 1",
      friendly = "Data format error: please check that your data matches the expected format for this scoring method."
    ),
    list(
      pattern = "Impute method must be one of",
      friendly = "Invalid imputation method selected. Please choose a different option."
    ),
    list(
      pattern = "argument is of length zero",
      friendly = "A required column appears to be missing. Please verify your column names."
    ),
    list(
      pattern = "undefined columns selected",
      friendly = "Your data is missing expected columns. Please check the documentation for required columns."
    ),
    list(
      pattern = "Response length not equal to 27",
      friendly = "MCQ data must contain exactly 27 items per participant. Please check your data."
    )
  )

  for (p in patterns) {
    if (grepl(p$pattern, msg, fixed = TRUE)) return(p$friendly)
  }

  paste0("An error occurred during scoring: ", msg)
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
