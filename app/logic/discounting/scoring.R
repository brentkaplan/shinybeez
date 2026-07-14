#' MCQ Scoring and Formatting
#'
#' Pure functions for scoring the 27-Item MCQ and formatting results.

box::use(
  beezdiscounting[score_mcq27, summarize_mcq],
  dplyr,
)

# The only methods beezdiscounting accepts (mcq.R:27-28). Anything else — an empty string
# from a selectInput that has not initialised yet, a stale value, NA — is rejected outright
# with "Impute method must be one of none, ggm, GGM, inn, INN" (signature 1155c2d9, 21 events).
#
# GGM (Group Geometric Mean) and INN (Item Nearest Neighbor) are the imputation approaches of
# Yeh et al. (2023), doi:10.1371/journal.pone.0292258. Note that `random` is a SEPARATE
# boolean argument to score_mcq27() — "insert a random draw (0 or 1) for NAs" — not an
# imputation method. The UI's "INN (random)" is INN plus that flag, which is why the
# "_random" suffix is stripped off the method before it is validated here.
valid_impute_methods <- c("none", "ggm", "GGM", "inn", "INN")

#' Fail early, and precisely, when a subject does not have all 27 MCQ items
#'
#' score_mcq27() aborts with "Response length not equal to 27 for subjectid: N", naming the
#' subject but not what is actually wrong with it. Imputation cannot rescue this: it fills
#' missing *values*, not missing *rows*. Check up front so the user is told which subjects are
#' short and by how much, and so the whole results panel does not collapse on a raw abort.
#'
#' @param data Long MCQ data frame with a subjectid column.
#' @return Invisibly TRUE; aborts with a precise message otherwise.
check_mcq_item_counts <- function(data) {
  if (is.null(data) || !"subjectid" %in% colnames(data)) {
    return(invisible(TRUE))
  }

  counts <- table(data$subjectid)
  short <- counts[counts != 27]
  if (length(short) == 0L) {
    return(invisible(TRUE))
  }

  detail <- paste0(
    "Subject ", names(short), " has ", as.integer(short),
    " of the 27 required MCQ items",
    collapse = "; "
  )
  stop(detail, ". Each subject must have exactly 27 items.", call. = FALSE)
}

#' Resolve imputation settings from UI input
#'
#' Anything not recognised falls back to "none" rather than being handed to beezdiscounting,
#' which aborts on an unknown method. An empty string is the common case: the selectInput
#' reports "" before it has initialised, and the Calculate button was reachable in that window.
#'
#' @param imputation Character imputation method from UI ("none", or a method)
#' @return List with impute_method (character) and random (logical)
#' @export
resolve_imputation <- function(imputation) {
  if (is.null(imputation) || length(imputation) != 1L || is.na(imputation)) {
    imputation <- "none"
  }
  imputation <- as.character(imputation)

  random <- grepl("random", imputation, ignore.case = TRUE)
  impute_method <- sub("_random$", "", imputation, ignore.case = TRUE)

  if (!nzchar(impute_method) || !impute_method %in% valid_impute_methods) {
    impute_method <- "none"
    random <- FALSE
  }

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

  # beezdiscounting wraps its abort messages across lines with runs of padding spaces, so the
  # raw text looks like "Response length\n      not equal to 27\n      for subjectid: 1".
  # Matching that with fixed = TRUE against a flat pattern never succeeded, which is why every
  # one of these translations silently fell through to the generic fallback in production.
  # Flatten the whitespace before matching; keep the original for the fallback message.
  flattened <- gsub("[[:space:]]+", " ", trimws(msg))

  for (p in patterns) {
    if (grepl(p$pattern, flattened, fixed = TRUE)) return(p$friendly)
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
  check_mcq_item_counts(data)

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
