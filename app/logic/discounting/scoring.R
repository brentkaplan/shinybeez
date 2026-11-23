#' MCQ Scoring and Formatting
#'
#' Pure functions for scoring the 27-Item MCQ and formatting results.

box::use(
  beezdiscounting[prop_ss, score_mcq27, summarize_mcq],
  dplyr,
  ggplot2,
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
#' @param data Long MCQ data frame with subjectid, questionid and response columns.
#' @return Invisibly TRUE; aborts with a precise message otherwise.
check_mcq_item_counts <- function(data) {
  # Data left over from another analysis type (e.g. id/x/y) used to skip this check and
  # reach score_mcq27(), which aborted with "argument is of length zero" or "replacement has
  # 1 row, data has 0". Name the problem instead.
  required <- c("subjectid", "questionid", "response")
  if (!is.null(data) && !all(required %in% colnames(data))) {
    stop(
      "This does not look like 27-Item MCQ data: it needs subjectid, questionid, response ",
      "columns (or subjectid plus 27 items, wide).",
      call. = FALSE
    )
  }
  if (is.null(data) || nrow(data) == 0) {
    stop("The MCQ data have no rows. Please upload a file with 27-item responses.", call. = FALSE)
  }

  # A missing or blank subject identifier cannot be scored and silently corrupts
  # the per-subject grouping below: table() drops NA entirely (so an all-NA
  # subject is invisible and its short count never surfaces) and treats "" as an
  # ordinary subject. Reject up front, mirroring the ResponseId blank-identifier
  # validation in five_trial.R.
  blank <- is.na(data$subjectid) | !nzchar(trimws(as.character(data$subjectid)))
  if (any(blank)) {
    stop(
      sum(blank), " row(s) have a missing or blank subject identifier. ",
      "Every MCQ row must have a subjectid.",
      call. = FALSE
    )
  }

  counts <- table(data$subjectid)
  short <- counts[counts != 27]
  if (length(short) > 0L) {
    detail <- paste0(
      "Subject ", names(short), " has ", as.integer(short),
      " of the 27 required MCQ items",
      collapse = "; "
    )
    stop(detail, ". Each subject must have exactly 27 items.", call. = FALSE)
  }

  # 27 rows is not 27 questions: a repeated question (and so a missing one) passed the
  # count, and the scorer returned NA or a k computed from the wrong items.
  question <- suppressWarnings(as.numeric(as.character(data$questionid)))
  outside <- is.na(question) | !question %in% 1:27
  if (any(outside)) {
    stop(
      "MCQ question numbers must be the whole numbers 1 to 27; found: ",
      paste(utils::head(unique(as.character(data$questionid[outside])), 5), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  repeated <- duplicated(data.frame(data$subjectid, question))
  if (any(repeated)) {
    first <- which(repeated)[1]
    stop(
      "Subject ", data$subjectid[first], " has question ", question[first],
      " more than once. Each subject must answer each of the 27 questions exactly once.",
      call. = FALSE
    )
  }
  invisible(TRUE)
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
      pattern = "data has 0",
      friendly = "The data have no rows to score. Please check that the file matches the selected analysis type."
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

  # score_mcq27() returns its imputed responses in question order but attaches them to
  # the rows as uploaded, so questions listed out of order came back with other
  # questions' answers. Put each participant's questions in numeric order first;
  # participants keep the order they were uploaded in.
  question_number <- suppressWarnings(as.numeric(as.character(data$questionid)))
  if (is.factor(data$questionid) && !anyNA(question_number)) {
    # the scorer orders a factor by its codes ("1", "10", "11", ...), not by number
    data$questionid <- question_number
  }
  data <- data[order(match(data$subjectid, unique(data$subjectid)), question_number), ]

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

#' Mean proportion of smaller-sooner choices at each k rank, across participants
#'
#' beezdiscounting::prop_ss() scores one participant: given several it keeps 27 rows in
#' total, not 27 per participant. Score each participant on their own, then average the
#' participants at each k rank.
#'
#' A participant's proportion at a k rank is NA when they answered none of its items;
#' those participants are left out of that rank's average. A rank nobody answered is NA.
#'
#' @param data Data frame with subjectid, questionid, response (27 rows per participant)
#' @return Tibble with k_rank and prop_ss (2 dp), classed `prop_ss_output` for plot()
#' @export
mean_prop_ss <- function(data) {
  # prop_ss() warns once per participant with a missing response. The app tells the user
  # about missing MCQ data itself, once, so that warning is dropped; any other is kept.
  per_participant <- withCallingHandlers(
    lapply(split(data, data$subjectid, drop = TRUE), prop_ss),
    warning = function(w) {
      if (grepl("Missing data found and ignored", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  out <- dplyr$bind_rows(per_participant) |>
    dplyr$group_by(k_rank) |>
    dplyr$summarise(prop_ss = mean_of_answered(exact_thirds(prop_ss)), .groups = "drop")

  class(out) <- class(per_participant[[1]])
  out
}

#' Plot of the mean proportion of smaller-sooner choices at each k rank
#'
#' beezdiscounting 0.4.0.9000's plot method labels every x tick NA (it indexes its label
#' vector by the k values). The labels are set here from the k values plotted.
#'
#' @param data Data frame with subjectid, questionid, response (27 rows per participant)
#' @return A ggplot
#' @export
plot_mean_prop_ss <- function(data) {
  out <- mean_prop_ss(data)
  k_labels <- vapply(
    sort(unique(out$k_rank)),
    format,
    character(1),
    scientific = FALSE,
    drop0trailing = TRUE
  )
  # Replacing the x scale that plot() added makes ggplot2 say so; nothing to report.
  suppressMessages(plot(out) + ggplot2$scale_x_discrete(labels = k_labels))
}

# prop_ss() rounds to 2 dp, so 1/3 and 2/3 arrive as 0.33 and 0.67. Restore those two
# before averaging, so the mean is rounded once. A participant who answered 1 or 2 of a
# rank's 3 items gives 0, 0.5 or 1, which are exact already.
exact_thirds <- function(x) {
  x[!is.na(x) & x == 0.33] <- 1 / 3
  x[!is.na(x) & x == 0.67] <- 2 / 3
  x
}

mean_of_answered <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  round(mean(x, na.rm = TRUE), 2)
}
