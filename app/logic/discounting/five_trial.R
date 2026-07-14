#' 5.5 Trial (Minute Task) Discounting Computations
#'
#' Pure functions for 5.5 Trial Delay and Probability Discounting.

box::use(
  beezdiscounting[calc_dd, calc_pd],
  stats,
)

#' The Qualtrics 5.5-Trial column vocabulary, in the case beezdiscounting expects
#'
#' beezdiscounting's internals are case-sensitive: `timing_dd()`/`timing_pd()`
#' rename columns with a literal `gsub("Timing_First Click", ...)` and then
#' `separate()` on "-". Uploaded names arrive lowercased (file_input.R), so every
#' one of these has to be restored, not just `ResponseId` and the bare items.
#'
#' @return Character vector of canonical column names
qualtrics_columns <- function() {
  items <- paste0("I", 1:31)
  attend <- c("Attend-LL", "Attend-SS")
  timing <- c(
    "Timing_First Click", "Timing_Last Click",
    "Timing_Page Submit", "Timing_Click Count"
  )
  timed <- as.vector(t(outer(c(items, attend), timing, paste, sep = "-")))
  c("ResponseId", items, attend, timed, "Amount", "Currency", "Commodity")
}

#' Restore Qualtrics column case expected by beezdiscounting
#'
#' Column names are lowercased on upload, but beezdiscounting::calc_dd/calc_pd
#' expect the original Qualtrics case. Restoring only `ResponseId` and the bare
#' `I1`-`I31` items left the timing and attention-check columns lowercased, which
#' made calc_dd abort with "Each row of output must be identified by a unique
#' combination of keys" on otherwise valid uploads.
#'
#' Columns outside the known Qualtrics vocabulary are left alone.
#'
#' @param data Data frame with lowercased Qualtrics columns
#' @return Data frame with restored column case
#' @export
restore_qualtrics_case <- function(data) {
  canonical <- qualtrics_columns()
  lookup <- stats$setNames(canonical, tolower(canonical))

  lowered <- tolower(names(data))
  known <- lowered %in% names(lookup)
  names(data)[known] <- unname(lookup[lowered[known]])
  data
}

#' Validate a 5.5-Trial upload before it reaches beezdiscounting
#'
#' calc_dd/calc_pd select `ResponseId`, every `I1`-`I31` item, and the timing
#' columns by name. Missing any of them produced a raw tidyselect abort
#' ("Can't select columns that don't exist") rather than a usable message.
#'
#' @param data Data frame as stored by the uploader (column names lowercased)
#' @return TRUE, or a character error message naming what is missing
#' @export
validate_five_trial <- function(data) {
  present <- tolower(names(data))

  if (!"responseid" %in% present) {
    return(paste0(
      "This does not look like a Qualtrics 5.5-Trial export: the `ResponseId` ",
      "column is missing. Please use the 5.5-Trial template from the welcome page."
    ))
  }

  expected_items <- paste0("i", 1:31)
  missing_items <- expected_items[!expected_items %in% present]
  if (length(missing_items) > 0) {
    return(paste0(
      "The 5.5-Trial data are missing ", length(missing_items),
      " item column(s): ",
      paste(toupper(missing_items), collapse = ", "),
      ". All of I1-I31 are required."
    ))
  }

  # beezdiscounting selects Attend-SS/Attend-LL by name; without them calc_dd
  # aborts with "Column `Attend-SS` doesn't exist".
  expected_attend <- c("attend-ll", "attend-ss")
  missing_attend <- expected_attend[!expected_attend %in% present]
  if (length(missing_attend) > 0) {
    return(paste0(
      "The 5.5-Trial data are missing the attention-check column(s): ",
      paste(c("Attend-LL", "Attend-SS")[expected_attend %in% missing_attend],
        collapse = ", "
      ),
      ". Please use the 5.5-Trial template from the welcome page."
    ))
  }

  # timing_dd()/timing_pd() pivot every Timing column and then spread on the
  # measure, so a partial set of measures produces a ragged frame rather than a
  # clean abort. Require all four.
  expected_measures <- c(
    "timing_first click", "timing_last click",
    "timing_page submit", "timing_click count"
  )
  missing_measures <- expected_measures[
    !vapply(
      expected_measures,
      function(m) any(grepl(m, present, fixed = TRUE)),
      logical(1)
    )
  ]
  if (length(missing_measures) > 0) {
    return(paste0(
      "The 5.5-Trial data are missing Timing measure(s): ",
      paste(missing_measures, collapse = ", "),
      ". Export the Qualtrics survey with all four timing measures ",
      "(First Click, Last Click, Page Submit, Click Count)."
    ))
  }

  TRUE
}

#' Compute 5.5 Trial Delay Discounting results
#'
#' @param data Data frame in Qualtrics ResponseId/I1-I31 format
#' @return Data frame of delay discounting results from calc_dd()
#' @export
compute_five_trial_dd <- function(data) {
  chk <- validate_five_trial(data)
  if (is.character(chk)) stop(chk, call. = FALSE)
  calc_dd(restore_qualtrics_case(data))
}

#' Compute 5.5 Trial Probability Discounting results
#'
#' @param data Data frame in Qualtrics ResponseId/I1-I31 format
#' @return Data frame of probability discounting results from calc_pd()
#' @export
compute_five_trial_pd <- function(data) {
  chk <- validate_five_trial(data)
  if (is.character(chk)) stop(chk, call. = FALSE)
  calc_pd(restore_qualtrics_case(data))
}
