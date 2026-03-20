#' 5.5 Trial (Minute Task) Discounting Computations
#'
#' Pure functions for 5.5 Trial Delay and Probability Discounting.

box::use(
  beezdiscounting[calc_dd, calc_pd],
)

#' Compute 5.5 Trial Delay Discounting results
#'
#' @param data Data frame in Qualtrics ResponseId/I1-I31 format
#' @return Data frame of delay discounting results from calc_dd()
#' @export
compute_five_trial_dd <- function(data) {
  calc_dd(restore_qualtrics_case(data))
}

#' Compute 5.5 Trial Probability Discounting results
#'
#' @param data Data frame in Qualtrics ResponseId/I1-I31 format
#' @return Data frame of probability discounting results from calc_pd()
#' @export
compute_five_trial_pd <- function(data) {
  calc_pd(restore_qualtrics_case(data))
}

#' Restore Qualtrics column case expected by beezdiscounting
#'
#' Column names are lowercased on upload, but beezdiscounting::calc_dd/calc_pd
#' expect original Qualtrics case (ResponseId, I1-I31).
#' @param data Data frame with lowercased Qualtrics columns
#' @return Data frame with restored column case
restore_qualtrics_case <- function(data) {
  names(data)[names(data) == "responseid"] <- "ResponseId"
  names(data) <- sub("^i(\\d+)$", "I\\1", names(data))
  data
}
