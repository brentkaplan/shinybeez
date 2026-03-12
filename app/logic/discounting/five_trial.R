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
  calc_dd(data)
}

#' Compute 5.5 Trial Probability Discounting results
#'
#' @param data Data frame in Qualtrics ResponseId/I1-I31 format
#' @return Data frame of probability discounting results from calc_pd()
#' @export
compute_five_trial_pd <- function(data) {
  calc_pd(data)
}
