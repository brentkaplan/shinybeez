#' Discounting Logic Module
#'
#' Exports utility functions for discounting analysis.

box::use(
  . / five_trial,
  . / regression,
  . / scoring,
  . / systematic,
)

#' @export
box::export(
  five_trial,
  regression,
  scoring,
  systematic
)
