#' Discounting Logic Module
#'
#' Exports utility functions for discounting analysis.

box::use(
  . / regression,
  . / scoring,
  . / systematic,
)

#' @export
box::export(
  regression,
  scoring,
  systematic
)
