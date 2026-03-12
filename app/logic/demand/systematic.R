#' Systematic Criteria for Demand Data
#'
#' Wraps beezdemand::check_systematic_demand for the fixed-effects demand
#' workflow, where data is already in standard id/x/y format.

box::use(
  beezdemand[check_systematic_demand],
  dplyr,
)

#' Compute systematic criteria with optional grouping
#'
#' @param data Data frame with id, x, y (and optionally group) columns
#' @param deltaq Passed as trend_threshold to check_systematic_demand
#' @param bounce Passed as bounce_threshold to check_systematic_demand
#' @param reversals Passed as max_reversals to check_systematic_demand
#' @param ncons0 Passed as consecutive_zeros to check_systematic_demand
#' @param is_grouped Logical; whether to compute by group
#' @return Data frame with systematic criteria results
#' @export
compute_systematic <- function(
    data,
    deltaq = 0.025,
    bounce = 0.10,
    reversals = 0,
    ncons0 = 2,
    is_grouped = FALSE) {
  if (is_grouped) {
    if (!"group" %in% colnames(data)) {
      stop("Grouping requested but no 'group' column found in data.")
    }
    data |>
      dplyr$group_by(group) |>
      dplyr$group_modify(
        ~ check_systematic_demand(
          data = .x,
          trend_threshold = deltaq,
          bounce_threshold = bounce,
          max_reversals = reversals,
          consecutive_zeros = ncons0
        )$results
      )
  } else {
    check_systematic_demand(
      data = data,
      trend_threshold = deltaq,
      bounce_threshold = bounce,
      max_reversals = reversals,
      consecutive_zeros = ncons0
    )$results
  }
}
