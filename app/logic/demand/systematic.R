#' Systematic Criteria for Demand Data
#'
#' Wraps beezdemand::CheckUnsystematic for the fixed-effects demand workflow,
#' where data is already in standard id/x/y format.

box::use(
  beezdemand[CheckUnsystematic],
  dplyr,
)

#' Compute systematic criteria with optional grouping
#'
#' @param data Data frame with id, x, y (and optionally group) columns
#' @param deltaq DeltaQ criterion value
#' @param bounce Bounce criterion value
#' @param reversals Reversals criterion value
#' @param ncons0 Consecutive zeros criterion value
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
        ~ CheckUnsystematic(
          dat = .x,
          deltaq = deltaq,
          bounce = bounce,
          reversals = reversals,
          ncons0 = ncons0
        )
      )
  } else {
    CheckUnsystematic(
      dat = data,
      deltaq = deltaq,
      bounce = bounce,
      reversals = reversals,
      ncons0 = ncons0
    )
  }
}
