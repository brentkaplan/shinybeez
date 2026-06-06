#' Systematic Criteria for Demand Data
#'
#' Wraps beezdemand::check_systematic_demand for the fixed-effects demand
#' workflow, where data is already in standard id/x/y format.

box::use(
  beezdemand[check_systematic_demand],
  dplyr,
)

# Coalesce a NULL / empty / all-NA value back to a default. Shiny numericInputs
# report NULL until they initialize, so a reactive that fires on data upload can
# call compute_systematic() with NULL thresholds during that brief init race;
# without this, the NULLs reach check_systematic_demand() and abort.
coalesce_default <- function(x, default) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) default else x
}

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
  deltaq <- coalesce_default(deltaq, 0.025)
  bounce <- coalesce_default(bounce, 0.10)
  reversals <- coalesce_default(reversals, 0)
  ncons0 <- coalesce_default(ncons0, 2)

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
