#' EMM (Estimated Marginal Means) Computation for Mixed Effects Demand
#'
#' Thin wrapper over beezdemand's observed-grid EMM extractor. Kept separate
#' from emms_utils.R (which stays pure-format) so the package call lives in
#' tested logic rather than directly in the Shiny view.

box::use(
  beezdemand[get_observed_demand_param_emms],
)

#' Compute observed-grid demand parameter EMMs via beezdemand
#'
#' Thin wrapper over [beezdemand::get_observed_demand_param_emms()] for the
#' NLME backend. Returns a tibble of estimated marginal means for Q0, alpha,
#' and (optionally) EV at the observed factor combinations, with confidence
#' intervals. The downstream emms_utils formatters extract the Q0/alpha/EV
#' columns by case-insensitive grep, so the column-name contract matters.
#'
#' @param fit_obj A fitted `beezdemand_nlme` object
#' @param factors_in_emm Character vector of model factors to include in the
#'   EMM grid (NULL uses all model factors)
#' @param at Optional named list for covariate conditioning
#' @param include_ev Logical; include Essential Value columns (default TRUE)
#' @param ci_level Confidence level for the intervals (default 0.95)
#' @return A data frame (tibble) of EMMs with Q0-, alpha-, and EV-bearing
#'   columns plus their confidence intervals
#' @export
run_observed_emms <- function(
  fit_obj,
  factors_in_emm = NULL,
  at = NULL,
  include_ev = TRUE,
  ci_level = 0.95
) {
  get_observed_demand_param_emms(
    fit_obj = fit_obj,
    factors_in_emm = factors_in_emm,
    at = at,
    include_ev = include_ev,
    ci_level = ci_level
  )
}
