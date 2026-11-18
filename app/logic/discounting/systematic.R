#' Systematic Criteria for Discounting Data
#'
#' Wraps beezdiscounting::check_unsystematic for the discounting workflow,
#' applying criteria per participant with sorting by original id order.

box::use(
  beezdiscounting[check_unsystematic],
  dplyr,
  purrr,
)

#' Compute systematic criteria for discounting data
#'
#' Splits data by id, applies check_unsystematic per participant,
#' and returns results sorted by original id order.
#'
#' @param data Data frame with id, x (numeric delay), y columns
#' @param c1 Local change criterion (default 0.2)
#' @param c2 Global trend criterion (default 0.1)
#' @return Data frame with systematic criteria results, one row per id,
#'   sorted by original id order
#' @export
compute_systematic_discounting <- function(data, c1 = 0.2, c2 = 0.1) {
  original_levels <- unique(data$id)

  data |>
    dplyr$group_split(id) |>
    purrr$map_dfr(
      # check_unsystematic() compares consecutive rows and first with last, so each
      # participant's points must be in delay order whatever order they were uploaded in
      ~ check_unsystematic(
        dplyr$arrange(.x, x),
        ll = 1,
        c1 = c1,
        c2 = c2
      )
    ) |>
    dplyr$mutate(id = factor(id, levels = original_levels)) |>
    dplyr$arrange(id)
}
