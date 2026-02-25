#' Empirical Demand Measures
#'
#' Pure functions for computing descriptive statistics and empirical
#' demand measures, with optional group handling.

box::use(
  beezdemand[GetDescriptives, GetEmpirical],
  dplyr,
  stats[aggregate],
)

#' Compute descriptive statistics for demand data
#'
#' @param data Data frame with id, x, y (and optionally group) columns
#' @param is_grouped Logical; whether to compute by group
#' @return Data frame of descriptive statistics
#' @export
compute_descriptives <- function(data, is_grouped = FALSE) {
  if (is_grouped) {
    if (!"group" %in% colnames(data)) {
      stop("Grouping requested but no 'group' column found in data.")
    }
    data_agg <- data |>
      dplyr$mutate(group = "aggregate")
    desc_agg <- data_agg |>
      GetDescriptives(dat = _, bwplot = FALSE) |>
      dplyr$mutate(group = "aggregate")
    desc_by_group <- data |>
      dplyr$group_by(group) |>
      dplyr$group_modify(~ GetDescriptives(dat = .x, bwplot = FALSE))
    descriptives <- dplyr$bind_rows(desc_agg, desc_by_group) |>
      dplyr$relocate(group, .before = Price)
  } else {
    descriptives <- data |>
      GetDescriptives(dat = _, bwplot = FALSE)
  }

  descriptives |>
    dplyr$mutate(dplyr$across(dplyr$where(is.numeric), round, 2))
}

#' Compute empirical demand measures
#'
#' @param data Data frame with id, x, y (and optionally group) columns
#' @param is_grouped Logical; whether to compute by group
#' @return Data frame of empirical measures, or NULL on error
#' @export
compute_empirical_measures <- function(data, is_grouped = FALSE) {
  data_agg <- aggregate(y ~ x, data, mean, na.rm = TRUE)
  data_agg$id <- "aggregate"

  if (is_grouped) {
    if (!"group" %in% colnames(data)) {
      stop("Grouping requested but no 'group' column found in data.")
    }
    data_group_agg <- aggregate(y ~ x + group, data, mean, na.rm = TRUE)
    data_group_agg$id <- "group aggregate"

    emp_agg <- GetEmpirical(data_agg) |>
      dplyr$mutate(
        dplyr$across(dplyr$where(is.numeric), round, 1),
        group = "aggregate"
      ) |>
      dplyr$relocate(group, .before = id)
    emp_group_agg <- dplyr$group_by(data_group_agg, group) |>
      dplyr$group_modify(~ GetEmpirical(.x))
    emp_individual <- dplyr$group_by(data, group) |>
      dplyr$group_modify(~ GetEmpirical(.x))
    empirical <- dplyr$bind_rows(emp_agg, emp_group_agg, emp_individual)
  } else {
    emp_agg <- GetEmpirical(data_agg)
    emp_individual <- GetEmpirical(data)
    empirical <- dplyr$bind_rows(emp_agg, emp_individual)
  }

  empirical |>
    dplyr$mutate(dplyr$across(dplyr$where(is.numeric), round, 2))
}
