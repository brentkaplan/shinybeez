#' Demand Curve Fitting
#'
#' Pure functions for fitting demand curves via beezdemand::fit_demand_fixed,
#' with support for grouped and ungrouped data.

box::use(
  beezdemand[fit_demand_fixed],
  dplyr,
)

#' Map equation UI label to beezdemand equation code
#'
#' @param eq_label Character string from UI selectInput
#' @return Character code for beezdemand ("koff" or "hs")
#' @export
resolve_equation <- function(eq_label) {
  switch(
    eq_label,
    "Exponentiated (with k)" = "koff",
    "Exponential (with k)" = "hs",
    stop("Unknown equation label: ", eq_label)
  )
}

#' Resolve k value from UI input
#'
#' Converts k to numeric if it's a recognized numeric string,
#' otherwise passes through as-is (e.g., "ind", "fit", "range").
#'
#' @param kval Character or numeric k value from UI
#' @param k_values Numeric vector of recognized k values
#' @return Numeric or character k value for fit_demand_fixed
#' @export
resolve_k_value <- function(kval, k_values) {
  if (kval %in% as.character(k_values)) {
    as.numeric(kval)
  } else {
    kval
  }
}

#' Resolve aggregation parameter for fit_demand_fixed
#'
#' @param agg_label Character analysis type from UI
#' @return NULL (for individual) or the aggregation type
#' @export
resolve_aggregation <- function(agg_label) {
  if (is.null(agg_label) || agg_label == "Ind") NULL else agg_label
}

#' Resolve Q0 constraint
#'
#' @param q0_val Numeric Q0 value from UI
#' @param fix_q0 Logical whether to constrain Q0
#' @return NULL or the Q0 value
#' @export
resolve_q0_constraint <- function(q0_val, fix_q0) {
  if (is.null(q0_val) || !fix_q0) NULL else q0_val
}

# Column names for rounding in results
results_cols_2dp <- c(
  "Q0d", "K", "R2", "Q0se", "AbsSS", "SdRes",
  "Q0Low", "Q0High", "EV", "Omaxd", "Pmaxd", "Omaxa", "Pmaxa"
)
results_cols_4dp <- c("Alpha", "Alphase", "AlphaLow", "AlphaHigh")

#' Format fit_demand_fixed output into a clean results data frame
#'
#' @param output List output from fit_demand_fixed (or synthetic grouped list)
#' @return Formatted data frame with rounded columns
#' @export
format_demand_results <- function(output) {
  output[[1]] |>
    dplyr$select(!(Intensity:Pmaxe)) |>
    dplyr$mutate(
      dplyr$across(dplyr$all_of(results_cols_2dp), \(x) round(x, 2)),
      dplyr$across(dplyr$all_of(results_cols_4dp), \(x) round(x, 4))
    )
}

#' Fit demand curves for ungrouped data
#'
#' @param data Data frame with id, x, y columns
#' @param eq Equation code ("koff" or "hs")
#' @param agg Aggregation type (NULL, "Pooled", "Mean")
#' @param k Numeric or character k value
#' @param constrainq0 NULL or numeric Q0 constraint
#' @return List with output (raw fit_demand_fixed result) and results (formatted df),
#'   or NULL on error
#' @export
fit_demand_ungrouped <- function(data, eq, agg, k, constrainq0 = NULL) {
  output <- fit_demand_fixed(
    data = data,
    equation = eq,
    agg = agg,
    k = k,
    constrainq0 = constrainq0
  )
  list(
    output = output,
    results = format_demand_results(output)
  )
}

#' Fit demand curves for grouped data
#'
#' @param data Data frame with id, x, y, group columns
#' @param eq Equation code ("koff" or "hs")
#' @param agg Aggregation type (NULL, "Pooled", "Mean")
#' @param k Numeric or character k value
#' @param constrainq0 NULL or numeric Q0 constraint
#' @return List with output (raw fit_demand_fixed result) and results (formatted df),
#'   or NULL if all groups fail
#' @export
fit_demand_grouped <- function(data, eq, agg, k, constrainq0 = NULL) {
  if (!"group" %in% colnames(data)) {
    stop("Grouping requested but no 'group' column found in data.")
  }

  group_fits <- data |>
    dplyr$group_by(group) |>
    dplyr$group_map(
      ~ {
        fit_result <- tryCatch(
          fit_demand_fixed(
            data = .x,
            equation = eq,
            agg = agg,
            k = k,
            constrainq0 = constrainq0
          ),
          error = function(e) NULL
        )
        if (!is.null(fit_result)) {
          list(
            group = dplyr$first(.x$group),
            fit_result_1 = fit_result[[1]],
            fit_result_3 = fit_result[[3]]
          )
        }
      },
      .keep = TRUE
    )

  # Filter out NULL results from failed fits
  group_fits <- group_fits[!sapply(group_fits, is.null)]

  if (length(group_fits) == 0) {
    return(NULL)
  }

  output <- vector("list", length = 3)
  output[[1]] <- dplyr$bind_rows(lapply(group_fits, function(x) {
    cbind(group = x$group, x$fit_result_1)
  }))
  output[[3]] <- dplyr$bind_rows(lapply(group_fits, function(x) {
    cbind(group = x$group, x$fit_result_3[[1]])
  }))

  list(
    output = output,
    results = format_demand_results(output)
  )
}
