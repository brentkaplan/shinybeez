#' Export Utilities for Mixed Effects Demand
#'
#' Helper functions for building Excel export data structures.

box::use(
  dplyr,
  stats
)

#' Build summary sheet data for Excel export
#'
#' @param settings List with analysis settings (equation, factors, id_var, x_var, y_var, etc.)
#' @param version Application version string
#' @return Data frame with Item and Value columns for summary sheet
#' @export
build_summary_sheet <- function(settings, version = "1.0.0") {
  # Build string representations
  eq_str <- if (is.null(settings$equation)) {
    "N/A"
  } else {
    as.character(settings$equation)
  }

  factors_str <- if (
    is.null(settings$factors) ||
      length(settings$factors) == 0 ||
      identical(settings$factors, "None")
  ) {
    "None"
  } else {
    paste(settings$factors, collapse = ", ")
  }

  interaction_str <- if (isTRUE(settings$factor_interaction)) "Yes" else "No"

  id_str <- if (is.null(settings$id_var)) {
    "N/A"
  } else {
    as.character(settings$id_var)
  }
  x_str <- if (is.null(settings$x_var)) "N/A" else as.character(settings$x_var)
  y_str <- if (is.null(settings$y_var)) "N/A" else as.character(settings$y_var)

  ytrans_str <- if (identical(settings$equation, "zben")) {
    "Log-Log 4 (LL4)"
  } else {
    "None"
  }

  re_str <- if (
    is.null(settings$random_effects) || length(settings$random_effects) == 0
  ) {
    "N/A"
  } else {
    paste(settings$random_effects, collapse = ", ")
  }

  cov_str <- if (is.null(settings$covariance_structure)) {
    "N/A"
  } else {
    as.character(settings$covariance_structure)
  }

  # Build summary data frame
  summary_items <- c(
    "shinybeez Mixed-Effects Demand Analysis",
    "",
    "Export Date",
    "shinybeez Version",
    "",
    "--- Analysis Settings ---",
    "Equation",
    "Factor(s)",
    "Factor Interaction",
    "ID Variable",
    "X Variable",
    "Y Variable",
    "Y Transformation",
    "Random Effects",
    "Covariance Structure"
  )

  summary_values <- c(
    "",
    "",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    version,
    "",
    "",
    eq_str,
    factors_str,
    interaction_str,
    id_str,
    x_str,
    y_str,
    ytrans_str,
    re_str,
    cov_str
  )

  data.frame(
    Item = summary_items,
    Value = summary_values,
    stringsAsFactors = FALSE
  )
}

#' Add collapse levels info to summary data
#'
#' @param summary_data Existing summary data frame
#' @param collapse_info Collapse levels configuration
#' @return Updated summary data frame
#' @export
add_collapse_info <- function(summary_data, collapse_info) {
  if (
    is.null(collapse_info) ||
      identical(collapse_info, "ERROR_OVERLAP") ||
      identical(collapse_info, "ERROR_SINGLE_LEVEL")
  ) {
    return(summary_data)
  }

  collapse_rows <- data.frame(
    Item = c("", "--- Collapse Levels ---"),
    Value = c("", ""),
    stringsAsFactors = FALSE
  )
  summary_data <- rbind(summary_data, collapse_rows)

  if (!is.null(collapse_info$Q0)) {
    q0_str <- paste(names(collapse_info$Q0), collapse = ", ")
    q0_val <- if (nzchar(q0_str)) q0_str else "None"
    summary_data <- rbind(
      summary_data,
      data.frame(Item = "Q0 Collapse", Value = q0_val, stringsAsFactors = FALSE)
    )
  }

  if (!is.null(collapse_info$alpha)) {
    alpha_str <- paste(names(collapse_info$alpha), collapse = ", ")
    alpha_val <- if (nzchar(alpha_str)) alpha_str else "None"
    summary_data <- rbind(
      summary_data,
      data.frame(
        Item = "Alpha Collapse",
        Value = alpha_val,
        stringsAsFactors = FALSE
      )
    )
  }

  summary_data
}

#' Add fitting settings to summary data
#'
#' @param summary_data Existing summary data frame
#' @param nlme_ctrl NLME control settings list
#' @return Updated summary data frame
#' @export
add_fitting_settings <- function(summary_data, nlme_ctrl) {
  if (is.null(nlme_ctrl)) {
    return(summary_data)
  }

  fitting_rows <- data.frame(
    Item = c(
      "",
      "--- Fitting Settings ---",
      "maxIter",
      "pnlsMaxIter",
      "msMaxIter",
      "tolerance",
      "pnlsTol",
      "minScale",
      "niterEM"
    ),
    Value = c(
      "",
      "",
      as.character(nlme_ctrl$maxIter),
      as.character(nlme_ctrl$pnlsMaxIter),
      as.character(nlme_ctrl$msMaxIter),
      as.character(nlme_ctrl$tolerance),
      as.character(nlme_ctrl$pnlsTol),
      as.character(nlme_ctrl$minScale),
      as.character(nlme_ctrl$niterEM)
    ),
    stringsAsFactors = FALSE
  )

  rbind(summary_data, fitting_rows)
}

#' Build descriptives summary from data
#'
#' @param df Data frame with y_for_model column
#' @param grouping_vars Character vector of grouping variable names
#' @return Summarized data frame
#' @export
build_descriptives <- function(df, grouping_vars = character(0)) {
  if (is.null(df) || !"y_for_model" %in% names(df)) {
    return(NULL)
  }

  # Filter to present grouping vars
  grouping_vars_present <- intersect(grouping_vars, names(df))

  if (length(grouping_vars_present) == 0) {
    result <- df |>
      dplyr$summarise(
        N = dplyr$n(),
        Mean_Y_Model = mean(y_for_model, na.rm = TRUE),
        SD_Y_Model = stats$sd(y_for_model, na.rm = TRUE),
        Median_Y_Model = stats$median(y_for_model, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    result <- df |>
      dplyr$group_by(dplyr$across(dplyr$all_of(grouping_vars_present))) |>
      dplyr$summarise(
        N = dplyr$n(),
        Mean_Y_Model = mean(y_for_model, na.rm = TRUE),
        SD_Y_Model = stats$sd(y_for_model, na.rm = TRUE),
        Median_Y_Model = stats$median(y_for_model, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # Round numeric columns
  result <- dplyr$mutate(
    result,
    dplyr$across(where(is.numeric), ~ round(., 3))
  )

  result
}

#' Generate timestamp for filenames
#'
#' @return Character string with timestamp
#' @export
generate_export_timestamp <- function() {
  format(Sys.time(), "%Y%m%d_%H%M%S")
}

#' Build export filename
#'
#' @param prefix Filename prefix
#' @param extension File extension (without dot)
#' @return Complete filename with timestamp
#' @export
build_export_filename <- function(
  prefix = "shinybeez_export",
  extension = "xlsx"
) {
  timestamp <- generate_export_timestamp()
  paste0(prefix, "_", timestamp, ".", extension)
}
