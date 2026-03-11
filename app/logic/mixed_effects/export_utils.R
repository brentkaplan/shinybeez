#' Export Utilities for Mixed Effects Demand
#'
#' Helper functions for building Excel export data structures.

box::use(
  dplyr,
  stats
)

# --- Excel Styling Constants & Factories ---

#' Color palette for Excel export styling
#' @export
excel_palette <- list(
  HEADER_BG = "#2C3E50",
  HEADER_FONT = "#FFFFFF",
  SECTION_BG = "#4A6FA5",
  SECTION_FONT = "#FFFFFF",
  TITLE_FONT = "#2C3E50",
  STRIPE_BG = "#F2F4F7",
  BORDER_COLOR = "#BDC3C7",
  TAB_SUMMARY = "#2C3E50",
  TAB_DATA = "#27AE60",
  TAB_MODEL = "#8E44AD",
  TAB_COMPARISON = "#E67E22",
  TAB_PLOT = "#16A085"
)

#' Create header style (navy bg, white bold, centered)
#' @param openxlsx The openxlsx module reference
#' @return An openxlsx style object
#' @export
create_header_style <- function(openxlsx) {
  openxlsx$createStyle(
    fontName = "Calibri",
    fontSize = 11,
    fontColour = excel_palette$HEADER_FONT,
    bgFill = excel_palette$HEADER_BG,
    textDecoration = "bold",
    halign = "center",
    wrapText = TRUE,
    border = "TopBottomLeftRight",
    borderColour = excel_palette$BORDER_COLOR,
    borderStyle = "thin"
  )
}

#' Create zebra stripe style (light gray bg, thin borders)
#' @param openxlsx The openxlsx module reference
#' @return An openxlsx style object
#' @export
create_stripe_style <- function(openxlsx) {
  openxlsx$createStyle(
    fontName = "Calibri",
    fontSize = 11,
    bgFill = excel_palette$STRIPE_BG,
    border = "TopBottomLeftRight",
    borderColour = excel_palette$BORDER_COLOR,
    borderStyle = "thin"
  )
}

#' Create body border style (white bg, thin borders)
#' @param openxlsx The openxlsx module reference
#' @return An openxlsx style object
#' @export
create_body_border_style <- function(openxlsx) {
  openxlsx$createStyle(
    fontName = "Calibri",
    fontSize = 11,
    border = "TopBottomLeftRight",
    borderColour = excel_palette$BORDER_COLOR,
    borderStyle = "thin"
  )
}

#' Create number format style with borders
#' @param openxlsx The openxlsx module reference
#' @param fmt Number format string (e.g., "0.0000")
#' @return An openxlsx style object
#' @export
create_numfmt_style <- function(openxlsx, fmt) {
  openxlsx$createStyle(numFmt = fmt)
}

#' Create summary title style (18pt bold navy)
#' @param openxlsx The openxlsx module reference
#' @return An openxlsx style object
#' @export
create_summary_title_style <- function(openxlsx) {
  openxlsx$createStyle(
    fontName = "Calibri",
    fontSize = 18,
    fontColour = excel_palette$TITLE_FONT,
    textDecoration = "bold"
  )
}

#' Create summary section header style (blue bg, white bold)
#' @param openxlsx The openxlsx module reference
#' @return An openxlsx style object
#' @export
create_summary_section_style <- function(openxlsx) {
  openxlsx$createStyle(
    fontName = "Calibri",
    fontSize = 11,
    fontColour = excel_palette$SECTION_FONT,
    bgFill = excel_palette$SECTION_BG,
    textDecoration = "bold"
  )
}

#' Create summary label style (bold 11pt)
#' @param openxlsx The openxlsx module reference
#' @return An openxlsx style object
#' @export
create_summary_label_style <- function(openxlsx) {
  openxlsx$createStyle(
    fontName = "Calibri",
    fontSize = 11,
    textDecoration = "bold"
  )
}

#' Create model text style (Consolas 10pt monospace)
#' @param openxlsx The openxlsx module reference
#' @return An openxlsx style object
#' @export
create_model_text_style <- function(openxlsx) {
  openxlsx$createStyle(fontName = "Consolas", fontSize = 10)
}

#' Detect number format for a column based on its name
#'
#' @param col_name Column name string
#' @return Number format string
#' @export
detect_number_format <- function(col_name) {
  lc <- tolower(col_name)
  if (grepl("p[._]?value|^pr$|^pvalue$", lc)) return("0.0000")
  if (grepl("^se$|^std|std[._]error", lc)) return("0.0000")
  if (grepl("estimate|ratio|coefficient", lc)) return("0.0000")
  if (grepl("t[._]value|z[._]value|^df$", lc)) return("0.00")
  if (grepl("^n$|count|n_positive", lc)) return("0")
  "0.000"
}

#' Apply professional styling to a data sheet
#'
#' Adds header styling, frozen panes, zebra stripes, borders,
#' and smart number formatting to a sheet after data is written.
#'
#' @param wb openxlsx workbook object
#' @param sheet_name Name of the worksheet
#' @param data Data frame that was written to the sheet
#' @param openxlsx The openxlsx module reference
#' @export
style_data_sheet <- function(wb, sheet_name, data, openxlsx) {
  n_col <- ncol(data)
  n_row <- nrow(data)
  if (n_col == 0 || n_row == 0) return(invisible(NULL))

  # Header style on row 1
  header_style <- create_header_style(openxlsx)
  openxlsx$addStyle(
    wb, sheet_name, header_style,
    rows = 1, cols = seq_len(n_col), gridExpand = TRUE
  )

  # Freeze header row
  openxlsx$freezePane(wb, sheet_name, firstActiveRow = 2)

  # Zebra stripes + body borders
  if (n_row >= 1) {
    body_rows <- seq(2, n_row + 1)
    odd_rows <- body_rows[body_rows %% 2 == 0]
    even_rows <- body_rows[body_rows %% 2 == 1]

    body_style <- create_body_border_style(openxlsx)
    openxlsx$addStyle(
      wb, sheet_name, body_style,
      rows = body_rows, cols = seq_len(n_col),
      gridExpand = TRUE, stack = TRUE
    )

    if (length(even_rows) > 0) {
      stripe_style <- create_stripe_style(openxlsx)
      openxlsx$addStyle(
        wb, sheet_name, stripe_style,
        rows = even_rows, cols = seq_len(n_col),
        gridExpand = TRUE, stack = TRUE
      )
    }
  }

  # Smart number formatting per column
  col_names <- names(data)
  for (i in seq_len(n_col)) {
    if (is.numeric(data[[i]])) {
      fmt <- detect_number_format(col_names[i])
      fmt_style <- create_numfmt_style(openxlsx, fmt)
      openxlsx$addStyle(
        wb, sheet_name, fmt_style,
        rows = seq(2, n_row + 1), cols = i,
        gridExpand = TRUE, stack = TRUE
      )
    }
  }

  invisible(NULL)
}

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

#' Build standard DT export buttons configuration
#'
#' Creates a consistent button configuration for DT tables with copy, print,
#' CSV, Excel, and PDF export options.
#'
#' @param filename Filename prefix for exports (without extension)
#' @return List of button configurations for DT options
#' @export
build_dt_buttons <- function(filename) {
  list(
    list(extend = "copy"),
    list(extend = "print"),
    list(extend = "csv", filename = filename, title = NULL),
    list(extend = "excel", filename = filename, title = NULL),
    list(extend = "pdf", filename = filename, title = NULL)
  )
}

#' Write a data sheet to an Excel workbook
#'
#' Generic helper to write a data frame to an Excel sheet with auto-width columns
#' and optional professional styling.
#'
#' @param wb openxlsx workbook object
#' @param sheet_name Name for the worksheet
#' @param data Data frame to write
#' @param openxlsx The openxlsx module reference
#' @param col_names Whether to write column names (default TRUE)
#' @param tab_colour Tab colour hex string (default NULL)
#' @return Invisibly returns TRUE if written, FALSE otherwise
#' @export
write_data_sheet <- function(
  wb, sheet_name, data, openxlsx,
  col_names = TRUE, tab_colour = NULL
) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(invisible(FALSE))
  }

  openxlsx$addWorksheet(wb, sheet_name, tabColour = tab_colour)
  openxlsx$writeData(wb, sheet_name, data, colNames = col_names)
  openxlsx$setColWidths(
    wb,
    sheet_name,
    cols = seq_len(ncol(data)),
    widths = "auto"
  )

  if (col_names) {
    style_data_sheet(wb, sheet_name, data, openxlsx)
  }

  invisible(TRUE)
}

#' Write a comparison sheet to an Excel workbook
#'
#' Helper to reduce repetitive code when exporting comparison data.
#'
#' @param wb openxlsx workbook object
#' @param sheet_name Name for the worksheet
#' @param data Data frame to write
#' @param digits Number of decimal places for rounding
#' @param openxlsx The openxlsx module reference
#' @param dplyr The dplyr module reference
#' @param tab_colour Tab colour hex string (default NULL)
#' @return Invisibly returns TRUE if written, FALSE otherwise
#' @export
write_comparison_sheet <- function(
  wb,
  sheet_name,
  data,
  digits = 4,
  openxlsx,
  dplyr,
  tab_colour = NULL
) {
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(invisible(FALSE))
  }

  rounded_data <- dplyr$mutate(
    data,
    dplyr$across(where(is.numeric), ~ round(., digits))
  )

  openxlsx$addWorksheet(wb, sheet_name, tabColour = tab_colour)
  openxlsx$writeData(wb, sheet_name, rounded_data)
  openxlsx$setColWidths(
    wb,
    sheet_name,
    cols = seq_len(ncol(rounded_data)),
    widths = "auto"
  )

  style_data_sheet(wb, sheet_name, rounded_data, openxlsx)

  invisible(TRUE)
}

#' Write a styled summary sheet to an Excel workbook
#'
#' Creates a professional summary sheet with merged title, colored section
#' headers, and bold key-value labels. Replaces inline styling in the view.
#'
#' @param wb openxlsx workbook object
#' @param summary_data Data frame with Item and Value columns
#' @param openxlsx The openxlsx module reference
#' @export
write_summary_sheet <- function(wb, summary_data, openxlsx) {
  openxlsx$addWorksheet(
    wb, "Summary",
    tabColour = excel_palette$TAB_SUMMARY
  )

  # Find title row (row 1), section rows (start with "---"), and data rows

  title_row <- 1
  section_rows <- which(grepl("^---", summary_data$Item))

  # Strip "--- " markers from section headers for display
  clean_data <- summary_data
  clean_data$Item <- gsub("^---\\s*", "", clean_data$Item)
  clean_data$Item <- gsub("\\s*---$", "", clean_data$Item)

  # Write data without column headers
  openxlsx$writeData(wb, "Summary", clean_data, colNames = FALSE)

  # Style title row — merge across cols 1:2
  openxlsx$mergeCells(wb, "Summary", cols = 1:2, rows = title_row)
  title_style <- create_summary_title_style(openxlsx)
  openxlsx$addStyle(
    wb, "Summary", title_style,
    rows = title_row, cols = 1
  )

  # Style section headers — merge + colored bg
  section_style <- create_summary_section_style(openxlsx)
  for (row in section_rows) {
    openxlsx$mergeCells(wb, "Summary", cols = 1:2, rows = row)
    openxlsx$addStyle(
      wb, "Summary", section_style,
      rows = row, cols = 1:2, gridExpand = TRUE
    )
  }

  # Style key-value labels (bold in col 1 for non-title, non-section,
  # non-empty rows)
  label_style <- create_summary_label_style(openxlsx)
  data_rows <- setdiff(
    which(nzchar(clean_data$Item)),
    c(title_row, section_rows)
  )
  if (length(data_rows) > 0) {
    openxlsx$addStyle(
      wb, "Summary", label_style,
      rows = data_rows, cols = 1, stack = TRUE
    )
  }

  # Set column widths
  openxlsx$setColWidths(wb, "Summary", cols = 1:2, widths = c(30, 45))

  invisible(TRUE)
}

#' Write a styled model summary sheet to an Excel workbook
#'
#' Creates a sheet with monospace-formatted model console output.
#'
#' @param wb openxlsx workbook object
#' @param model_text Character vector of model summary lines
#' @param openxlsx The openxlsx module reference
#' @export
write_model_summary_sheet <- function(wb, model_text, openxlsx) {
  openxlsx$addWorksheet(
    wb, "Model_Summary",
    tabColour = excel_palette$TAB_MODEL
  )

  # Header in row 1
  header_df <- data.frame(Output = "Model Summary", stringsAsFactors = FALSE)
  openxlsx$writeData(wb, "Model_Summary", header_df, colNames = FALSE)
  header_style <- openxlsx$createStyle(
    fontName = "Calibri",
    fontSize = 14,
    textDecoration = "bold"
  )
  openxlsx$addStyle(wb, "Model_Summary", header_style, rows = 1, cols = 1)

  # Write console output starting at row 3
  text_df <- data.frame(Output = model_text, stringsAsFactors = FALSE)
  openxlsx$writeData(
    wb, "Model_Summary", text_df,
    startRow = 3, colNames = FALSE
  )

  # Apply monospace font to all text rows
  mono_style <- create_model_text_style(openxlsx)
  text_rows <- seq(3, 3 + length(model_text) - 1)
  openxlsx$addStyle(
    wb, "Model_Summary", mono_style,
    rows = text_rows, cols = 1, gridExpand = TRUE
  )

  openxlsx$setColWidths(wb, "Model_Summary", cols = 1, widths = 120)

  invisible(TRUE)
}
