# Tests for app/logic/mixed_effects/export_utils.R

box::use(
  testthat[...],
  openxlsx,
)

box::use(
  app / logic / mixed_effects / export_utils
)

# ------------------------------------------------------------------------------
# build_summary_sheet() tests
# ------------------------------------------------------------------------------

describe("build_summary_sheet", {
  it("builds summary with all settings", {
    settings <- list(
      equation = "zben",
      factors = c("Treatment", "Session"),
      factor_interaction = TRUE,
      id_var = "monkey",
      x_var = "price",
      y_var = "consumption",
      random_effects = c("Q0", "alpha"),
      covariance_structure = "pdDiag"
    )
    result <- export_utils$build_summary_sheet(settings)
    expect_s3_class(result, "data.frame")
    expect_true("Item" %in% names(result))
    expect_true("Value" %in% names(result))
    expect_true(any(grepl("Equation", result$Item)))
  })

  it("handles NULL settings gracefully", {
    settings <- list(
      equation = NULL,
      factors = NULL,
      id_var = NULL
    )
    result <- export_utils$build_summary_sheet(settings)
    expect_s3_class(result, "data.frame")
    expect_true(any(result$Value == "N/A"))
  })

  it("formats factors as comma-separated string", {
    settings <- list(
      factors = c("A", "B", "C")
    )
    result <- export_utils$build_summary_sheet(settings)
    factor_row <- result[result$Item == "Factor(s)", ]
    expect_equal(factor_row$Value, "A, B, C")
  })

  it("shows None when factors is empty", {
    settings <- list(factors = character(0))
    result <- export_utils$build_summary_sheet(settings)
    factor_row <- result[result$Item == "Factor(s)", ]
    expect_equal(factor_row$Value, "None")
  })
})

# ------------------------------------------------------------------------------
# add_collapse_info() tests
# ------------------------------------------------------------------------------

describe("add_collapse_info", {
  it("adds Q0 collapse info", {
    summary_data <- data.frame(
      Item = "Test",
      Value = "1",
      stringsAsFactors = FALSE
    )
    collapse_info <- list(
      Q0 = list(Treatment = list(Low = c("A", "B")))
    )
    result <- export_utils$add_collapse_info(summary_data, collapse_info)
    expect_true(any(grepl("Q0 Collapse", result$Item)))
  })

  it("adds alpha collapse info", {
    summary_data <- data.frame(
      Item = "Test",
      Value = "1",
      stringsAsFactors = FALSE
    )
    collapse_info <- list(
      alpha = list(Treatment = list(High = c("C", "D")))
    )
    result <- export_utils$add_collapse_info(summary_data, collapse_info)
    expect_true(any(grepl("Alpha Collapse", result$Item)))
  })

  it("returns unchanged data for NULL collapse_info", {
    summary_data <- data.frame(
      Item = "Test",
      Value = "1",
      stringsAsFactors = FALSE
    )
    result <- export_utils$add_collapse_info(summary_data, NULL)
    expect_equal(nrow(result), 1)
  })

  it("returns unchanged data for ERROR_OVERLAP", {
    summary_data <- data.frame(
      Item = "Test",
      Value = "1",
      stringsAsFactors = FALSE
    )
    result <- export_utils$add_collapse_info(summary_data, "ERROR_OVERLAP")
    expect_equal(nrow(result), 1)
  })
})

# ------------------------------------------------------------------------------
# add_fitting_settings() tests
# ------------------------------------------------------------------------------

describe("add_fitting_settings", {
  it("adds fitting settings rows", {
    summary_data <- data.frame(
      Item = "Test",
      Value = "1",
      stringsAsFactors = FALSE
    )
    nlme_ctrl <- list(
      maxIter = 100,
      pnlsMaxIter = 10,
      msMaxIter = 100,
      tolerance = 1e-5,
      pnlsTol = 1e-4,
      minScale = 1e-4,
      niterEM = 50
    )
    result <- export_utils$add_fitting_settings(summary_data, nlme_ctrl)
    expect_true(any(grepl("maxIter", result$Item)))
    expect_true(any(grepl("tolerance", result$Item)))
  })

  it("returns unchanged data for NULL nlme_ctrl", {
    summary_data <- data.frame(
      Item = "Test",
      Value = "1",
      stringsAsFactors = FALSE
    )
    result <- export_utils$add_fitting_settings(summary_data, NULL)
    expect_equal(nrow(result), 1)
  })
})

# ------------------------------------------------------------------------------
# build_descriptives() tests
# ------------------------------------------------------------------------------

describe("build_descriptives", {
  it("calculates descriptives without grouping", {
    df <- data.frame(
      id = c("a", "b", "c"),
      y_for_model = c(10, 20, 30)
    )
    result <- export_utils$build_descriptives(df)
    expect_equal(result$N, 3)
    expect_equal(result$Mean_Y_Model, 20)
  })

  it("calculates descriptives with grouping", {
    df <- data.frame(
      Treatment = c("A", "A", "B", "B"),
      y_for_model = c(10, 20, 30, 40)
    )
    result <- export_utils$build_descriptives(df, grouping_vars = "Treatment")
    expect_equal(nrow(result), 2)
    expect_true("Treatment" %in% names(result))
  })

  it("returns NULL when y_for_model missing", {
    df <- data.frame(id = 1:3, y = c(10, 20, 30))
    result <- export_utils$build_descriptives(df)
    expect_null(result)
  })

  it("returns NULL for NULL input", {
    result <- export_utils$build_descriptives(NULL)
    expect_null(result)
  })
})

# ------------------------------------------------------------------------------
# generate_export_timestamp() tests
# ------------------------------------------------------------------------------

describe("generate_export_timestamp", {
  it("returns a timestamp string", {
    result <- export_utils$generate_export_timestamp()
    expect_type(result, "character")
    expect_match(result, "^\\d{8}_\\d{6}$")
  })
})

# ------------------------------------------------------------------------------
# build_export_filename() tests
# ------------------------------------------------------------------------------

describe("build_export_filename", {
  it("builds filename with default values", {
    result <- export_utils$build_export_filename()
    expect_match(result, "^shinybeez_export_\\d{8}_\\d{6}\\.xlsx$")
  })

  it("uses custom prefix and extension", {
    result <- export_utils$build_export_filename(
      prefix = "my_export",
      extension = "csv"
    )
    expect_match(result, "^my_export_\\d{8}_\\d{6}\\.csv$")
  })
})

# ------------------------------------------------------------------------------
# build_dt_buttons() tests
# ------------------------------------------------------------------------------

describe("build_dt_buttons", {
  it("returns list with 5 button configurations", {
    result <- export_utils$build_dt_buttons("test_file")
    expect_type(result, "list")
    expect_length(result, 5)
  })

  it("includes copy and print buttons", {
    result <- export_utils$build_dt_buttons("test_file")
    extends <- sapply(result, function(x) x$extend)
    expect_true("copy" %in% extends)
    expect_true("print" %in% extends)
  })

  it("includes csv, excel, pdf with correct filename", {
    result <- export_utils$build_dt_buttons("my_export")
    csv_btn <- result[[which(sapply(result, function(x) x$extend == "csv"))]]
    excel_btn <- result[[which(sapply(result, function(x) {
      x$extend == "excel"
    }))]]
    pdf_btn <- result[[which(sapply(result, function(x) x$extend == "pdf"))]]

    expect_equal(csv_btn$filename, "my_export")
    expect_equal(excel_btn$filename, "my_export")
    expect_equal(pdf_btn$filename, "my_export")
  })

  it("sets title to NULL for file exports", {
    result <- export_utils$build_dt_buttons("test")
    csv_btn <- result[[which(sapply(result, function(x) x$extend == "csv"))]]
    expect_null(csv_btn$title)
  })
})

# ------------------------------------------------------------------------------
# excel_palette tests
# ------------------------------------------------------------------------------

describe("excel_palette", {
  it("contains all required colour keys", {
    expected_keys <- c(
      "HEADER_BG", "HEADER_FONT", "SECTION_BG", "SECTION_FONT",
      "TITLE_FONT", "STRIPE_BG", "BORDER_COLOR",
      "TAB_SUMMARY", "TAB_DATA", "TAB_MODEL",
      "TAB_COMPARISON", "TAB_PLOT"
    )
    for (key in expected_keys) {
      expect_true(
        key %in% names(export_utils$excel_palette),
        info = paste("Missing key:", key)
      )
    }
  })

  it("has valid hex colour values", {
    for (key in names(export_utils$excel_palette)) {
      expect_match(
        export_utils$excel_palette[[key]],
        "^#[0-9A-Fa-f]{6}$",
        info = paste("Invalid hex for:", key)
      )
    }
  })
})

# ------------------------------------------------------------------------------
# Style factory tests
# ------------------------------------------------------------------------------

describe("create_header_style", {
  it("returns a non-NULL style object", {
    style <- export_utils$create_header_style(openxlsx)
    expect_false(is.null(style))
  })
})

describe("create_stripe_style", {
  it("returns a non-NULL style object", {
    style <- export_utils$create_stripe_style(openxlsx)
    expect_false(is.null(style))
  })
})

describe("create_body_border_style", {
  it("returns a non-NULL style object", {
    style <- export_utils$create_body_border_style(openxlsx)
    expect_false(is.null(style))
  })
})

describe("create_numfmt_style", {
  it("returns a non-NULL style object", {
    style <- export_utils$create_numfmt_style(openxlsx, "0.0000")
    expect_false(is.null(style))
  })
})

describe("create_summary_title_style", {
  it("returns a non-NULL style object", {
    style <- export_utils$create_summary_title_style(openxlsx)
    expect_false(is.null(style))
  })
})

describe("create_model_text_style", {
  it("returns a non-NULL style object", {
    style <- export_utils$create_model_text_style(openxlsx)
    expect_false(is.null(style))
  })
})

# ------------------------------------------------------------------------------
# detect_number_format() tests
# ------------------------------------------------------------------------------

describe("detect_number_format", {
  it("detects p-value columns", {
    expect_equal(export_utils$detect_number_format("p.value"), "0.0000")
    expect_equal(export_utils$detect_number_format("p_value"), "0.0000")
    expect_equal(export_utils$detect_number_format("Pr"), "0.0000")
    expect_equal(export_utils$detect_number_format("pvalue"), "0.0000")
  })

  it("detects standard error columns", {
    expect_equal(export_utils$detect_number_format("SE"), "0.0000")
    expect_equal(export_utils$detect_number_format("Std.Error"), "0.0000")
    expect_equal(export_utils$detect_number_format("std.error"), "0.0000")
  })

  it("detects estimate columns", {
    expect_equal(export_utils$detect_number_format("estimate"), "0.0000")
    expect_equal(export_utils$detect_number_format("ratio"), "0.0000")
    expect_equal(export_utils$detect_number_format("coefficient"), "0.0000")
  })

  it("detects t/z/df columns", {
    expect_equal(export_utils$detect_number_format("t.value"), "0.00")
    expect_equal(export_utils$detect_number_format("t_value"), "0.00")
    expect_equal(export_utils$detect_number_format("z.value"), "0.00")
    expect_equal(export_utils$detect_number_format("df"), "0.00")
  })

  it("detects integer count columns", {
    expect_equal(export_utils$detect_number_format("N"), "0")
    expect_equal(export_utils$detect_number_format("count"), "0")
    expect_equal(export_utils$detect_number_format("n_positive"), "0")
  })

  it("defaults to 3 decimal places", {
    expect_equal(export_utils$detect_number_format("some_metric"), "0.000")
    expect_equal(export_utils$detect_number_format("alpha"), "0.000")
  })
})

# ------------------------------------------------------------------------------
# style_data_sheet() tests
# ------------------------------------------------------------------------------

describe("style_data_sheet", {
  it("styles a data sheet without error", {
    wb <- openxlsx$createWorkbook()
    test_data <- data.frame(
      id = c("a", "b", "c"),
      p.value = c(0.01, 0.05, 0.99),
      N = c(10L, 20L, 30L),
      metric = c(1.234, 5.678, 9.012)
    )
    openxlsx$addWorksheet(wb, "Test")
    openxlsx$writeData(wb, "Test", test_data)
    expect_no_error(
      export_utils$style_data_sheet(wb, "Test", test_data, openxlsx)
    )
  })

  it("saves styled workbook without error", {
    wb <- openxlsx$createWorkbook()
    test_data <- data.frame(x = 1:5, y = rnorm(5))
    openxlsx$addWorksheet(wb, "Test")
    openxlsx$writeData(wb, "Test", test_data)
    export_utils$style_data_sheet(wb, "Test", test_data, openxlsx)
    tmp <- tempfile(fileext = ".xlsx")
    expect_no_error(openxlsx$saveWorkbook(wb, tmp, overwrite = TRUE))
    expect_true(file.exists(tmp))
    unlink(tmp)
  })
})

# ------------------------------------------------------------------------------
# write_data_sheet() with tab_colour tests
# ------------------------------------------------------------------------------

describe("write_data_sheet with tab_colour", {
  it("creates a styled sheet with tab colour", {
    wb <- openxlsx$createWorkbook()
    test_data <- data.frame(a = 1:3, b = 4:6)
    result <- export_utils$write_data_sheet(
      wb, "Colored", test_data, openxlsx,
      tab_colour = "#27AE60"
    )
    expect_true(result)
    expect_true("Colored" %in% names(wb))
  })

  it("returns FALSE for NULL data", {
    wb <- openxlsx$createWorkbook()
    result <- export_utils$write_data_sheet(
      wb, "Empty", NULL, openxlsx, tab_colour = "#27AE60"
    )
    expect_false(result)
  })
})

# ------------------------------------------------------------------------------
# write_summary_sheet() tests
# ------------------------------------------------------------------------------

describe("write_summary_sheet", {
  it("creates a Summary sheet without error", {
    wb <- openxlsx$createWorkbook()
    summary_data <- export_utils$build_summary_sheet(
      list(equation = "hs", factors = "Treatment"),
      version = "1.0.0"
    )
    expect_no_error(
      export_utils$write_summary_sheet(wb, summary_data, openxlsx)
    )
    expect_true("Summary" %in% names(wb))
  })

  it("saves summary workbook to file without error", {
    wb <- openxlsx$createWorkbook()
    summary_data <- export_utils$build_summary_sheet(
      list(equation = "hs"),
      version = "1.0.0"
    )
    export_utils$write_summary_sheet(wb, summary_data, openxlsx)
    tmp <- tempfile(fileext = ".xlsx")
    expect_no_error(openxlsx$saveWorkbook(wb, tmp, overwrite = TRUE))
    expect_true(file.exists(tmp))
    unlink(tmp)
  })
})

# ------------------------------------------------------------------------------
# write_model_summary_sheet() tests
# ------------------------------------------------------------------------------

describe("write_model_summary_sheet", {
  it("creates a Model_Summary sheet without error", {
    wb <- openxlsx$createWorkbook()
    model_text <- c(
      "Linear mixed-effects model",
      "  Fixed: y ~ x",
      "  AIC: 123.45"
    )
    expect_no_error(
      export_utils$write_model_summary_sheet(wb, model_text, openxlsx)
    )
    expect_true("Model_Summary" %in% names(wb))
  })

  it("saves model summary workbook to file without error", {
    wb <- openxlsx$createWorkbook()
    export_utils$write_model_summary_sheet(
      wb, c("test line 1", "test line 2"), openxlsx
    )
    tmp <- tempfile(fileext = ".xlsx")
    expect_no_error(openxlsx$saveWorkbook(wb, tmp, overwrite = TRUE))
    expect_true(file.exists(tmp))
    unlink(tmp)
  })
})
