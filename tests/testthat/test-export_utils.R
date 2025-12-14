# Tests for app/logic/mixed_effects/export_utils.R

box::use(
  testthat[...],
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
