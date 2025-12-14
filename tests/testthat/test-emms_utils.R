# Tests for app/logic/mixed_effects/emms_utils.R

box::use(
  testthat[...],
)

box::use(
  app / logic / mixed_effects / emms_utils
)

# ------------------------------------------------------------------------------
# extract_q0_columns() tests
# ------------------------------------------------------------------------------

describe("extract_q0_columns", {
  it("extracts Q0 columns from EMM data", {
    df <- data.frame(
      Treatment = c("A", "B"),
      Q0 = c(10, 20),
      Q0_SE = c(1, 2),
      alpha = c(0.001, 0.002)
    )
    result <- emms_utils$extract_q0_columns(df)
    expect_true("Q0" %in% names(result))
    expect_true("Q0_SE" %in% names(result))
    expect_false("alpha" %in% names(result))
  })

  it("returns NULL for empty data", {
    result <- emms_utils$extract_q0_columns(NULL)
    expect_null(result)

    result2 <- emms_utils$extract_q0_columns(data.frame())
    expect_null(result2)
  })

  it("returns NULL when no Q0 columns present", {
    df <- data.frame(
      Treatment = c("A", "B"),
      alpha = c(0.001, 0.002)
    )
    result <- emms_utils$extract_q0_columns(df)
    expect_null(result)
  })
})

# ------------------------------------------------------------------------------
# extract_alpha_columns() tests
# ------------------------------------------------------------------------------

describe("extract_alpha_columns", {
  it("extracts alpha columns from EMM data", {
    df <- data.frame(
      Treatment = c("A", "B"),
      Q0 = c(10, 20),
      alpha = c(0.001, 0.002),
      alpha_SE = c(0.0001, 0.0002)
    )
    result <- emms_utils$extract_alpha_columns(df)
    expect_true("alpha" %in% names(result))
    expect_true("alpha_SE" %in% names(result))
    expect_false("Q0" %in% names(result))
  })

  it("returns NULL for empty data", {
    result <- emms_utils$extract_alpha_columns(NULL)
    expect_null(result)
  })
})

# ------------------------------------------------------------------------------
# extract_ev_columns() tests
# ------------------------------------------------------------------------------

describe("extract_ev_columns", {
  it("extracts EV columns from EMM data", {
    df <- data.frame(
      Treatment = c("A", "B"),
      Q0 = c(10, 20),
      EV = c(100, 200),
      alpha = c(0.001, 0.002)
    )
    result <- emms_utils$extract_ev_columns(df)
    expect_true("EV" %in% names(result))
    expect_false("Q0" %in% names(result))
    expect_false("alpha" %in% names(result))
  })

  it("returns NULL when no EV columns present", {
    df <- data.frame(
      Treatment = c("A", "B"),
      Q0 = c(10, 20)
    )
    result <- emms_utils$extract_ev_columns(df)
    expect_null(result)
  })
})

# ------------------------------------------------------------------------------
# round_numeric_columns() tests
# ------------------------------------------------------------------------------

describe("round_numeric_columns", {
  it("rounds numeric columns to specified digits", {
    df <- data.frame(
      Treatment = c("A", "B"),
      value = c(1.123456, 2.987654)
    )
    result <- emms_utils$round_numeric_columns(df, digits = 2)
    expect_equal(result$value, c(1.12, 2.99))
  })

  it("preserves non-numeric columns", {
    df <- data.frame(
      Treatment = c("A", "B"),
      value = c(1.123456, 2.987654),
      stringsAsFactors = FALSE
    )
    result <- emms_utils$round_numeric_columns(df, digits = 2)
    expect_equal(result$Treatment, c("A", "B"))
  })

  it("handles NULL input", {
    result <- emms_utils$round_numeric_columns(NULL)
    expect_null(result)
  })

  it("handles empty data frame", {
    df <- data.frame()
    result <- emms_utils$round_numeric_columns(df)
    expect_equal(nrow(result), 0)
  })
})

# ------------------------------------------------------------------------------
# format_comparison_results() tests
# ------------------------------------------------------------------------------

describe("format_comparison_results", {
  it("extracts ratio format comparisons", {
    comparisons <- list(
      Q0 = list(
        contrasts_ratio = data.frame(
          contrast = c("A - B"),
          ratio = c(1.5),
          p.value = c(0.03)
        ),
        contrasts_log10 = data.frame(
          contrast = c("A - B"),
          estimate = c(0.176)
        )
      )
    )
    result <- emms_utils$format_comparison_results(comparisons, "Q0", "ratio")
    expect_true("ratio" %in% names(result))
    expect_false("estimate" %in% names(result))
  })

  it("extracts log10 format comparisons", {
    comparisons <- list(
      alpha = list(
        contrasts_ratio = data.frame(ratio = 1.5),
        contrasts_log10 = data.frame(estimate = 0.176)
      )
    )
    result <- emms_utils$format_comparison_results(
      comparisons,
      "alpha",
      "log10"
    )
    expect_true("estimate" %in% names(result))
  })

  it("returns NULL for missing parameter", {
    comparisons <- list(Q0 = list(contrasts_ratio = data.frame(x = 1)))
    result <- emms_utils$format_comparison_results(
      comparisons,
      "alpha",
      "ratio"
    )
    expect_null(result)
  })

  it("returns NULL for NULL input", {
    result <- emms_utils$format_comparison_results(NULL, "Q0", "ratio")
    expect_null(result)
  })
})

# ------------------------------------------------------------------------------
# has_emm_content() tests
# ------------------------------------------------------------------------------

describe("has_emm_content", {
  it("returns TRUE for valid data frame with rows", {
    df <- data.frame(x = 1:3)
    expect_true(emms_utils$has_emm_content(df))
  })

  it("returns FALSE for NULL", {
    expect_false(emms_utils$has_emm_content(NULL))
  })

  it("returns FALSE for empty data frame", {
    expect_false(emms_utils$has_emm_content(data.frame()))
  })

  it("returns FALSE for non-data frame", {
    expect_false(emms_utils$has_emm_content(list(x = 1)))
  })
})
