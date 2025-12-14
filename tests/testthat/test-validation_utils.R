# test-validation_utils.R
# Tests for validation_utils module

box::use(
  app / logic / mixed_effects / validation_utils
)

# ------------------------------------------------------------------------------
# is_valid_dataframe() tests
# ------------------------------------------------------------------------------

describe("is_valid_dataframe", {
  it("returns TRUE for valid non-empty data frame", {
    df <- data.frame(a = 1:3, b = c("x", "y", "z"))
    expect_true(validation_utils$is_valid_dataframe(df))
  })

  it("returns FALSE for NULL", {
    expect_false(validation_utils$is_valid_dataframe(NULL))
  })

  it("returns FALSE for empty data frame", {
    df <- data.frame(a = integer(0), b = character(0))
    expect_false(validation_utils$is_valid_dataframe(df))
  })

  it("returns FALSE for non-data.frame objects", {
    expect_false(validation_utils$is_valid_dataframe(list(a = 1)))
    expect_false(validation_utils$is_valid_dataframe("string"))
    expect_false(validation_utils$is_valid_dataframe(123))
    expect_false(validation_utils$is_valid_dataframe(c(1, 2, 3)))
    expect_false(validation_utils$is_valid_dataframe(matrix(1:4, 2, 2)))
  })

  it("returns TRUE for tibble with data", {
    tbl <- tibble::tibble(a = 1:3, b = c("x", "y", "z"))
    expect_true(validation_utils$is_valid_dataframe(tbl))
  })

  it("returns FALSE for empty tibble", {
    tbl <- tibble::tibble()
    expect_false(validation_utils$is_valid_dataframe(tbl))
  })
})

# ------------------------------------------------------------------------------
# is_valid_comparison_data() tests
# ------------------------------------------------------------------------------

describe("is_valid_comparison_data", {
  it("returns TRUE for valid comparison data", {
    df <- data.frame(contrast = "a - b", estimate = 0.5, p.value = 0.03)
    expect_true(validation_utils$is_valid_comparison_data(df))
  })

  it("returns FALSE for NULL", {
    expect_false(validation_utils$is_valid_comparison_data(NULL))
  })

  it("returns FALSE for empty data frame", {
    df <- data.frame(contrast = character(0))
    expect_false(validation_utils$is_valid_comparison_data(df))
  })
})

# ------------------------------------------------------------------------------
# is_valid_emms_data() tests
# ------------------------------------------------------------------------------

describe("is_valid_emms_data", {
  it("returns TRUE for valid EMMs data", {
    df <- data.frame(factor = "A", emmean = 1.5, SE = 0.2)
    expect_true(validation_utils$is_valid_emms_data(df))
  })

  it("returns FALSE for NULL", {
    expect_false(validation_utils$is_valid_emms_data(NULL))
  })

  it("returns FALSE for empty data frame", {
    df <- data.frame(factor = character(0))
    expect_false(validation_utils$is_valid_emms_data(df))
  })
})

# ------------------------------------------------------------------------------
# is_valid_model_fit() tests
# ------------------------------------------------------------------------------

describe("is_valid_model_fit", {
  it("returns TRUE for valid model fit object", {
    model_fit <- list(model = "mock_model", data = data.frame(x = 1))
    expect_true(validation_utils$is_valid_model_fit(model_fit))
  })

  it("returns FALSE for NULL", {
    expect_false(validation_utils$is_valid_model_fit(NULL))
  })

  it("returns FALSE for object without model element", {
    model_fit <- list(data = data.frame(x = 1))
    expect_false(validation_utils$is_valid_model_fit(model_fit))
  })

  it("returns FALSE for object with NULL model", {
    model_fit <- list(model = NULL, data = data.frame(x = 1))
    expect_false(validation_utils$is_valid_model_fit(model_fit))
  })
})

# ------------------------------------------------------------------------------
# is_ready_for_analysis() tests
# ------------------------------------------------------------------------------

describe("is_ready_for_analysis", {
  it("returns TRUE for valid data with no required columns", {
    df <- data.frame(a = 1:3, b = c("x", "y", "z"))
    expect_true(validation_utils$is_ready_for_analysis(df))
  })

  it("returns TRUE when all required columns present", {
    df <- data.frame(id = 1:3, x = c(1, 2, 3), y = c(10, 20, 30))
    expect_true(validation_utils$is_ready_for_analysis(df, c("id", "x", "y")))
  })

  it("returns FALSE when required columns missing", {
    df <- data.frame(id = 1:3, x = c(1, 2, 3))
    expect_false(validation_utils$is_ready_for_analysis(df, c("id", "x", "y")))
  })

  it("returns FALSE for NULL data", {
    expect_false(validation_utils$is_ready_for_analysis(NULL, c("id")))
  })

  it("returns FALSE for empty data frame", {
    df <- data.frame(id = integer(0))
    expect_false(validation_utils$is_ready_for_analysis(df, c("id")))
  })
})

# ------------------------------------------------------------------------------
# safe_nrow() tests
# ------------------------------------------------------------------------------

describe("safe_nrow", {
  it("returns correct row count for valid data frame", {
    df <- data.frame(a = 1:5)
    expect_equal(validation_utils$safe_nrow(df), 5)
  })

  it("returns 0 for NULL", {
    expect_equal(validation_utils$safe_nrow(NULL), 0L)
  })

  it("returns 0 for non-data.frame", {
    expect_equal(validation_utils$safe_nrow(list(a = 1:5)), 0L)
    expect_equal(validation_utils$safe_nrow("string"), 0L)
    expect_equal(validation_utils$safe_nrow(123), 0L)
  })

  it("returns 0 for empty data frame", {
    df <- data.frame(a = integer(0))
    expect_equal(validation_utils$safe_nrow(df), 0)
  })
})
