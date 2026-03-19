# Tests for app/logic/discounting/scoring.R

box::use(
  testthat[...],
)

box::use(
  app / logic / discounting / scoring
)

# --- Test data helpers --------------------------------------------------------

make_mock_results <- function() {
  data.frame(
    subjectid = c("s1", "s2"),
    small_k = c(0.123456789, 0.987654321),
    medium_k = c(0.111111111, 0.222222222),
    large_k = c(0.333333333, 0.444444444),
    overall_k = c(0.555555555, 0.666666666),
    geomean_k = c(0.777777777, 0.888888888),
    small_proportion = c(0.12345, 0.67891),
    medium_proportion = c(0.56789, 0.23456),
    large_proportion = c(0.98765, 0.43219),
    overall_proportion = c(0.11111, 0.99999),
    overall_consistency = c(0.98765, 0.43219),
    small_consistency = c(0.12345, 0.67891),
    medium_consistency = c(0.56789, 0.23456),
    large_consistency = c(0.33333, 0.77777),
    composite_consistency = c(0.44444, 0.55555),
    other_col = c(1.123456, 2.654321),
    impute_method = c("none", "none"),
    stringsAsFactors = FALSE
  )
}

make_mcq_data <- function(n_ids = 5, seed = 42) {
  beezdiscounting::generate_data_mcq(n_ids = n_ids, seed = seed)
}

# ------------------------------------------------------------------------------
# resolve_imputation() tests
# ------------------------------------------------------------------------------

describe("resolve_imputation", {

  it("returns 'none' impute_method and FALSE random for 'none'", {
    result <- scoring$resolve_imputation("none")
    expect_equal(result$impute_method, "none")
    expect_false(result$random)
  })

  it("returns 'none' impute_method and FALSE random for NULL input", {
    result <- scoring$resolve_imputation(NULL)
    expect_equal(result$impute_method, "none")
    expect_false(result$random)
  })

  it("returns the method name and FALSE random for 'mean'", {
    result <- scoring$resolve_imputation("mean")
    expect_equal(result$impute_method, "mean")
    expect_false(result$random)
  })

  it("returns 'median' and FALSE random for 'median'", {
    result <- scoring$resolve_imputation("median")
    expect_equal(result$impute_method, "median")
    expect_false(result$random)
  })

  it("returns 'random' and TRUE random for 'random'", {
    result <- scoring$resolve_imputation("random")
    expect_equal(result$impute_method, "random")
    expect_true(result$random)
  })

  it("detects 'random' case-insensitively for 'Random'", {
    result <- scoring$resolve_imputation("Random")
    expect_equal(result$impute_method, "Random")
    expect_true(result$random)
  })

  it("detects 'random' case-insensitively for 'RANDOM'", {
    result <- scoring$resolve_imputation("RANDOM")
    expect_equal(result$impute_method, "RANDOM")
    expect_true(result$random)
  })

  it("passes through non-random methods unchanged", {
    result <- scoring$resolve_imputation("ggm")
    expect_equal(result$impute_method, "ggm")
    expect_false(result$random)
  })

  it("always returns a list with exactly two named elements", {
    result <- scoring$resolve_imputation("none")
    expect_type(result, "list")
    expect_equal(length(result), 2)
    expect_true(all(c("impute_method", "random") %in% names(result)))
  })

  it("strips _random suffix and returns TRUE random for 'INN_random'", {
    result <- scoring$resolve_imputation("INN_random")
    expect_equal(result$impute_method, "INN")
    expect_true(result$random)
  })

  it("strips _random suffix and returns TRUE random for 'GGM_random'", {
    result <- scoring$resolve_imputation("GGM_random")
    expect_equal(result$impute_method, "GGM")
    expect_true(result$random)
  })

  it("strips _random suffix case-insensitively for 'inn_Random'", {
    result <- scoring$resolve_imputation("inn_Random")
    expect_equal(result$impute_method, "inn")
    expect_true(result$random)
  })
})

# ------------------------------------------------------------------------------
# friendly_discounting_error() tests
# ------------------------------------------------------------------------------

describe("friendly_discounting_error", {

  it("translates 'In index: 1' pattern to data format error", {
    msg <- scoring$friendly_discounting_error("Error in mutate: In index: 1. something")
    expect_match(msg, "Data format error")
  })

  it("translates impute method error", {
    msg <- scoring$friendly_discounting_error("Impute method must be one of none, ggm")
    expect_match(msg, "Invalid imputation method")
  })

  it("translates argument length zero error", {
    msg <- scoring$friendly_discounting_error("argument is of length zero")
    expect_match(msg, "required column appears to be missing")
  })

  it("translates undefined columns error", {
    msg <- scoring$friendly_discounting_error("undefined columns selected")
    expect_match(msg, "missing expected columns")
  })

  it("translates response length error", {
    msg <- scoring$friendly_discounting_error("Response length not equal to 27")
    expect_match(msg, "exactly 27 items")
  })

  it("returns generic message for unrecognized errors", {
    msg <- scoring$friendly_discounting_error("something completely unknown")
    expect_match(msg, "An error occurred during scoring")
    expect_match(msg, "something completely unknown")
  })
})

# ------------------------------------------------------------------------------
# format_mcq_results() tests
# ------------------------------------------------------------------------------

describe("format_mcq_results", {

  it("rounds _k columns to 6 decimal places", {
    mock <- make_mock_results()
    result <- scoring$format_mcq_results(mock)
    k_cols <- c("small_k", "medium_k", "large_k", "overall_k", "geomean_k")
    for (col in k_cols) {
      expect_equal(
        result[[col]],
        round(mock[[col]], 6),
        info = paste(col, "should be rounded to 6 decimal places")
      )
    }
  })

  it("rounds _prop columns to 3 decimal places", {
    mock <- make_mock_results()
    result <- scoring$format_mcq_results(mock)
    prop_cols <- c(
      "small_proportion", "medium_proportion",
      "large_proportion", "overall_proportion"
    )
    for (col in prop_cols) {
      expect_equal(
        result[[col]],
        round(mock[[col]], 3),
        info = paste(col, "should be rounded to 3 decimal places")
      )
    }
  })

  it("rounds _cons columns to 3 decimal places", {
    mock <- make_mock_results()
    result <- scoring$format_mcq_results(mock)
    cons_cols <- c(
      "overall_consistency", "small_consistency",
      "medium_consistency", "large_consistency",
      "composite_consistency"
    )
    for (col in cons_cols) {
      expect_equal(
        result[[col]],
        round(mock[[col]], 3),
        info = paste(col, "should be rounded to 3 decimal places")
      )
    }
  })

  it("does not modify columns that lack _k, _prop, or _cons in the name", {
    mock <- make_mock_results()
    result <- scoring$format_mcq_results(mock)
    expect_equal(result$other_col, mock$other_col)
    expect_equal(result$subjectid, mock$subjectid)
    expect_equal(result$impute_method, mock$impute_method)
  })

  it("preserves all column names", {
    mock <- make_mock_results()
    result <- scoring$format_mcq_results(mock)
    expect_equal(names(result), names(mock))
  })

  it("preserves the number of rows", {
    mock <- make_mock_results()
    result <- scoring$format_mcq_results(mock)
    expect_equal(nrow(result), nrow(mock))
  })

  it("returns a data.frame", {
    mock <- make_mock_results()
    result <- scoring$format_mcq_results(mock)
    expect_s3_class(result, "data.frame")
  })
})

# ------------------------------------------------------------------------------
# score_and_format_mcq() tests
# ------------------------------------------------------------------------------

describe("score_and_format_mcq", {

  it("returns a list with results, data, and summary components", {
    mcq_data <- make_mcq_data(n_ids = 3)
    result <- scoring$score_and_format_mcq(mcq_data)
    expect_type(result, "list")
    expect_true("results" %in% names(result))
    expect_true("data" %in% names(result))
    expect_true("summary" %in% names(result))
  })

  it("results component is a data.frame with one row per participant", {
    mcq_data <- make_mcq_data(n_ids = 5)
    result <- scoring$score_and_format_mcq(mcq_data)
    expect_s3_class(result$results, "data.frame")
    expect_equal(nrow(result$results), 5)
  })

  it("data component is a data.frame", {
    mcq_data <- make_mcq_data(n_ids = 3)
    result <- scoring$score_and_format_mcq(mcq_data)
    expect_s3_class(result$data, "data.frame")
  })

  it("summary component is a data.frame with expected columns", {
    mcq_data <- make_mcq_data(n_ids = 3)
    result <- scoring$score_and_format_mcq(mcq_data)
    expect_s3_class(result$summary, "data.frame")
    expect_true(all(c("Metric", "Mean", "SD", "SEM") %in% names(result$summary)))
  })

  it("results contain expected MCQ columns", {
    mcq_data <- make_mcq_data(n_ids = 3)
    result <- scoring$score_and_format_mcq(mcq_data)
    expected_cols <- c(
      "subjectid", "overall_k", "small_k", "medium_k", "large_k",
      "geomean_k", "overall_consistency", "small_consistency",
      "medium_consistency", "large_consistency"
    )
    for (col in expected_cols) {
      expect_true(
        col %in% names(result$results),
        info = paste(col, "should be in results")
      )
    }
  })

  it("results have _k columns rounded to 6 decimal places", {
    mcq_data <- make_mcq_data(n_ids = 5)
    result <- scoring$score_and_format_mcq(mcq_data)
    k_cols <- grep("_k$", names(result$results), value = TRUE)
    for (col in k_cols) {
      values <- result$results[[col]]
      expect_equal(
        values,
        round(values, 6),
        info = paste(col, "should be rounded to 6 decimal places")
      )
    }
  })

  it("results have _consistency columns rounded to 3 decimal places", {
    mcq_data <- make_mcq_data(n_ids = 5)
    result <- scoring$score_and_format_mcq(mcq_data)
    cons_cols <- grep("_cons", names(result$results), value = TRUE)
    for (col in cons_cols) {
      values <- result$results[[col]]
      expect_equal(
        values,
        round(values, 3),
        info = paste(col, "should be rounded to 3 decimal places")
      )
    }
  })

  it("results have _proportion columns rounded to 3 decimal places", {
    mcq_data <- make_mcq_data(n_ids = 5)
    result <- scoring$score_and_format_mcq(mcq_data)
    prop_cols <- grep("_prop", names(result$results), value = TRUE)
    for (col in prop_cols) {
      values <- result$results[[col]]
      expect_equal(
        values,
        round(values, 3),
        info = paste(col, "should be rounded to 3 decimal places")
      )
    }
  })

  it("defaults imputation to 'none' when not specified", {
    mcq_data <- make_mcq_data(n_ids = 3)
    result <- scoring$score_and_format_mcq(mcq_data)
    expect_true(all(result$results$impute_method == "none"))
  })

  it("defaults trans to 'none' when not specified", {
    mcq_data <- make_mcq_data(n_ids = 3)
    result_default <- scoring$score_and_format_mcq(mcq_data)
    result_explicit <- scoring$score_and_format_mcq(mcq_data, trans = "none")
    expect_equal(result_default$results, result_explicit$results)
  })

  it("accepts an explicit 'none' imputation method", {
    mcq_data <- make_mcq_data(n_ids = 3, seed = 99)
    result <- scoring$score_and_format_mcq(mcq_data, imputation = "none")
    expect_type(result, "list")
    expect_s3_class(result$results, "data.frame")
  })

  it("returns consistent results for the same input data", {
    mcq_data <- make_mcq_data(n_ids = 3, seed = 123)
    result1 <- scoring$score_and_format_mcq(mcq_data)
    result2 <- scoring$score_and_format_mcq(mcq_data)
    expect_equal(result1$results, result2$results)
  })
})
