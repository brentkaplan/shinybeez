# Tests for app/logic/discounting/regression.R

box::use(
  testthat[...],
)

box::use(
  app / logic / discounting / regression
)

# --- Test data helpers --------------------------------------------------------

make_dd_data <- function() {
  delays <- c(1, 7, 30, 90, 180, 365)
  data.frame(
    id = rep(c("p1", "p2", "p3"), each = 6),
    x = rep(delays, 3),
    y = c(
      95, 85, 60, 35, 20, 10,
      90, 78, 55, 30, 18, 8,
      92, 80, 58, 32, 19, 9
    )
  )
}

make_mock_results <- function() {
  data.frame(
    id = c("b", "a", "c"),
    method = c("two stage", "two stage", "two stage"),
    term = c("k", "k", "k"),
    estimate = c(0.12345678, 0.98765432, 0.55555555),
    std.error = c(0.00123456, 0.00987654, 0.00555555),
    p.value = c(0.00001234, 0.04999999, 0.12345678),
    R2 = c(0.87654321, 0.99999999, 0.12345678),
    auc_regular = c(12.3456789, 45.6789012, 78.9012345),
    note = c("converged", "converged", "failed"),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------------
# format_regression_results() tests
# ------------------------------------------------------------------------------

describe("format_regression_results", {

  it("rounds numeric columns to 4 decimal places", {
    mock <- make_mock_results()
    result <- regression$format_regression_results(mock, id_levels = c("b", "a", "c"))

    expect_equal(result$estimate, c(0.1235, 0.9877, 0.5556))
    expect_equal(result$std.error, c(0.0012, 0.0099, 0.0056))
    expect_equal(result$p.value, c(0, 0.05, 0.1235))
    expect_equal(result$R2, c(0.8765, 1, 0.1235))
    expect_equal(result$auc_regular, c(12.3457, 45.6789, 78.9012))
  })

  it("converts id to a factor with specified levels", {
    mock <- make_mock_results()
    result <- regression$format_regression_results(mock, id_levels = c("b", "a", "c"))

    expect_s3_class(result$id, "factor")
    expect_equal(levels(result$id), c("b", "a", "c"))
  })

  it("sorts results by the id factor order", {
    mock <- make_mock_results()
    # Specify levels in a different order than the data
    result <- regression$format_regression_results(mock, id_levels = c("a", "b", "c"))

    expect_equal(as.character(result$id), c("a", "b", "c"))
    # Verify the associated data moved with the id
    expect_equal(result$note, c("converged", "converged", "failed"))
  })

  it("preserves non-id non-numeric columns unchanged", {
    mock <- make_mock_results()
    result <- regression$format_regression_results(mock, id_levels = c("b", "a", "c"))

    expect_equal(result$method, c("two stage", "two stage", "two stage"))
    expect_equal(result$term, c("k", "k", "k"))
    expect_equal(result$note, c("converged", "converged", "failed"))
  })

  it("preserves the number of rows", {
    mock <- make_mock_results()
    result <- regression$format_regression_results(mock, id_levels = c("b", "a", "c"))

    expect_equal(nrow(result), 3)
  })

  it("preserves all columns", {
    mock <- make_mock_results()
    result <- regression$format_regression_results(mock, id_levels = c("b", "a", "c"))

    expect_equal(sort(names(result)), sort(names(mock)))
  })

  it("handles a single-row data frame", {
    single <- data.frame(
      id = "only_one",
      estimate = 0.123456789,
      note = "ok",
      stringsAsFactors = FALSE
    )
    result <- regression$format_regression_results(single, id_levels = "only_one")

    expect_equal(nrow(result), 1)
    expect_s3_class(result$id, "factor")
    expect_equal(result$estimate, 0.1235)
  })

  it("does not alter logical columns", {
    mock_with_logical <- data.frame(
      id = c("x", "y"),
      value = c(1.23456, 7.89012),
      converged = c(TRUE, FALSE),
      stringsAsFactors = FALSE
    )
    result <- regression$format_regression_results(
      mock_with_logical, id_levels = c("x", "y")
    )

    expect_type(result$converged, "logical")
    expect_equal(result$converged, c(TRUE, FALSE))
  })

  it("respects custom id ordering that differs from alphabetical", {
    mock <- make_mock_results()
    result <- regression$format_regression_results(mock, id_levels = c("c", "b", "a"))

    expect_equal(as.character(result$id), c("c", "b", "a"))
    expect_equal(levels(result$id), c("c", "b", "a"))
  })
})

# ------------------------------------------------------------------------------
# fit_and_format_regression() tests
# ------------------------------------------------------------------------------

describe("fit_and_format_regression", {

  it("returns a list with dd_fit and results components", {
    dat <- make_dd_data()
    result <- suppressWarnings(
      regression$fit_and_format_regression(dat, equation = "hyperbolic", method = "two stage")
    )

    expect_type(result, "list")
    expect_true("dd_fit" %in% names(result))
    expect_true("results" %in% names(result))
  })

  it("returns a data frame in the results component", {
    dat <- make_dd_data()
    result <- suppressWarnings(
      regression$fit_and_format_regression(dat, equation = "hyperbolic", method = "two stage")
    )

    expect_s3_class(result$results, "data.frame")
  })

  it("results has id column as a factor", {
    dat <- make_dd_data()
    result <- suppressWarnings(
      regression$fit_and_format_regression(dat, equation = "hyperbolic", method = "two stage")
    )

    expect_s3_class(result$results$id, "factor")
  })

  it("id factor levels match the original data order", {
    dat <- make_dd_data()
    result <- suppressWarnings(
      regression$fit_and_format_regression(dat, equation = "hyperbolic", method = "two stage")
    )

    expect_equal(levels(result$results$id), c("p1", "p2", "p3"))
  })

  it("results numeric columns are rounded to 4 decimal places", {
    dat <- make_dd_data()
    result <- suppressWarnings(
      regression$fit_and_format_regression(dat, equation = "hyperbolic", method = "two stage")
    )

    numeric_cols <- result$results[, sapply(result$results, is.numeric)]
    all_rounded <- all(sapply(numeric_cols, function(col) {
      all(col == round(col, 4), na.rm = TRUE)
    }))
    expect_true(all_rounded)
  })

  it("results has one row per participant with two stage method", {
    dat <- make_dd_data()
    result <- suppressWarnings(
      regression$fit_and_format_regression(dat, equation = "hyperbolic", method = "two stage")
    )

    expect_equal(nrow(result$results), 3)
  })

  it("dd_fit is a fit_dd class object", {
    dat <- make_dd_data()
    result <- suppressWarnings(
      regression$fit_and_format_regression(dat, equation = "hyperbolic", method = "two stage")
    )

    expect_true("fit_dd" %in% class(result$dd_fit))
  })

  it("works with the exponential equation", {
    dat <- make_dd_data()
    result <- suppressWarnings(
      regression$fit_and_format_regression(dat, equation = "exponential", method = "two stage")
    )

    expect_type(result, "list")
    expect_s3_class(result$results, "data.frame")
    expect_equal(nrow(result$results), 3)
  })

  it("results contain expected columns from beezdiscounting", {
    dat <- make_dd_data()
    result <- suppressWarnings(
      regression$fit_and_format_regression(dat, equation = "hyperbolic", method = "two stage")
    )

    expected_cols <- c("id", "method", "term", "estimate", "R2")
    for (col in expected_cols) {
      expect_true(
        col %in% names(result$results),
        info = paste(col, "should be present in results")
      )
    }
  })

  it("preserves participant ids in the results", {
    dat <- make_dd_data()
    result <- suppressWarnings(
      regression$fit_and_format_regression(dat, equation = "hyperbolic", method = "two stage")
    )

    result_ids <- as.character(result$results$id)
    expect_true(all(c("p1", "p2", "p3") %in% result_ids))
  })

  it("results are sorted by original id order from the data", {
    # Create data with ids in a specific non-alphabetical order
    delays <- c(1, 7, 30, 90, 180, 365)
    dat <- data.frame(
      id = rep(c("z_last", "a_first", "m_mid"), each = 6),
      x = rep(delays, 3),
      y = c(
        95, 85, 60, 35, 20, 10,
        90, 78, 55, 30, 18, 8,
        92, 80, 58, 32, 19, 9
      )
    )
    result <- suppressWarnings(
      regression$fit_and_format_regression(dat, equation = "hyperbolic", method = "two stage")
    )

    # Factor levels should match original data order, not alphabetical
    expect_equal(levels(result$results$id), c("z_last", "a_first", "m_mid"))
    expect_equal(as.character(result$results$id), c("z_last", "a_first", "m_mid"))
  })
})
