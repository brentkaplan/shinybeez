# Tests for app/logic/demand/systematic.R

box::use(
  testthat[...],
)

box::use(
  app / logic / demand / systematic
)

# --- Test data helpers --------------------------------------------------------

make_demand_data <- function() {
  prices <- c(0.01, 0.1, 1, 10, 100)
  data.frame(
    id = rep(c("1", "2", "3"), each = 5),
    x = rep(prices, 3),
    y = c(
      10, 8, 5, 2, 0,
      12, 9, 6, 3, 1,
      8, 7, 4, 1, 0
    )
  )
}

# Data with non-monotonic consumption for id "2" (bounce = 0.25).
# This lets us test that different bounce thresholds produce different results:
# bounce = 0.30 -> id "2" passes; bounce = 0.10 -> id "2" fails.
make_noisy_demand_data <- function() {
  prices <- c(0.01, 0.1, 1, 10, 100)
  data.frame(
    id = rep(c("1", "2"), each = 5),
    x = rep(prices, 2),
    y = c(
      10, 8, 5, 2, 0,
      10, 5, 8, 2, 4
    )
  )
}

make_grouped_demand_data <- function() {
  d <- make_demand_data()
  d$group <- rep(c("A", "A", "B"), each = 5)
  d
}

make_grouped_noisy_data <- function() {
  d <- make_noisy_demand_data()
  d$group <- rep(c("A", "B"), each = 5)
  d
}

# ------------------------------------------------------------------------------
# compute_systematic() — ungrouped
# ------------------------------------------------------------------------------

describe("compute_systematic (ungrouped)", {
  it("returns a data frame with expected systematic columns", {
    d <- make_demand_data()
    result <- systematic$compute_systematic(d)

    expect_s3_class(result, "data.frame")
    expected_cols <- c("id", "TotalPass", "DeltaQ", "DeltaQPass",
                       "Bounce", "BouncePass", "Reversals",
                       "ReversalsPass", "NumPosValues")
    for (col in expected_cols) {
      expect_true(
        col %in% names(result),
        info = paste("Expected column", col, "in result")
      )
    }
  })

  it("returns one row per unique id", {
    d <- make_demand_data()
    result <- systematic$compute_systematic(d)

    unique_ids <- unique(d$id)
    expect_equal(nrow(result), length(unique_ids))
    expect_true(all(unique_ids %in% result$id))
  })

  it("works with default parameter values", {
    d <- make_demand_data()
    # Should not error when called with only the data argument
    result <- systematic$compute_systematic(d)

    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 0)
  })

  it("passes custom criteria values through correctly", {
    # Use noisy data where id "2" has bounce = 0.25.
    # With bounce = 0.30 id "2" passes; with bounce = 0.10 id "2" fails.
    d <- make_noisy_demand_data()

    result_lenient <- systematic$compute_systematic(
      d,
      deltaq = 0.025,
      bounce = 0.30,
      reversals = 0,
      ncons0 = 2
    )
    result_strict <- systematic$compute_systematic(
      d,
      deltaq = 0.025,
      bounce = 0.10,
      reversals = 0,
      ncons0 = 2
    )

    # Both return valid data frames with the same shape
    expect_equal(nrow(result_lenient), nrow(result_strict))
    expect_equal(names(result_lenient), names(result_strict))

    # The BouncePass column should differ for id "2"
    expect_false(
      identical(result_lenient$BouncePass, result_strict$BouncePass),
      info = "Different bounce thresholds should produce different BouncePass results"
    )
  })
})

# ------------------------------------------------------------------------------
# compute_systematic() — grouped
# ------------------------------------------------------------------------------

describe("compute_systematic (grouped)", {
  it("returns a data frame that includes a 'group' column", {
    d <- make_grouped_demand_data()
    result <- systematic$compute_systematic(d, is_grouped = TRUE)

    expect_s3_class(result, "data.frame")
    expect_true("group" %in% names(result))
  })

  it("returns rows from all groups", {
    d <- make_grouped_demand_data()
    result <- systematic$compute_systematic(d, is_grouped = TRUE)

    groups_in_result <- unique(result$group)
    expect_true("A" %in% groups_in_result)
    expect_true("B" %in% groups_in_result)
  })

  it("returns one row per id within each group", {
    d <- make_grouped_demand_data()
    result <- systematic$compute_systematic(d, is_grouped = TRUE)

    # Group A has ids "1" and "2"; Group B has id "3"
    group_a <- result[result$group == "A", ]
    group_b <- result[result$group == "B", ]

    expect_equal(nrow(group_a), 2)
    expect_equal(nrow(group_b), 1)
  })

  it("contains all expected systematic columns alongside 'group'", {
    d <- make_grouped_demand_data()
    result <- systematic$compute_systematic(d, is_grouped = TRUE)

    expected_cols <- c("group", "id", "TotalPass", "DeltaQ", "DeltaQPass",
                       "Bounce", "BouncePass", "Reversals",
                       "ReversalsPass", "NumPosValues")
    for (col in expected_cols) {
      expect_true(
        col %in% names(result),
        info = paste("Expected column", col, "in grouped result")
      )
    }
  })

  it("errors when is_grouped is TRUE but no 'group' column exists", {
    d <- make_demand_data()  # no group column
    expect_error(
      systematic$compute_systematic(d, is_grouped = TRUE),
      "group"
    )
  })

  it("passes custom criteria values through in grouped mode", {
    # Use noisy grouped data where id "2" (group B) has bounce = 0.25.
    d <- make_grouped_noisy_data()

    result_lenient <- systematic$compute_systematic(
      d,
      deltaq = 0.025,
      bounce = 0.30,
      is_grouped = TRUE
    )
    result_strict <- systematic$compute_systematic(
      d,
      deltaq = 0.025,
      bounce = 0.10,
      is_grouped = TRUE
    )

    expect_equal(nrow(result_lenient), nrow(result_strict))

    # The BouncePass column should differ for id "2" in group B
    expect_false(
      identical(result_lenient$BouncePass, result_strict$BouncePass),
      info = "Different bounce thresholds should produce different BouncePass in grouped mode"
    )
  })
})
