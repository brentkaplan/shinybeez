# Tests for app/logic/demand/empirical.R

box::use(
  testthat[...],
)

box::use(
  app / logic / demand / empirical
)

# --- Test data helpers --------------------------------------------------------

make_demand_data <- function() {
  prices <- c(0.01, 0.1, 1, 10, 100)
  data.frame(
    id = rep(c("1", "2", "3"), each = 5),
    x = rep(prices, 3),
    y = c(10, 8, 5, 2, 0,
          12, 9, 6, 3, 1,
          8, 7, 4, 1, 0)
  )
}

make_grouped_demand_data <- function() {
  d <- make_demand_data()
  d$group <- rep(c("A", "A", "B"), each = 5)
  d
}

# ------------------------------------------------------------------------------
# compute_descriptives() tests
# ------------------------------------------------------------------------------

describe("compute_descriptives", {

  it("returns a data frame for ungrouped data", {
    d <- make_demand_data()
    result <- empirical$compute_descriptives(d, is_grouped = FALSE)
    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
  })

  it("has expected columns for ungrouped data", {
    d <- make_demand_data()
    result <- empirical$compute_descriptives(d, is_grouped = FALSE)
    expected_cols <- c("Price", "Mean", "Median", "SD", "PropZeros", "NAs",
                       "Min", "Max")
    expect_true(all(expected_cols %in% colnames(result)))
    expect_false("group" %in% colnames(result))
  })

  it("rounds numeric columns to 2 decimal places for ungrouped data", {
    d <- make_demand_data()
    result <- empirical$compute_descriptives(d, is_grouped = FALSE)
    numeric_cols <- result[, sapply(result, is.numeric)]
    all_rounded <- all(sapply(numeric_cols, function(col) {
      all(col == round(col, 2), na.rm = TRUE)
    }), na.rm = TRUE)
    expect_true(all_rounded)
  })

  it("returns one row per unique price for ungrouped data", {
    d <- make_demand_data()
    result <- empirical$compute_descriptives(d, is_grouped = FALSE)
    expect_equal(nrow(result), length(unique(d$x)))
  })

  it("returns a data frame with group column for grouped data", {
    d <- make_grouped_demand_data()
    result <- empirical$compute_descriptives(d, is_grouped = TRUE)
    expect_s3_class(result, "data.frame")
    expect_true("group" %in% colnames(result))
  })

  it("contains aggregate and individual group rows for grouped data", {
    d <- make_grouped_demand_data()
    result <- empirical$compute_descriptives(d, is_grouped = TRUE)
    group_values <- unique(result$group)
    expect_true("aggregate" %in% group_values)
    expect_true("A" %in% group_values)
    expect_true("B" %in% group_values)
  })

  it("places group column before Price for grouped data", {
    d <- make_grouped_demand_data()
    result <- empirical$compute_descriptives(d, is_grouped = TRUE)
    col_positions <- match(c("group", "Price"), colnames(result))
    expect_true(col_positions[1] < col_positions[2])
  })

  it("rounds numeric columns to 2 decimal places for grouped data", {
    d <- make_grouped_demand_data()
    result <- empirical$compute_descriptives(d, is_grouped = TRUE)
    numeric_cols <- result[, sapply(result, is.numeric)]
    all_rounded <- all(sapply(numeric_cols, function(col) {
      all(col == round(col, 2), na.rm = TRUE)
    }), na.rm = TRUE)
    expect_true(all_rounded)
  })

  it("errors when is_grouped is TRUE but no group column present", {
    d <- make_demand_data()
    expect_error(
      empirical$compute_descriptives(d, is_grouped = TRUE),
      "group.*column"
    )
  })
})

# ------------------------------------------------------------------------------
# compute_empirical_measures() tests
# ------------------------------------------------------------------------------

describe("compute_empirical_measures", {

  it("returns a data frame for ungrouped data", {
    d <- make_demand_data()
    result <- empirical$compute_empirical_measures(d, is_grouped = FALSE)
    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
  })

  it("has expected columns for ungrouped data", {
    d <- make_demand_data()
    result <- empirical$compute_empirical_measures(d, is_grouped = FALSE)
    expected_cols <- c("id", "Intensity", "BP0", "BP1", "Omaxe", "Pmaxe")
    expect_true(all(expected_cols %in% colnames(result)))
    expect_false("group" %in% colnames(result))
  })

  it("includes aggregate and individual ids for ungrouped data", {
    d <- make_demand_data()
    result <- empirical$compute_empirical_measures(d, is_grouped = FALSE)
    ids <- unique(result$id)
    expect_true("aggregate" %in% ids)
    expect_true(all(c("1", "2", "3") %in% ids))
  })

  it("rounds numeric columns to 2 decimal places for ungrouped data", {
    d <- make_demand_data()
    result <- empirical$compute_empirical_measures(d, is_grouped = FALSE)
    numeric_cols <- result[, sapply(result, is.numeric)]
    all_rounded <- all(sapply(numeric_cols, function(col) {
      all(col == round(col, 2), na.rm = TRUE)
    }))
    expect_true(all_rounded)
  })

  it("returns a data frame with group column for grouped data", {
    d <- make_grouped_demand_data()
    result <- empirical$compute_empirical_measures(d, is_grouped = TRUE)
    expect_s3_class(result, "data.frame")
    expect_true("group" %in% colnames(result))
  })

  it("contains aggregate and individual group values for grouped data", {
    d <- make_grouped_demand_data()
    result <- empirical$compute_empirical_measures(d, is_grouped = TRUE)
    group_values <- unique(result$group)
    expect_true("aggregate" %in% group_values)
    expect_true("A" %in% group_values)
    expect_true("B" %in% group_values)
  })

  it("includes group aggregate rows for grouped data", {
    d <- make_grouped_demand_data()
    result <- empirical$compute_empirical_measures(d, is_grouped = TRUE)
    expect_true("group aggregate" %in% result$id)
  })

  it("includes individual participant rows for grouped data", {
    d <- make_grouped_demand_data()
    result <- empirical$compute_empirical_measures(d, is_grouped = TRUE)
    individual_ids <- unique(result$id)
    expect_true(all(c("1", "2", "3") %in% individual_ids))
  })

  it("rounds numeric columns to 2 decimal places for grouped data", {
    d <- make_grouped_demand_data()
    result <- empirical$compute_empirical_measures(d, is_grouped = TRUE)
    numeric_cols <- result[, sapply(result, is.numeric)]
    all_rounded <- all(sapply(numeric_cols, function(col) {
      all(col == round(col, 2), na.rm = TRUE)
    }))
    expect_true(all_rounded)
  })

  it("errors when is_grouped is TRUE but no group column present", {
    d <- make_demand_data()
    expect_error(
      empirical$compute_empirical_measures(d, is_grouped = TRUE),
      "group.*column"
    )
  })
})
