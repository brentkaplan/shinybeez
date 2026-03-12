# Tests for app/logic/discounting/systematic.R

box::use(
  testthat[...],
)

box::use(
  app / logic / discounting / systematic
)

# --- Test data helpers --------------------------------------------------------

make_discounting_data <- function() {
  delays <- c(1, 7, 30, 90, 365)
  data.frame(
    id = rep(c("1", "2", "3"), each = 5),
    x = rep(delays, 3),
    y = c(
      95, 85, 60, 30, 10,   # id 1: systematic
      90, 80, 50, 25, 5,    # id 2: systematic
      80, 70, 40, 15, 3     # id 3: systematic
    )
  )
}

# Data where c1 and c2 criteria can be toggled by changing thresholds.
# - id "1": y goes 0.9, 0.7, 0.95, 0.3, 0.1
#   consecutive increase = 0.25 (0.7 -> 0.95)
#   c1 = 0.2 => 0.25 > 0.2 => c1 FAIL
#   c1 = 0.3 => 0.25 < 0.3 => c1 PASS
# - id "2": y goes 0.9, 0.88, 0.87, 0.86, 0.85
#   last - first diff = 0.05
#   c2 = 0.1 => 0.85 not < 0.9 - 0.1 = 0.80 => c2 FAIL
#   c2 = 0.03 => 0.85 < 0.9 - 0.03 = 0.87 => c2 PASS
make_threshold_sensitive_data <- function() {
  delays <- c(1, 7, 30, 90, 365)
  data.frame(
    id = rep(c("1", "2"), each = 5),
    x = rep(delays, 2),
    y = c(
      0.9, 0.7, 0.95, 0.3, 0.1,
      0.9, 0.88, 0.87, 0.86, 0.85
    )
  )
}

# ------------------------------------------------------------------------------
# compute_systematic_discounting tests
# ------------------------------------------------------------------------------

describe("compute_systematic_discounting", {

  it("returns a data frame with expected columns", {
    d <- make_discounting_data()
    result <- systematic$compute_systematic_discounting(d)

    expect_s3_class(result, "data.frame")
    expected_cols <- c("id", "c1_pass", "c2_pass")
    for (col in expected_cols) {
      expect_true(
        col %in% names(result),
        info = paste("Expected column", col, "in result")
      )
    }
  })

  it("returns one row per unique id", {
    d <- make_discounting_data()
    result <- systematic$compute_systematic_discounting(d)

    unique_ids <- unique(d$id)
    expect_equal(nrow(result), length(unique_ids))
    expect_true(all(unique_ids %in% as.character(result$id)))
  })

  it("returns results sorted by original id order", {
    # Use ids in non-alphabetical order to verify sorting is by
    # original factor order, not alphabetical
    delays <- c(1, 7, 30, 90, 365)
    d <- data.frame(
      id = rep(c("Z", "A", "M"), each = 5),
      x = rep(delays, 3),
      y = c(
        95, 85, 60, 30, 10,
        90, 80, 50, 25, 5,
        80, 70, 40, 15, 3
      )
    )

    result <- systematic$compute_systematic_discounting(d)
    expect_equal(as.character(result$id), c("Z", "A", "M"))
  })

  it("returns id column as a factor with levels matching original data order", {
    delays <- c(1, 7, 30, 90, 365)
    d <- data.frame(
      id = rep(c("Z", "A", "M"), each = 5),
      x = rep(delays, 3),
      y = c(
        95, 85, 60, 30, 10,
        90, 80, 50, 25, 5,
        80, 70, 40, 15, 3
      )
    )

    result <- systematic$compute_systematic_discounting(d)
    expect_true(is.factor(result$id))
    expect_equal(levels(result$id), c("Z", "A", "M"))
  })

  it("works with default c1 and c2 values", {
    d <- make_discounting_data()
    # Should not error when called with only the data argument
    result <- systematic$compute_systematic_discounting(d)

    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 0)
  })

  it("passes custom c1 values through correctly", {
    d <- make_threshold_sensitive_data()

    # id "1" has consecutive increase of 0.25
    # c1 = 0.2 => FAIL; c1 = 0.3 => PASS
    result_strict <- systematic$compute_systematic_discounting(d, c1 = 0.2, c2 = 0.5)
    result_lenient <- systematic$compute_systematic_discounting(d, c1 = 0.3, c2 = 0.5)

    id1_strict <- result_strict[result_strict$id == "1", ]
    id1_lenient <- result_lenient[result_lenient$id == "1", ]

    expect_false(id1_strict$c1_pass)
    expect_true(id1_lenient$c1_pass)
  })

  it("passes custom c2 values through correctly", {
    d <- make_threshold_sensitive_data()

    # id "2" has global decrease of 0.05
    # c2 = 0.1 => FAIL; c2 = 0.03 => PASS
    result_strict <- systematic$compute_systematic_discounting(d, c1 = 0.5, c2 = 0.1)
    result_lenient <- systematic$compute_systematic_discounting(d, c1 = 0.5, c2 = 0.03)

    id2_strict <- result_strict[result_strict$id == "2", ]
    id2_lenient <- result_lenient[result_lenient$id == "2", ]

    expect_false(id2_strict$c2_pass)
    expect_true(id2_lenient$c2_pass)
  })

  it("identifies systematic data as passing both criteria", {
    d <- make_discounting_data()
    result <- systematic$compute_systematic_discounting(d)

    # All three ids in the test data are cleanly decreasing
    expect_true(all(result$c1_pass))
    expect_true(all(result$c2_pass))
  })

  it("handles a single participant", {
    delays <- c(1, 7, 30, 90, 365)
    d <- data.frame(
      id = rep("solo", 5),
      x = delays,
      y = c(95, 85, 60, 30, 10)
    )

    result <- systematic$compute_systematic_discounting(d)
    expect_equal(nrow(result), 1)
    expect_equal(as.character(result$id), "solo")
  })
})
