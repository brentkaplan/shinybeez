# Tests for app/logic/mixed_effects/systematic_utils.R

box::use(
  testthat[...],
)

box::use(
  app / logic / mixed_effects / systematic_utils
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
    ),
    stringsAsFactors = FALSE
  )
}

make_demand_data_custom_cols <- function() {
  prices <- c(0.01, 0.1, 1, 10, 100)
  data.frame(
    subject = rep(c("1", "2"), each = 5),
    price = rep(prices, 2),
    consumption = c(
      10, 8, 5, 2, 0,
      12, 9, 6, 3, 1
    ),
    stringsAsFactors = FALSE
  )
}

make_grouped_demand_data <- function() {
  prices <- c(0.01, 0.1, 1, 10, 100)
  data.frame(
    id = rep(c("1", "2", "3"), each = 5),
    x = rep(prices, 3),
    y = c(
      10, 8, 5, 2, 0,
      12, 9, 6, 3, 1,
      8, 7, 4, 1, 0
    ),
    condition = rep(c("A", "A", "B"), each = 5),
    stringsAsFactors = FALSE
  )
}

make_noisy_demand_data <- function() {
  prices <- c(0.01, 0.1, 1, 10, 100)
  data.frame(
    id = rep(c("1", "2"), each = 5),
    x = rep(prices, 2),
    y = c(
      10, 8, 5, 2, 0,
      10, 5, 8, 2, 4
    ),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------------
# compute_systematic_criteria() — edge cases
# ------------------------------------------------------------------------------

describe("compute_systematic_criteria (edge cases)", {
  it("returns NULL for NULL input", {
    result <- systematic_utils$compute_systematic_criteria(
      df_raw = NULL,
      id_col = "id",
      x_col = "x",
      y_col = "y"
    )
    expect_null(result)
  })

  it("returns NULL for empty data frame", {
    empty_df <- data.frame(id = character(), x = numeric(), y = numeric())
    result <- systematic_utils$compute_systematic_criteria(
      df_raw = empty_df,
      id_col = "id",
      x_col = "x",
      y_col = "y"
    )
    expect_null(result)
  })

  it("returns NULL when required columns are missing", {
    d <- data.frame(a = 1:3, b = 4:6)
    result <- systematic_utils$compute_systematic_criteria(
      df_raw = d,
      id_col = "id",
      x_col = "x",
      y_col = "y"
    )
    expect_null(result)
  })
})

# ------------------------------------------------------------------------------
# compute_systematic_criteria() — ungrouped
# ------------------------------------------------------------------------------

describe("compute_systematic_criteria (ungrouped)", {
  it("returns a data frame with v0.2.0 snake_case columns", {
    d <- make_demand_data()
    result <- systematic_utils$compute_systematic_criteria(
      df_raw = d,
      id_col = "id",
      x_col = "x",
      y_col = "y"
    )

    expect_s3_class(result, "data.frame")
    expected_cols <- c(
      "id", "systematic", "trend_stat", "trend_pass",
      "bounce_stat", "bounce_pass", "reversals",
      "reversals_pass", "n_positive"
    )
    for (col in expected_cols) {
      expect_true(
        col %in% names(result),
        info = paste("Expected column", col, "in result")
      )
    }
  })

  it("returns one row per unique id", {
    d <- make_demand_data()
    result <- systematic_utils$compute_systematic_criteria(
      df_raw = d,
      id_col = "id",
      x_col = "x",
      y_col = "y"
    )
    expect_equal(nrow(result), 3)
  })

  it("handles custom column names", {
    d <- make_demand_data_custom_cols()
    result <- systematic_utils$compute_systematic_criteria(
      df_raw = d,
      id_col = "subject",
      x_col = "price",
      y_col = "consumption"
    )

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 2)
    expect_true("id" %in% names(result))
  })

  it("passes custom criteria thresholds through", {
    d <- make_noisy_demand_data()

    result_lenient <- systematic_utils$compute_systematic_criteria(
      df_raw = d,
      id_col = "id",
      x_col = "x",
      y_col = "y",
      bounce = 0.30
    )
    result_strict <- systematic_utils$compute_systematic_criteria(
      df_raw = d,
      id_col = "id",
      x_col = "x",
      y_col = "y",
      bounce = 0.10
    )

    expect_equal(nrow(result_lenient), nrow(result_strict))
    expect_false(
      identical(result_lenient$bounce_pass, result_strict$bounce_pass),
      info = "Different bounce thresholds should produce different results"
    )
  })

  it("uses prepare_fn when provided", {
    d <- make_demand_data_custom_cols()
    prepare_fn <- function(df, id_col, x_col, y_col) {
      out <- df[, c(id_col, x_col, y_col), drop = FALSE]
      names(out) <- c("id", "x", "y")
      suppressWarnings({
        out$x <- as.numeric(out$x)
        out$y <- as.numeric(out$y)
      })
      out[!is.na(out$y), , drop = FALSE]
    }

    result <- systematic_utils$compute_systematic_criteria(
      df_raw = d,
      id_col = "subject",
      x_col = "price",
      y_col = "consumption",
      prepare_fn = prepare_fn
    )

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 2)
  })
})

# ------------------------------------------------------------------------------
# compute_systematic_criteria() — grouped
# ------------------------------------------------------------------------------

describe("compute_systematic_criteria (grouped)", {
  it("returns a data frame with group variable column", {
    d <- make_grouped_demand_data()
    result <- systematic_utils$compute_systematic_criteria(
      df_raw = d,
      id_col = "id",
      x_col = "x",
      y_col = "y",
      group_vars = "condition"
    )

    expect_s3_class(result, "data.frame")
    expect_true("condition" %in% names(result))
  })

  it("returns rows from all groups", {
    d <- make_grouped_demand_data()
    result <- systematic_utils$compute_systematic_criteria(
      df_raw = d,
      id_col = "id",
      x_col = "x",
      y_col = "y",
      group_vars = "condition"
    )

    groups_in_result <- unique(result$condition)
    expect_true("A" %in% groups_in_result)
    expect_true("B" %in% groups_in_result)
  })

  it("returns one row per id within each group", {
    d <- make_grouped_demand_data()
    result <- systematic_utils$compute_systematic_criteria(
      df_raw = d,
      id_col = "id",
      x_col = "x",
      y_col = "y",
      group_vars = "condition"
    )

    group_a <- result[result$condition == "A", ]
    group_b <- result[result$condition == "B", ]
    expect_equal(nrow(group_a), 2)
    expect_equal(nrow(group_b), 1)
  })

  it("returns NULL when group_vars columns are missing from data", {
    d <- make_demand_data()
    result <- systematic_utils$compute_systematic_criteria(
      df_raw = d,
      id_col = "id",
      x_col = "x",
      y_col = "y",
      group_vars = "nonexistent"
    )
    expect_null(result)
  })

  it("contains v0.2.0 snake_case columns alongside group variable", {
    d <- make_grouped_demand_data()
    result <- systematic_utils$compute_systematic_criteria(
      df_raw = d,
      id_col = "id",
      x_col = "x",
      y_col = "y",
      group_vars = "condition"
    )

    expected_cols <- c(
      "condition", "id", "systematic", "trend_stat", "trend_pass",
      "bounce_stat", "bounce_pass", "reversals",
      "reversals_pass", "n_positive"
    )
    for (col in expected_cols) {
      expect_true(
        col %in% names(result),
        info = paste("Expected column", col, "in grouped result")
      )
    }
  })
})

# ------------------------------------------------------------------------------
# Tests for validate_group_vars
# ------------------------------------------------------------------------------

describe("validate_group_vars", {
  it("returns empty character vector when group_vars is NULL", {
    result <- systematic_utils$validate_group_vars(NULL, data.frame(a = 1))
    expect_equal(result, character(0))
  })

  it("returns empty character vector when group_vars is empty", {
    result <- systematic_utils$validate_group_vars(character(0), data.frame(a = 1))
    expect_equal(result, character(0))
  })

  it("returns empty character vector when all vars exist in data", {
    df <- data.frame(a = 1, b = 2, c = 3)
    result <- systematic_utils$validate_group_vars(c("a", "b"), df)
    expect_equal(result, character(0))
  })

  it("returns missing variables when they do not exist in data", {
    df <- data.frame(a = 1, b = 2)
    result <- systematic_utils$validate_group_vars(c("a", "c", "d"), df)
    expect_equal(sort(result), c("c", "d"))
  })
})
