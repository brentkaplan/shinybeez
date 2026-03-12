# Tests for app/logic/mixed_effects/data_prep.R

box::use(
  testthat[...],
)

box::use(
  app / logic / mixed_effects / data_prep
)

# ------------------------------------------------------------------------------
# guess_first_match() tests
# ------------------------------------------------------------------------------

describe("guess_first_match", {
  it("finds exact match (case-insensitive)", {
    result <- data_prep$guess_first_match(c("id", "x"), c("ID", "X", "Y"))
    expect_equal(result, "ID")
  })

  it("returns original case of column name", {
    result <- data_prep$guess_first_match(
      c("monkey"),
      c("Monkey", "Price", "Y")
    )
    expect_equal(result, "Monkey")
  })

  it("returns first match when multiple candidates exist", {
    result <- data_prep$guess_first_match(
      c("id", "subject"),
      c("Other", "Subject", "ID")
    )
    expect_equal(result, "ID") # "id" comes before "subject" in candidates
  })

  it("returns NA_character_ when no match", {
    result <- data_prep$guess_first_match(c("id", "x"), c("foo", "bar"))
    expect_true(is.na(result))
  })

  it("returns NA_character_ for empty cols", {
    result <- data_prep$guess_first_match(c("id"), character(0))
    expect_true(is.na(result))
  })
})

# ------------------------------------------------------------------------------
# guess_id_column() tests
# ------------------------------------------------------------------------------

describe("guess_id_column", {
  it("finds 'id' column", {
    result <- data_prep$guess_id_column(c("id", "x", "y"))
    expect_equal(result, "id")
  })

  it("finds 'monkey' column (common in beezdemand)", {
    result <- data_prep$guess_id_column(c("monkey", "x", "y"))
    expect_equal(result, "monkey")
  })

  it("finds 'subject' column", {
    result <- data_prep$guess_id_column(c("subject", "price", "consumption"))
    expect_equal(result, "subject")
  })

  it("falls back to first column when no match", {
    result <- data_prep$guess_id_column(c("participant_code", "cost", "amount"))
    expect_equal(result, "participant_code")
  })

  it("uses custom fallback index", {
    result <- data_prep$guess_id_column(c("a", "b", "c"), fallback_index = 2L)
    expect_equal(result, "b")
  })
})

# ------------------------------------------------------------------------------
# guess_x_column() tests
# ------------------------------------------------------------------------------

describe("guess_x_column", {
  it("finds 'x' column", {
    result <- data_prep$guess_x_column(c("id", "x", "y"))
    expect_equal(result, "x")
  })

  it("finds 'price' column", {
    result <- data_prep$guess_x_column(c("id", "price", "consumption"))
    expect_equal(result, "price")
  })

  it("falls back to second column when no match", {
    result <- data_prep$guess_x_column(c("subject", "cost", "amount"))
    expect_equal(result, "cost")
  })
})

# ------------------------------------------------------------------------------
# guess_y_column() tests
# ------------------------------------------------------------------------------

describe("guess_y_column", {
  it("finds 'y' column", {
    result <- data_prep$guess_y_column(c("id", "x", "y"))
    expect_equal(result, "y")
  })

  it("finds 'consumption' column", {
    result <- data_prep$guess_y_column(c("id", "price", "consumption"))
    expect_equal(result, "consumption")
  })

  it("finds 'y_ll4' column", {
    result <- data_prep$guess_y_column(c("monkey", "x", "y_ll4"))
    expect_equal(result, "y_ll4")
  })

  it("prefers 'y' over 'y_ll4' when both present", {
    result <- data_prep$guess_y_column(c("monkey", "x", "y_ll4", "y"))
    expect_equal(result, "y") # 'y' comes before 'y_ll4' in candidates
  })

  it("falls back to third column when no match", {
    result <- data_prep$guess_y_column(c("subject", "cost", "amount"))
    expect_equal(result, "amount")
  })
})

# ------------------------------------------------------------------------------
# guess_variable_columns() tests
# ------------------------------------------------------------------------------

describe("guess_variable_columns", {
  it("guesses all three columns correctly", {
    df <- data.frame(id = 1, x = 2, y = 3)
    result <- data_prep$guess_variable_columns(df)
    expect_equal(result$id, "id")
    expect_equal(result$x, "x")
    expect_equal(result$y, "y")
  })

  it("handles typical beezdemand column names", {
    df <- data.frame(monkey = "m1", price = 0.01, consumption = 100)
    result <- data_prep$guess_variable_columns(df)
    expect_equal(result$id, "monkey")
    expect_equal(result$x, "price")
    expect_equal(result$y, "consumption")
  })

  it("returns NAs for NULL input", {
    result <- data_prep$guess_variable_columns(NULL)
    expect_true(is.na(result$id))
    expect_true(is.na(result$x))
    expect_true(is.na(result$y))
  })

  it("returns NAs for empty dataframe", {
    df <- data.frame()
    result <- data_prep$guess_variable_columns(df)
    expect_true(is.na(result$id))
  })
})

# ------------------------------------------------------------------------------
# identify_factor_columns() tests
# ------------------------------------------------------------------------------

describe("identify_factor_columns", {
  it("identifies character columns as factors", {
    df <- data.frame(
      id = c("p1", "p2"),
      group = c("A", "B"),
      x = c(1, 2),
      stringsAsFactors = FALSE
    )
    result <- data_prep$identify_factor_columns(df, exclude_cols = c("id"))
    expect_true("group" %in% result)
  })

  it("identifies factor columns", {
    df <- data.frame(
      id = c("p1", "p2"),
      treatment = factor(c("Drug", "Placebo")),
      x = c(1, 2)
    )
    result <- data_prep$identify_factor_columns(df, exclude_cols = c("id"))
    expect_true("treatment" %in% result)
  })

  it("identifies numeric columns with few unique values", {
    df <- data.frame(
      id = 1:10,
      session = rep(1:2, 5), # Only 2 unique values
      x = rnorm(10)
    )
    result <- data_prep$identify_factor_columns(df, exclude_cols = c("id", "x"))
    expect_true("session" %in% result)
  })

  it("excludes numeric columns with many unique values", {
    df <- data.frame(
      id = 1:100,
      continuous = rnorm(100)
    )
    result <- data_prep$identify_factor_columns(df, exclude_cols = c("id"))
    expect_false("continuous" %in% result)
  })

  it("respects exclude_cols parameter", {
    df <- data.frame(
      id = c("p1", "p2"),
      group = c("A", "B"),
      stringsAsFactors = FALSE
    )
    result <- data_prep$identify_factor_columns(
      df,
      exclude_cols = c("id", "group")
    )
    expect_length(result, 0)
  })

  it("returns empty vector for NULL input", {
    result <- data_prep$identify_factor_columns(NULL)
    expect_length(result, 0)
  })
})

# ------------------------------------------------------------------------------
# identify_covariate_columns() tests
# ------------------------------------------------------------------------------

describe("identify_covariate_columns", {
  it("identifies numeric columns", {
    df <- data.frame(
      id = c("p1", "p2"),
      age = c(25, 30),
      weight = c(70, 80),
      group = c("A", "B"),
      stringsAsFactors = FALSE
    )
    result <- data_prep$identify_covariate_columns(df, exclude_cols = c("id"))
    expect_true("age" %in% result)
    expect_true("weight" %in% result)
    expect_false("group" %in% result)
  })

  it("respects exclude_cols parameter", {
    df <- data.frame(
      id = 1:3,
      x = c(1, 2, 3),
      y = c(10, 20, 30)
    )
    result <- data_prep$identify_covariate_columns(
      df,
      exclude_cols = c("x", "y")
    )
    expect_false("x" %in% result)
    expect_false("y" %in% result)
    expect_true("id" %in% result)
  })

  it("returns empty vector for non-numeric dataframe", {
    df <- data.frame(
      a = c("x", "y"),
      b = c("1", "2"),
      stringsAsFactors = FALSE
    )
    result <- data_prep$identify_covariate_columns(df)
    expect_length(result, 0)
  })
})

# ------------------------------------------------------------------------------
# as_ordered_factor() tests
# ------------------------------------------------------------------------------

describe("as_ordered_factor", {
  it("converts to factor with automatic levels", {
    result <- data_prep$as_ordered_factor(c("B", "A", "C", "A"))
    expect_s3_class(result, "factor")
    expect_equal(levels(result), c("B", "A", "C")) # Order of first appearance
  })

  it("respects specified level order", {
    result <- data_prep$as_ordered_factor(
      c("B", "A", "C", "A"),
      levels = c("A", "B", "C")
    )
    expect_equal(levels(result), c("A", "B", "C"))
  })

  it("handles numeric input", {
    result <- data_prep$as_ordered_factor(c(1, 2, 3, 1))
    expect_s3_class(result, "factor")
  })
})
