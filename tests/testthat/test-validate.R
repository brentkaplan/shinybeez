# Tests for app/logic/validate.R

box::use(
  testthat[...],
)

box::use(
  app / logic / validate
)

# ------------------------------------------------------------------------------
# check_data() tests
# ------------------------------------------------------------------------------

describe("check_data", {
  it("validates demand data with id column only (wide format)", {
    df <- data.frame(
      id = c("p1", "p2"),
      `0.01` = c(100, 90),
      `0.1` = c(80, 70),
      `1` = c(50, 40),
      check.names = FALSE
    )
    result <- validate$check_data(df, type = "demand")
    expect_true(result)
  })

  it("validates demand data with id and group columns (wide format)", {
    df <- data.frame(
      id = c("p1", "p2"),
      group = c("A", "B"),
      `0.01` = c(100, 90),
      `0.1` = c(80, 70),
      check.names = FALSE
    )
    result <- validate$check_data(df, type = "demand")
    expect_true(result)
  })

  it("validates demand data in long format", {
    df <- data.frame(
      id = c("p1", "p1", "p2", "p2"),
      x = c(0.01, 0.1, 0.01, 0.1),
      y = c(100, 80, 90, 70)
    )
    result <- validate$check_data(df, type = "demand")
    expect_true(result)
  })

  it("returns error message when id column missing", {
    df <- data.frame(
      participant = c("p1", "p2"),
      `0.01` = c(100, 90),
      check.names = FALSE
    )
    result <- validate$check_data(df, type = "demand")
    expect_type(result, "character")
    expect_match(result, "id", ignore.case = TRUE)
  })

  it("validates mixed_effects_demand data", {
    df <- data.frame(
      id = c("m1", "m1", "m2", "m2"),
      x = c(0.01, 0.1, 0.01, 0.1),
      y = c(100, 80, 90, 70)
    )
    result <- validate$check_data(df, type = "mixed_effects_demand")
    expect_true(result)
  })

  it("validates mixed_effects_demand with monkey column", {
    df <- data.frame(
      monkey = c("m1", "m1", "m2", "m2"),
      x = c(0.01, 0.1, 0.01, 0.1),
      y = c(100, 80, 90, 70)
    )
    result <- validate$check_data(df, type = "mixed_effects_demand")
    expect_true(result)
  })

  it("returns error when x column missing for mixed_effects_demand", {
    df <- data.frame(
      id = c("m1", "m2"),
      price = c(0.01, 0.1),
      y = c(100, 80)
    )
    result <- validate$check_data(df, type = "mixed_effects_demand")
    expect_type(result, "character")
    expect_match(result, "x", ignore.case = TRUE)
  })

  # Column name normalization tests
  it("accepts uppercase column names by normalizing them", {
    df <- data.frame(
      ID = c("p1", "p1", "p2", "p2"),
      X = c(0.01, 0.1, 0.01, 0.1),
      Y = c(100, 80, 90, 70)
    )
    result <- validate$check_data(df, type = "demand")
    expect_true(result)
  })

  it("accepts column names with leading/trailing whitespace", {
    df <- data.frame(
      id = c("p1", "p1", "p2", "p2"),
      x = c(0.01, 0.1, 0.01, 0.1),
      y = c(100, 80, 90, 70)
    )
    # Simulate whitespace in column names
    colnames(df) <- c(" id", "x ", " y ")
    result <- validate$check_data(df, type = "demand")
    expect_true(result)
  })

  it("accepts mixed case column names for discounting", {
    df <- data.frame(
      ID = c("p1", "p1"),
      X = c(0.01, 0.1),
      Y = c(100, 80)
    )
    result <- validate$check_data(df, type = "discounting")
    expect_true(result)
  })

  it("accepts mixed case column names for mixed_effects_demand", {
    df <- data.frame(
      ID = c("m1", "m1"),
      X = c(0.01, 0.1),
      Y = c(100, 80)
    )
    result <- validate$check_data(df, type = "mixed_effects_demand")
    expect_true(result)
  })

  it("rejects wide format demand data with non-numeric column names", {
    df <- data.frame(
      id = c("p1", "p2"),
      price_low = c(100, 90),
      price_high = c(80, 70),
      check.names = FALSE
    )
    result <- validate$check_data(df, type = "demand")
    expect_type(result, "character")
    expect_match(result, "not numeric", ignore.case = TRUE)
  })

  it("rejects wide grouped demand data with non-numeric column names", {
    df <- data.frame(
      id = c("p1", "p2"),
      group = c("A", "B"),
      abc = c(100, 90),
      def = c(80, 70),
      check.names = FALSE
    )
    result <- validate$check_data(df, type = "demand")
    expect_type(result, "character")
    expect_match(result, "not numeric", ignore.case = TRUE)
  })
})

# ------------------------------------------------------------------------------
# obliterate_empty_cols() tests
# ------------------------------------------------------------------------------

describe("obliterate_empty_cols", {
  it("removes columns that are entirely NA", {
    df <- data.frame(
      id = c("p1", "p2"),
      empty = c(NA, NA),
      values = c(1, 2)
    )
    result <- validate$obliterate_empty_cols(df)
    expect_equal(names(result), c("id", "values"))
    expect_equal(nrow(result), 2)
  })

  it("keeps columns with at least one non-NA value", {
    df <- data.frame(
      id = c("p1", "p2"),
      partial = c(NA, 1),
      values = c(1, 2)
    )
    result <- validate$obliterate_empty_cols(df)
    expect_equal(names(result), c("id", "partial", "values"))
  })

  it("returns unchanged dataframe when no empty columns", {
    df <- data.frame(
      id = c("p1", "p2"),
      values = c(1, 2)
    )
    result <- validate$obliterate_empty_cols(df)
    expect_equal(result, df)
  })

  it("removes phantom columns from CSV with trailing commas (demand)", {
    df <- vroom::vroom(
      test_path("fixtures", "demand-phantom-cols.csv"),
      delim = ",", show_col_types = FALSE
    )
    # CSV with trailing commas creates an all-NA column
    expect_true(any(colSums(is.na(df)) == nrow(df)))
    result <- validate$obliterate_empty_cols(df)
    expect_equal(names(result), c("id", "x", "y"))
  })

  it("removes phantom columns from CSV with trailing commas (discounting)", {
    df <- vroom::vroom(
      test_path("fixtures", "discounting-ip-phantom-cols.csv"),
      delim = ",", show_col_types = FALSE
    )
    expect_true(any(colSums(is.na(df)) == nrow(df)))
    result <- validate$obliterate_empty_cols(df)
    expect_equal(names(result), c("id", "x", "y"))
  })

  it("removes phantom columns from CSV with trailing commas (mixed effects)", {
    df <- vroom::vroom(
      test_path("fixtures", "mixed-effects-phantom-cols.csv"),
      delim = ",", show_col_types = FALSE
    )
    expect_true(any(colSums(is.na(df)) == nrow(df)))
    result <- validate$obliterate_empty_cols(df)
    expect_equal(names(result), c("id", "x", "y"))
  })
})

# ------------------------------------------------------------------------------
# Phantom column integration: obliterate then validate (Issue #4)
# ------------------------------------------------------------------------------

describe("phantom column integration", {
  it("demand data with trailing commas passes validation after obliteration", {
    df <- vroom::vroom(
      test_path("fixtures", "demand-phantom-cols.csv"),
      delim = ",", show_col_types = FALSE
    )
    colnames(df) <- trimws(tolower(colnames(df)))
    df <- validate$obliterate_empty_cols(df)
    result <- validate$check_data(df, type = "demand")
    expect_true(result)
  })

  it("discounting IP data with trailing commas passes validation after obliteration", {
    df <- vroom::vroom(
      test_path("fixtures", "discounting-ip-phantom-cols.csv"),
      delim = ",", show_col_types = FALSE
    )
    colnames(df) <- trimws(tolower(colnames(df)))
    df <- validate$obliterate_empty_cols(df)
    result <- validate$check_data(df, type = "discounting")
    expect_true(result)
  })

  it("mixed effects data with trailing commas passes validation after obliteration", {
    df <- vroom::vroom(
      test_path("fixtures", "mixed-effects-phantom-cols.csv"),
      delim = ",", show_col_types = FALSE
    )
    colnames(df) <- trimws(tolower(colnames(df)))
    df <- validate$obliterate_empty_cols(df)
    result <- validate$check_data(df, type = "mixed_effects_demand")
    expect_true(result)
  })
})

# ------------------------------------------------------------------------------
# reshape_data() tests
# ------------------------------------------------------------------------------

describe("reshape_data", {
  it("reshapes wide demand data to long format", {
    df <- data.frame(
      id = c("p1", "p2"),
      `0.01` = c(100, 90),
      `0.1` = c(80, 70),
      check.names = FALSE
    )
    # First rename columns as the function expects
    df <- validate$rename_cols(df)
    result <- validate$reshape_data(df, type = "demand")
    expect_equal(nrow(result), 4)
    expect_true("x" %in% names(result))
    expect_true("y" %in% names(result))
  })

  it("reshapes grouped wide demand data to long format", {
    df <- data.frame(
      id = c("p1", "p2"),
      group = c("A", "B"),
      `0.01` = c(100, 90),
      `0.1` = c(80, 70),
      `1` = c(50, 40),
      check.names = FALSE
    )
    df <- validate$rename_cols(df)
    result <- validate$reshape_data(df, type = "demand")
    expect_equal(nrow(result), 6)
    expect_true(all(c("id", "group", "x", "y") %in% names(result)))
    expect_equal(sort(unique(result$group)), c("A", "B"))
    expect_equal(sort(unique(as.character(result$id))), c("p1", "p2"))
  })

  it("preserves NA values during wide-to-long reshape", {
    df <- data.frame(
      id = c("p1", "p2"),
      `0.01` = c(100, NA),
      `0.1` = c(80, 70),
      check.names = FALSE
    )
    df <- validate$rename_cols(df)
    result <- suppressWarnings(validate$reshape_data(df, type = "demand"))
    expect_equal(nrow(result), 4)
    expect_true(any(is.na(result$y)))
  })

  it("returns long data unchanged", {
    df <- data.frame(
      id = c("p1", "p1", "p2", "p2"),
      x = c(0.01, 0.1, 0.01, 0.1),
      y = c(100, 80, 90, 70)
    )
    result <- validate$reshape_data(df, type = "demand")
    expect_equal(nrow(result), 4)
  })
})

# ------------------------------------------------------------------------------
# retype_data() tests
# ------------------------------------------------------------------------------

describe("retype_data", {
  it("does not re-parse integer x columns", {
    df <- data.frame(
      id = c("p1", "p1", "p2", "p2"),
      x = c(1L, 2L, 1L, 2L),
      y = c(100, 80, 90, 70)
    )
    result <- validate$retype_data(df)
    expect_true(is.numeric(result$x))
    expect_equal(as.numeric(result$x), c(1, 2, 1, 2))
  })

  it("re-parses character x columns", {
    df <- data.frame(
      id = c("p1", "p2"),
      x = c("0.01", "0.1"),
      y = c(100, 80),
      stringsAsFactors = FALSE
    )
    result <- validate$retype_data(df)
    expect_true(is.numeric(result$x))
    expect_equal(as.numeric(result$x), c(0.01, 0.1))
  })
})

# ------------------------------------------------------------------------------
# remove_na_rows() tests
# ------------------------------------------------------------------------------

describe("remove_na_rows", {
  it("removes rows with any NA values", {
    df <- data.frame(
      id = c("p1", "p2", "p3"),
      x = c(1, NA, 3),
      y = c(10, 20, 30)
    )
    result <- validate$remove_na_rows(df)
    expect_equal(result$n_dropped, 1)
    expect_equal(nrow(result$data), 2)
  })

  it("returns zero dropped when no NAs present", {
    df <- data.frame(
      id = c("p1", "p2"),
      x = c(1, 2),
      y = c(10, 20)
    )
    result <- validate$remove_na_rows(df)
    expect_equal(result$n_dropped, 0)
    expect_equal(nrow(result$data), 2)
  })

  it("handles dataframe with all NAs in one column", {
    df <- data.frame(
      id = c("p1", "p2"),
      x = c(NA, NA),
      y = c(10, 20)
    )
    result <- validate$remove_na_rows(df)
    expect_equal(result$n_dropped, 2)
    expect_equal(nrow(result$data), 0)
  })

  it("returns 0-row data frame when every row has at least one NA", {
    df <- data.frame(
      id = c("p1", "p2", "p3"),
      x = c(NA, 1, NA),
      y = c(10, NA, NA)
    )
    result <- validate$remove_na_rows(df)
    expect_equal(result$n_dropped, 3)
    expect_equal(nrow(result$data), 0)
    expect_true(is.data.frame(result$data))
  })
})

# ------------------------------------------------------------------------------
# k_values constant test
# ------------------------------------------------------------------------------

describe("k_values", {
  it("contains expected k values", {
    expect_equal(validate$k_values, c(1.5, 2, 2.5, 3, 3.5, 4))
  })
})
