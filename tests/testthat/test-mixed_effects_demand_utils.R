# Tests for app/logic/mixed_effects_demand_utils.R

box::use(
  testthat[...],
)

box::use(
  app / logic / mixed_effects_demand_utils
)

# ------------------------------------------------------------------------------
# prepare_systematic_input() tests
# ------------------------------------------------------------------------------

describe("prepare_systematic_input", {
  it("selects and renames columns correctly", {
    df <- data.frame(
      monkey = c("m1", "m1", "m2"),
      price = c(0.01, 0.1, 0.01),
      consumption = c(100, 80, 90),
      extra_col = c("a", "b", "c")
    )
    result <- mixed_effects_demand_utils$prepare_systematic_input(
      df,
      id_col = "monkey",
      x_col = "price",
      y_col = "consumption"
    )
    expect_equal(names(result), c("id", "x", "y"))
    expect_equal(nrow(result), 3)
    expect_false("extra_col" %in% names(result))
  })

  it("coerces id to character", {
    df <- data.frame(
      id = c(1, 2, 3),
      x = c(0.01, 0.1, 1),
      y = c(100, 80, 50)
    )
    result <- mixed_effects_demand_utils$prepare_systematic_input(
      df,
      id_col = "id",
      x_col = "x",
      y_col = "y"
    )
    expect_type(result$id, "character")
  })

  it("coerces x and y to numeric", {
    df <- data.frame(
      id = c("p1", "p2"),
      x = c("0.01", "0.1"),
      y = c("100", "80")
    )
    result <- mixed_effects_demand_utils$prepare_systematic_input(
      df,
      id_col = "id",
      x_col = "x",
      y_col = "y"
    )
    expect_type(result$x, "double")
    expect_type(result$y, "double")
  })

  it("drops rows with NA in y column", {
    df <- data.frame(
      id = c("p1", "p1", "p2"),
      x = c(0.01, 0.1, 0.01),
      y = c(100, NA, 90)
    )
    result <- mixed_effects_demand_utils$prepare_systematic_input(
      df,
      id_col = "id",
      x_col = "x",
      y_col = "y"
    )
    expect_equal(nrow(result), 2)
    expect_false(any(is.na(result$y)))
  })

  it("errors when df is NULL", {
    expect_error(
      mixed_effects_demand_utils$prepare_systematic_input(
        NULL,
        id_col = "id",
        x_col = "x",
        y_col = "y"
      ),
      "must be a data.frame"
    )
  })

  it("errors when required column is missing", {
    df <- data.frame(
      id = c("p1", "p2"),
      x = c(0.01, 0.1)
    )
    expect_error(
      mixed_effects_demand_utils$prepare_systematic_input(
        df,
        id_col = "id",
        x_col = "x",
        y_col = "y"
      ),
      "Missing required columns"
    )
  })

  it("handles factor columns correctly", {
    df <- data.frame(
      id = factor(c("p1", "p2")),
      x = c(0.01, 0.1),
      y = c(100, 80)
    )
    result <- mixed_effects_demand_utils$prepare_systematic_input(
      df,
      id_col = "id",
      x_col = "x",
      y_col = "y"
    )
    expect_type(result$id, "character")
    expect_equal(result$id, c("p1", "p2"))
  })

  it("warns when non-numeric x or y values are coerced to NA", {
    df <- data.frame(
      id = c("p1", "p1", "p2"),
      x = c("0.01", "missing", "0.1"),
      y = c("100", "80", "N/A"),
      stringsAsFactors = FALSE
    )
    expect_warning(
      mixed_effects_demand_utils$prepare_systematic_input(
        df,
        id_col = "id",
        x_col = "x",
        y_col = "y"
      ),
      "Non-numeric values found"
    )
  })

  it("does not warn when all x and y values are validly numeric", {
    df <- data.frame(
      id = c("p1", "p2"),
      x = c("0.01", "0.1"),
      y = c("100", "80"),
      stringsAsFactors = FALSE
    )
    expect_no_warning(
      mixed_effects_demand_utils$prepare_systematic_input(
        df,
        id_col = "id",
        x_col = "x",
        y_col = "y"
      )
    )
  })
})


# ------------------------------------------------------------------------------
# is_valid_comparison_data() tests
# ------------------------------------------------------------------------------

describe("is_valid_comparison_data", {
  it("returns TRUE for valid non-empty data frame", {
    df <- data.frame(contrast = "a - b", estimate = 0.5)
    expect_true(mixed_effects_demand_utils$is_valid_comparison_data(df))
  })

  it("returns FALSE for NULL", {
    expect_false(mixed_effects_demand_utils$is_valid_comparison_data(NULL))
  })

  it("returns FALSE for empty data frame", {
    df <- data.frame(contrast = character(0), estimate = numeric(0))
    expect_false(mixed_effects_demand_utils$is_valid_comparison_data(df))
  })

  it("returns FALSE for non-data.frame objects", {
    expect_false(mixed_effects_demand_utils$is_valid_comparison_data(list(
      a = 1
    )))
    expect_false(mixed_effects_demand_utils$is_valid_comparison_data("string"))
    expect_false(mixed_effects_demand_utils$is_valid_comparison_data(123))
    expect_false(mixed_effects_demand_utils$is_valid_comparison_data(c(
      1,
      2,
      3
    )))
  })

  it("returns FALSE for tibble with zero rows", {
    # Simulates what beezdemand returns when contrast fails
    empty_tibble <- tibble::tibble()
    expect_false(mixed_effects_demand_utils$is_valid_comparison_data(
      empty_tibble
    ))
  })

  it("returns TRUE for tibble with data", {
    tbl <- tibble::tibble(contrast = "a - b", estimate = 0.5, p.value = 0.03)
    expect_true(mixed_effects_demand_utils$is_valid_comparison_data(tbl))
  })
})
