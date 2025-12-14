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
})
