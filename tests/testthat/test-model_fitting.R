# Tests for app/logic/mixed_effects/model_fitting.R

box::use(
  testthat[...],
)

box::use(
  app / logic / mixed_effects / model_fitting
)

# ------------------------------------------------------------------------------
# transform_covariate() tests
# ------------------------------------------------------------------------------

describe("transform_covariate", {
  it("returns unmodified values when center=FALSE and scale=FALSE", {
    x <- c(10, 20, 30)
    result <- model_fitting$transform_covariate(
      x,
      center = FALSE,
      scale = FALSE
    )
    expect_equal(result$values, x)
    expect_equal(result$transform_applied, "none")
  })

  it("centers values when center=TRUE", {
    x <- c(10, 20, 30)
    result <- model_fitting$transform_covariate(x, center = TRUE, scale = FALSE)
    expect_equal(mean(result$values), 0)
    expect_equal(result$transform_applied, "centered")
    expect_equal(result$mu, 20)
  })

  it("centers and scales when both TRUE", {
    x <- c(10, 20, 30)
    result <- model_fitting$transform_covariate(x, center = TRUE, scale = TRUE)
    expect_equal(mean(result$values), 0, tolerance = 1e-10)
    expect_equal(sd(result$values), 1, tolerance = 1e-10)
    expect_equal(result$transform_applied, "centered_scaled")
  })

  it("handles scale=TRUE with center=FALSE", {
    x <- c(10, 20, 30)
    result <- model_fitting$transform_covariate(x, center = FALSE, scale = TRUE)
    # Still centers and scales
    expect_equal(result$transform_applied, "centered_scaled")
  })

  it("sets scale_failed when SD is zero", {
    x <- c(5, 5, 5) # No variance
    result <- model_fitting$transform_covariate(x, center = TRUE, scale = TRUE)
    expect_true(result$scale_failed)
    expect_equal(result$transform_applied, "centered") # Falls back to centering
  })

  it("handles NA values", {
    x <- c(10, NA, 30)
    result <- model_fitting$transform_covariate(x, center = TRUE, scale = FALSE)
    expect_equal(result$mu, 20)
    expect_true(is.na(result$values[2]))
  })
})

# ------------------------------------------------------------------------------
# transform_at_value() tests
# ------------------------------------------------------------------------------

describe("transform_at_value", {
  it("returns value unchanged when no transformation", {
    result <- model_fitting$transform_at_value(
      25,
      mu = 20,
      sigma = 10,
      center = FALSE,
      scale = FALSE
    )
    expect_equal(result, 25)
  })

  it("centers the value", {
    result <- model_fitting$transform_at_value(
      25,
      mu = 20,
      sigma = 10,
      center = TRUE,
      scale = FALSE
    )
    expect_equal(result, 5) # 25 - 20
  })

  it("centers and scales the value", {
    result <- model_fitting$transform_at_value(
      25,
      mu = 20,
      sigma = 10,
      center = TRUE,
      scale = TRUE
    )
    expect_equal(result, 0.5) # (25 - 20) / 10
  })

  it("returns NA for non-finite input", {
    result <- model_fitting$transform_at_value(
      NA,
      mu = 20,
      sigma = 10,
      center = TRUE,
      scale = TRUE
    )
    expect_true(is.na(result))
  })

  it("handles scale=TRUE with zero sigma gracefully", {
    result <- model_fitting$transform_at_value(
      25,
      mu = 20,
      sigma = 0,
      center = TRUE,
      scale = TRUE
    )
    expect_equal(result, 5) # Falls back to centering only
  })
})

# ------------------------------------------------------------------------------
# build_covariate_column_name() tests
# ------------------------------------------------------------------------------

describe("build_covariate_column_name", {
  it("returns base name when no transformation", {
    result <- model_fitting$build_covariate_column_name(
      "age",
      center = FALSE,
      scale = FALSE
    )
    expect_equal(result, "age")
  })

  it("adds _c suffix for centering", {
    result <- model_fitting$build_covariate_column_name(
      "age",
      center = TRUE,
      scale = FALSE
    )
    expect_equal(result, "age_c")
  })

  it("adds _cs suffix for centering and scaling", {
    result <- model_fitting$build_covariate_column_name(
      "age",
      center = TRUE,
      scale = TRUE
    )
    expect_equal(result, "age_cs")
  })

  it("adds _cs suffix when only scale=TRUE", {
    result <- model_fitting$build_covariate_column_name(
      "age",
      center = FALSE,
      scale = TRUE
    )
    expect_equal(result, "age_cs")
  })
})

# ------------------------------------------------------------------------------
# process_covariate() tests
# ------------------------------------------------------------------------------

describe("process_covariate", {
  it("returns unchanged df when no covariate specified", {
    df <- data.frame(id = 1:3, x = c(1, 2, 3))
    result <- model_fitting$process_covariate(df, covariate_col = NULL)
    expect_equal(result$df, df)
    expect_null(result$model_covariate_name)
    expect_null(result$at_list)
  })

  it("returns unchanged df when covariate not in data", {
    df <- data.frame(id = 1:3, x = c(1, 2, 3))
    result <- model_fitting$process_covariate(df, covariate_col = "age")
    expect_null(result$model_covariate_name)
  })

  it("adds centered column when center=TRUE", {
    df <- data.frame(id = 1:3, age = c(20, 30, 40))
    result <- model_fitting$process_covariate(
      df,
      covariate_col = "age",
      center = TRUE
    )
    expect_true("age_c" %in% names(result$df))
    expect_equal(mean(result$df$age_c), 0)
    expect_equal(result$model_covariate_name, "age_c")
  })

  it("adds centered and scaled column", {
    df <- data.frame(id = 1:3, age = c(20, 30, 40))
    result <- model_fitting$process_covariate(
      df,
      covariate_col = "age",
      center = TRUE,
      scale = TRUE
    )
    expect_true("age_cs" %in% names(result$df))
    expect_equal(result$model_covariate_name, "age_cs")
  })

  it("builds at_list when at_value provided", {
    df <- data.frame(id = 1:3, age = c(20, 30, 40))
    result <- model_fitting$process_covariate(
      df,
      covariate_col = "age",
      center = TRUE,
      at_value = 35
    )
    expect_type(result$at_list, "list")
    expect_equal(names(result$at_list), "age_c")
    expect_equal(result$at_list$age_c, 5) # 35 - 30 (mean)
  })

  it("converts character covariate to numeric", {
    df <- data.frame(
      id = 1:3,
      age = c("20", "30", "40"),
      stringsAsFactors = FALSE
    )
    result <- model_fitting$process_covariate(df, covariate_col = "age")
    expect_true("age_num" %in% names(result$df))
    expect_equal(result$model_covariate_name, "age_num")
  })
})

# ------------------------------------------------------------------------------
# build_nlme_control() tests
# ------------------------------------------------------------------------------

describe("build_nlme_control", {
  it("returns default preset settings", {
    result <- model_fitting$build_nlme_control(preset = "default")
    expect_equal(result$maxIter, 50L)
    expect_equal(result$tolerance, 1e-6)
  })

  it("returns relaxed preset settings", {
    result <- model_fitting$build_nlme_control(preset = "relaxed")
    expect_equal(result$maxIter, 200L)
    expect_equal(result$tolerance, 1e-4)
  })

  it("returns aggressive preset settings", {
    result <- model_fitting$build_nlme_control(preset = "aggressive")
    expect_equal(result$maxIter, 500L)
    expect_equal(result$tolerance, 1e-8)
  })

  it("returns custom settings when preset is unknown", {
    result <- model_fitting$build_nlme_control(
      preset = "custom",
      max_iter = 100,
      tolerance = 1e-5
    )
    expect_equal(result$maxIter, 100L)
    expect_equal(result$tolerance, 1e-5)
  })

  it("converts parameters to correct types", {
    result <- model_fitting$build_nlme_control(
      preset = "custom",
      max_iter = 100.5, # Should become integer
      tolerance = "1e-5" # Should become numeric
    )
    expect_type(result$maxIter, "integer")
    expect_type(result$tolerance, "double")
  })
})

# ------------------------------------------------------------------------------
# validate_factors() tests
# ------------------------------------------------------------------------------

describe("validate_factors", {
  it("returns valid=TRUE when no factors specified", {
    df <- data.frame(id = 1:3)
    result <- model_fitting$validate_factors(df, factor_cols = NULL)
    expect_true(result$valid)
    expect_length(result$errors, 0)
  })

  it("returns valid=TRUE for valid factor columns", {
    df <- data.frame(
      id = 1:4,
      group = c("A", "B", "A", "B")
    )
    result <- model_fitting$validate_factors(df, factor_cols = "group")
    expect_true(result$valid)
  })

  it("returns error for missing factor column", {
    df <- data.frame(id = 1:3)
    result <- model_fitting$validate_factors(df, factor_cols = "treatment")
    expect_false(result$valid)
    expect_match(result$errors[1], "not found")
  })

  it("returns error for factor with single level", {
    df <- data.frame(
      id = 1:3,
      group = c("A", "A", "A")
    )
    result <- model_fitting$validate_factors(df, factor_cols = "group")
    expect_false(result$valid)
    expect_match(result$errors[1], "fewer than 2 levels")
  })

  it("ignores 'None' factor", {
    df <- data.frame(id = 1:3)
    result <- model_fitting$validate_factors(df, factor_cols = c("None", ""))
    expect_true(result$valid)
  })

  it("validates multiple factors", {
    df <- data.frame(
      id = 1:4,
      group = c("A", "B", "A", "B"),
      session = c(1, 1, 2, 2)
    )
    result <- model_fitting$validate_factors(
      df,
      factor_cols = c("group", "session")
    )
    expect_true(result$valid)
  })
})
