# Tests for model_summary module

box::use(
  app / logic / mixed_effects / model_summary
)

# Test get_model_spec
describe("get_model_spec", {
  it("returns NULL for NULL model_fit", {
    expect_null(model_summary$get_model_spec(NULL))
  })

  it("returns NULL when param_info is missing", {
    expect_null(model_summary$get_model_spec(list(model = "something")))
  })

  it("extracts spec from model with factors", {
    mock_fit <- list(
      param_info = list(
        equation_form = "zben",
        y_var = "y_for_model",
        x_var = "x",
        id_var = "id",
        factors = c("drug", "dose")
      ),
      data = data.frame(
        id = rep(1:5, each = 3),
        x = rep(1:3, 5),
        y = rnorm(15)
      )
    )
    spec <- model_summary$get_model_spec(mock_fit)
    expect_equal(spec$equation, "Zero-Bounded Exponential (ZBEn)")
    expect_equal(spec$factors, "drug, dose")
    expect_equal(spec$n_observations, 15)
    expect_equal(spec$n_groups, 5)
  })

  it("handles intercept-only model (no factors)", {
    mock_fit <- list(
      param_info = list(
        equation_form = "simplified",
        y_var = "y",
        x_var = "x",
        id_var = "id",
        factors = NULL
      ),
      data = data.frame(id = 1:3, x = 1:3, y = 1:3)
    )
    spec <- model_summary$get_model_spec(mock_fit)
    expect_equal(spec$equation, "Simplified Exponential")
    expect_equal(spec$factors, "None (intercept-only)")
  })
})

# Test get_fit_statistics
describe("get_fit_statistics", {
  it("returns NULL for NULL model_fit", {
    expect_null(model_summary$get_fit_statistics(NULL))
  })

  it("returns NULL when model is NULL", {
    expect_null(model_summary$get_fit_statistics(list(model = NULL)))
  })

  it("extracts statistics from an lm model (as fallback test)", {
    # Create a simple linear model to test extraction
    mdl <- lm(mpg ~ wt, data = mtcars)
    mock_fit <- list(model = mdl)
    stats <- model_summary$get_fit_statistics(mock_fit)
    expect_true(!is.null(stats))
    expect_true(is.numeric(stats$AIC))
    expect_true(is.numeric(stats$BIC))
    expect_true(is.numeric(stats$logLik))
  })
})

# Test get_variance_components
describe("get_variance_components", {
  it("returns NULL for NULL model_fit", {
    expect_null(model_summary$get_variance_components(NULL))
  })

  it("returns NULL when model is NULL", {
    expect_null(model_summary$get_variance_components(list(model = NULL)))
  })
})
