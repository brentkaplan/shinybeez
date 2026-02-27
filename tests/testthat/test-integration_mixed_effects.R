# Integration tests: Mixed Effects Demand tab workflows
# Tests default ko data auto-load, upload minimal fixture, and full examples.

# Helper: wait for calculation to complete
wait_for_calc <- function(app, sleep_secs = 5) {
  Sys.sleep(sleep_secs)
  app$wait_for_idle(duration = 2000)
}

# ==========================================================================
# Default ko data (auto-loaded on tab navigation)
# ==========================================================================
describe("Mixed Effects - default ko data", {
  app <- NULL

  it("starts the app and navigates to Mixed Effects Demand", {
    app <<- create_app_driver()
    require_app(app)
    app$set_inputs(!!ids$nav := "MixedEffectsDemand")
    app$wait_for_idle(duration = 2000)
    expect_equal(app$get_value(input = ids$nav), "MixedEffectsDemand")
  })

  it("auto-loads default data and populates variable dropdowns", {
    require_app(app)
    id_var <- app$get_value(input = ids$mixed$id_var)
    x_var <- app$get_value(input = ids$mixed$x_var)
    y_var <- app$get_value(input = ids$mixed$y_var)

    expect_true(!is.null(id_var) && nchar(id_var) > 0)
    expect_true(!is.null(x_var) && nchar(x_var) > 0)
    expect_true(!is.null(y_var) && nchar(y_var) > 0)
  })

  it("renders the data table for default data", {
    require_app(app)
    html <- app$get_html(".datatables")
    expect_true(any(nchar(html) > 0))
  })

  it("selects drug as Factor 1", {
    require_app(app)
    app$set_inputs(!!ids$mixed$factor1 := "drug", wait_ = FALSE)
    Sys.sleep(1)
    expect_equal(app$get_value(input = ids$mixed$factor1), "drug")
  })

  it("runs the mixed effects model", {
    require_app(app)
    app$click(selector = paste0("#", ids$mixed$run))
    wait_for_calc(app, sleep_secs = 30)
    expect_true(TRUE)
  })

  it("renders model summary output", {
    require_app(app)
    summary_sel <- paste0(
      "#", ns_id("mixed_effects_demand", "model_summary_output")
    )
    html <- app$get_html(summary_sel)
    expect_true(any(nchar(html) > 0))
  })

  it("renders fixed effects table", {
    require_app(app)
    fe_sel <- paste0(
      "#", ns_id("mixed_effects_demand", "fixed_effects_table")
    )
    html <- app$get_html(fe_sel)
    expect_true(any(nchar(html) > 0))
  })

  withr::defer(try(app$stop(), silent = TRUE), envir = teardown_env())
})

# ==========================================================================
# Upload minimal fixture
# ==========================================================================
describe("Mixed Effects - upload minimal fixture", {
  app <- NULL

  it("starts the app and navigates to Mixed Effects Demand", {
    app <<- create_app_driver()
    require_app(app)
    app$set_inputs(!!ids$nav := "MixedEffectsDemand")
    app$wait_for_idle(duration = 2000)
    expect_equal(app$get_value(input = ids$nav), "MixedEffectsDemand")
  })

  it("uploads mixed effects fixture data", {
    require_app(app)
    app$upload_file(
      !!ids$mixed$upload := fixture_path("mixed-effects-minimal.csv")
    )
    app$wait_for_idle(duration = 2000)
    id_var <- app$get_value(input = ids$mixed$id_var)
    expect_true(!is.null(id_var) && nchar(id_var) > 0)
  })

  it("selects drug as Factor 1 and runs model", {
    require_app(app)
    app$set_inputs(!!ids$mixed$factor1 := "drug", wait_ = FALSE)
    Sys.sleep(1)
    app$click(selector = paste0("#", ids$mixed$run))
    wait_for_calc(app, sleep_secs = 30)
    expect_true(TRUE)
  })

  it("renders model summary for uploaded data", {
    require_app(app)
    summary_sel <- paste0(
      "#", ns_id("mixed_effects_demand", "model_summary_output")
    )
    html <- app$get_html(summary_sel)
    expect_true(any(nchar(html) > 0))
  })

  withr::defer(try(app$stop(), silent = TRUE), envir = teardown_env())
})

# ==========================================================================
# Full ko dataset with 2 factors (gated)
# ==========================================================================
describe("Mixed Effects - full ko dataset with 2 factors", {
  app <- NULL

  it("uploads full ko, configures 2 factors, and runs model", {
    skip_if_not_full_tests()
    app <<- create_app_driver()
    require_app(app)
    app$set_inputs(!!ids$nav := "MixedEffectsDemand")
    app$wait_for_idle(duration = 2000)
    app$upload_file(
      !!ids$mixed$upload := example_path("shinybeez-ex-ko.csv")
    )
    app$wait_for_idle(duration = 2000)
    app$set_inputs(!!ids$mixed$factor1 := "drug", wait_ = FALSE)
    Sys.sleep(1)
    app$set_inputs(!!ids$mixed$factor2 := "dose", wait_ = FALSE)
    Sys.sleep(1)
    app$click(selector = paste0("#", ids$mixed$run))
    wait_for_calc(app, sleep_secs = 60)
    summary_sel <- paste0(
      "#", ns_id("mixed_effects_demand", "model_summary_output")
    )
    html <- app$get_html(summary_sel)
    expect_true(any(nchar(html) > 0))
  })

  withr::defer(
    if (!is.null(app)) try(app$stop(), silent = TRUE),
    envir = teardown_env()
  )
})
