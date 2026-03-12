# Integration tests: Mixed Effects Demand tab workflows
# Tests default ko data auto-load, upload minimal fixture, and full examples.

# ==========================================================================
# Default ko data (auto-loaded on tab navigation)
# ==========================================================================
describe("Mixed Effects - default ko data", {
  app <- NULL
  summary_id <- ns_id("mixed_effects_demand", "model_summary_structured")

  it("starts the app and navigates to Mixed Effects Demand", {
    app <<- create_app_driver()
    require_app(app)
    navigate_to_tab(app, "MixedEffectsDemand")
    expect_equal(app$get_value(input = ids$nav), "MixedEffectsDemand")
  })

  it("auto-loads default data and populates variable dropdowns", {
    require_app(app)
    # Wait for the auto-loaded data to populate dropdowns
    wait_for_input(app, ids$mixed$id_var)
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
    app$wait_for_idle(duration = 500)
    expect_equal(app$get_value(input = ids$mixed$factor1), "drug")
  })

  it("runs the mixed effects model", {
    require_app(app)
    app$click(selector = paste0("#", ids$mixed$run))
    wait_for_output(app, summary_id, timeout_ms = 60000)
    err_html <- app$get_html(".shiny-notification-error")
    expect_true(is.null(err_html) || !grepl("Error", err_html))
  })

  it("renders model summary output", {
    require_app(app)
    html <- app$get_html(paste0("#", summary_id))
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
  summary_id <- ns_id("mixed_effects_demand", "model_summary_structured")

  it("starts the app and navigates to Mixed Effects Demand", {
    app <<- create_app_driver()
    require_app(app)
    navigate_to_tab(app, "MixedEffectsDemand")
    expect_equal(app$get_value(input = ids$nav), "MixedEffectsDemand")
  })

  it("uploads mixed effects fixture data", {
    require_app(app)
    upload_and_wait(
      app, ids$mixed$upload, fixture_path("mixed-effects-minimal.csv")
    )
    id_var <- app$get_value(input = ids$mixed$id_var)
    expect_true(!is.null(id_var) && nchar(id_var) > 0)
  })

  it("selects drug as Factor 1 and runs model", {
    require_app(app)
    app$set_inputs(!!ids$mixed$factor1 := "drug", wait_ = FALSE)
    app$wait_for_idle(duration = 500)
    app$click(selector = paste0("#", ids$mixed$run))
    wait_for_output(app, summary_id, timeout_ms = 60000)
    err_html <- app$get_html(".shiny-notification-error")
    expect_true(is.null(err_html) || !grepl("Error", err_html))
  })

  it("renders model summary for uploaded data", {
    require_app(app)
    html <- app$get_html(paste0("#", summary_id))
    expect_true(any(nchar(html) > 0))
  })

  withr::defer(try(app$stop(), silent = TRUE), envir = teardown_env())
})

# ==========================================================================
# Full ko dataset with 2 factors (gated)
# ==========================================================================
describe("Mixed Effects - full ko dataset with 2 factors", {
  app <- NULL
  summary_id <- ns_id("mixed_effects_demand", "model_summary_structured")

  it("uploads full ko, configures 2 factors, and runs model", {
    skip_if_not_full_tests()
    app <<- create_app_driver()
    require_app(app)
    navigate_to_tab(app, "MixedEffectsDemand")
    upload_and_wait(
      app, ids$mixed$upload, example_path("shinybeez-ex-ko.csv")
    )
    app$set_inputs(!!ids$mixed$factor1 := "drug", wait_ = FALSE)
    app$wait_for_idle(duration = 500)
    app$set_inputs(!!ids$mixed$factor2 := "dose", wait_ = FALSE)
    app$wait_for_idle(duration = 500)
    app$click(selector = paste0("#", ids$mixed$run))
    wait_for_output(app, summary_id, timeout_ms = 120000)
    html <- app$get_html(paste0("#", summary_id))
    expect_true(any(nchar(html) > 0))
  })

  withr::defer(
    if (!is.null(app)) try(app$stop(), silent = TRUE),
    envir = teardown_env()
  )
})
