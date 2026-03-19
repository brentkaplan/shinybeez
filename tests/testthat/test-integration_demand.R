# Integration tests: Demand tab workflows
# Tests upload, configuration, and calculation for all demand analysis types.

# ==========================================================================
# Pooled, Two-Stage, and Mean analysis (shared fixture: demand-minimal.csv)
# ==========================================================================
describe("Demand - pooled, two-stage, and mean analysis", {
  app <- NULL
  result_id <- ns_id("demand", "results_table_demand", "model_results_table")

  it("starts the app and navigates to Demand", {
    app <<- create_app_driver()
    require_app(app)
    navigate_to_tab(app, "Demand")
    expect_equal(app$get_value(input = ids$nav), "Demand")
  })

  it("uploads demand data", {
    require_app(app)
    upload_and_wait(app, ids$demand$upload, fixture_path("demand-minimal.csv"))
    html <- app$get_html(".datatables")
    expect_true(any(grepl("<td", html, fixed = TRUE)))
  })

  it("renders the data table after upload", {
    require_app(app)
    html <- app$get_html(".datatables")
    expect_true(any(grepl("<td", html, fixed = TRUE)))
  })

  it("configures k value", {
    require_app(app)
    app$set_inputs(!!ids$demand$k := "2", wait_ = FALSE)
    app$wait_for_idle(duration = 500)
    expect_equal(app$get_value(input = ids$demand$k), "2")
  })

  it("defaults to Pooled analysis type", {
    require_app(app)
    val <- app$get_value(input = ids$demand$analysis_type)
    expect_equal(val, "Pooled")
  })

  it("shows the calculate button after data upload", {
    require_app(app)
    html <- app$get_html(paste0("#", ids$demand$calculate))
    expect_true(any(nchar(html) > 0))
  })

  it("runs pooled calculation and renders results", {
    require_app(app)
    app$click(selector = paste0("#", ids$demand$calculate))
    wait_for_output(app, result_id, timeout_ms = 15000)
    html <- app$get_html(".datatables")
    expect_true(any(grepl("<td", html, fixed = TRUE)))
  })

  it("switches to Two Stage and runs calculation", {
    require_app(app)
    app$set_inputs(!!ids$demand$analysis_type := "Ind")
    app$wait_for_idle(duration = 500)
    expect_equal(app$get_value(input = ids$demand$analysis_type), "Ind")
    app$click(selector = paste0("#", ids$demand$calculate))
    wait_for_output(app, result_id, timeout_ms = 30000)
    html <- app$get_html(".datatables")
    expect_true(any(grepl("<td", html, fixed = TRUE)))
  })

  it("switches to Mean and runs calculation", {
    require_app(app)
    app$set_inputs(!!ids$demand$analysis_type := "Mean")
    app$wait_for_idle(duration = 500)
    expect_equal(app$get_value(input = ids$demand$analysis_type), "Mean")
    app$click(selector = paste0("#", ids$demand$calculate))
    wait_for_output(app, result_id, timeout_ms = 15000)
    html <- app$get_html(".datatables")
    expect_true(any(grepl("<td", html, fixed = TRUE)))
  })

  withr::defer(try(app$stop(), silent = TRUE), envir = teardown_env())
})

# ==========================================================================
# Grouped analysis
# ==========================================================================
describe("Demand - grouped analysis", {
  app <- NULL
  result_id <- ns_id("demand", "results_table_demand", "model_results_table")

  it("starts the app and uploads grouped data", {
    app <<- create_app_driver()
    require_app(app)
    navigate_to_tab(app, "Demand")
    upload_and_wait(
      app, ids$demand$upload, fixture_path("demand-minimal-grouped.csv")
    )
    expect_equal(app$get_value(input = ids$nav), "Demand")
  })

  it("enables grouping checkbox and configures k", {
    require_app(app)
    app$set_inputs(!!ids$demand$group := TRUE)
    app$wait_for_idle(duration = 500)
    app$set_inputs(!!ids$demand$k := "2", wait_ = FALSE)
    app$wait_for_idle(duration = 500)
    expect_true(app$get_value(input = ids$demand$group))
  })

  it("runs grouped pooled calculation", {
    require_app(app)
    app$click(selector = paste0("#", ids$demand$calculate))
    wait_for_output(app, result_id, timeout_ms = 15000)
    html <- app$get_html(".datatables")
    expect_true(any(grepl("<td", html, fixed = TRUE)))
  })

  withr::defer(try(app$stop(), silent = TRUE), envir = teardown_env())
})

# ==========================================================================
# Full example (gated)
# ==========================================================================
describe("Demand - full 50-subject grouped example", {
  app <- NULL
  result_id <- ns_id("demand", "results_table_demand", "model_results_table")

  it("uploads and runs full 50-subject pooled analysis", {
    skip_if_not_full_tests()
    app <<- create_app_driver()
    require_app(app)
    navigate_to_tab(app, "Demand")
    upload_and_wait(
      app, ids$demand$upload, example_path("shinybeez-ex-apt-50.csv")
    )
    app$set_inputs(!!ids$demand$group := TRUE)
    app$wait_for_idle(duration = 500)
    app$set_inputs(!!ids$demand$k := "2", wait_ = FALSE)
    app$wait_for_idle(duration = 500)
    app$click(selector = paste0("#", ids$demand$calculate))
    wait_for_output(app, result_id, timeout_ms = 60000)
    html <- app$get_html(".datatables")
    expect_true(any(grepl("<td", html, fixed = TRUE)))
  })

  withr::defer(
    if (!is.null(app)) try(app$stop(), silent = TRUE),
    envir = teardown_env()
  )
})
