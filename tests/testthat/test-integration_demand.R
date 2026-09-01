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

  it("runs pooled calculation and renders results with demand columns", {
    require_app(app)
    app$click(selector = paste0("#", ids$demand$calculate))
    wait_for_output(app, result_id, timeout_ms = 15000)
    # Pooled fits a single aggregate curve -> exactly 1 row.
    expect_demand_results(app, result_id, n_rows = 1)
  })

  it("switches to Two Stage and runs calculation", {
    require_app(app)
    app$set_inputs(!!ids$demand$analysis_type := "Ind")
    app$wait_for_idle(duration = 500)
    expect_equal(app$get_value(input = ids$demand$analysis_type), "Ind")
    app$click(selector = paste0("#", ids$demand$calculate))
    wait_for_output(app, result_id, timeout_ms = 30000)
    # Two Stage fits per participant -> 3 rows for this fixture. The differing
    # count also proves the table refreshed rather than showing pooled results.
    expect_demand_results(app, result_id, n_rows = 3)
  })

  it("switches to Mean and runs calculation", {
    require_app(app)
    app$set_inputs(!!ids$demand$analysis_type := "Mean")
    app$wait_for_idle(duration = 500)
    expect_equal(app$get_value(input = ids$demand$analysis_type), "Mean")
    app$click(selector = paste0("#", ids$demand$calculate))
    wait_for_output(app, result_id, timeout_ms = 15000)
    # Mean collapses to a single averaged curve -> back to 1 row, which also
    # distinguishes this from the 3-row Two Stage table rendered just before.
    expect_demand_results(app, result_id, n_rows = 1)
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
    # Grouped pooled fits one curve per group; this fixture has groups A and B.
    expect_demand_results(app, result_id, n_rows = 2)
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
    # shinybeez-ex-apt-50.csv has groups A, B and C -> one curve per group.
    expect_demand_results(app, result_id, n_rows = 3)
  })

  withr::defer(
    if (!is.null(app)) try(app$stop(), silent = TRUE),
    envir = teardown_env()
  )
})

# ==========================================================================
# Async fit (ExtendedTask)
# ==========================================================================
# The task button is disabled exactly while its ExtendedTask is busy
# (bslib components.js: `el.disabled = state === "busy"`), so "enabled again"
# is the end-of-fit signal. A plain actionButton is never disabled, so this
# journey's JS wait returns immediately there and the results assertion fires
# before the blocking fit has rendered anything.
describe("Demand - async fit", {
  it("shows a busy Run button that clears when the fit completes", {
    result_id <- ns_id("demand", "results_table_demand", "model_results_table")
    app <- create_app_driver()
    on.exit(try(app$stop(), silent = TRUE), add = TRUE)
    require_app(app)
    navigate_to_tab(app, "Demand")
    upload_and_wait(app, ids$demand$upload, fixture_path("demand-minimal.csv"))
    wait_for_input(app, ids$demand$calculate)
    app$click(selector = paste0("#", ids$demand$calculate))
    app$wait_for_js(
      sprintf(
        paste0(
          "(function(){var b=document.getElementById('%s');",
          "return !!b && !b.disabled;})()"
        ),
        ids$demand$calculate
      ),
      timeout = 60000
    )
    expect_demand_results(app, result_id, min_rows = 1L)
  })
})
