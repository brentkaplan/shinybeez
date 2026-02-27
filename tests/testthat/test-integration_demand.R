# Integration tests: Demand tab workflows
# Tests upload, configuration, and calculation for all demand analysis types.

# ==========================================================================
# Pooled analysis (minimal fixture)
# ==========================================================================
describe("Demand - pooled analysis", {
  app <- NULL

  it("starts the app and navigates to Demand", {
    app <<- create_app_driver()
    require_app(app)
    app$set_inputs(!!ids$nav := "Demand")
    app$wait_for_idle(duration = 1000)
    expect_equal(app$get_value(input = ids$nav), "Demand")
  })

  it("uploads demand data", {
    require_app(app)
    app$upload_file(
      !!ids$demand$upload := fixture_path("demand-minimal.csv")
    )
    app$wait_for_idle(duration = 2000)
    html <- app$get_html(".datatables")
    expect_true(any(nchar(html) > 0))
  })

  it("renders the data table after upload", {
    require_app(app)
    html <- app$get_html(".datatables")
    expect_true(any(nchar(html) > 0))
  })

  it("configures k value", {
    require_app(app)
    # Equation defaults to "Exponentiated (with k)" — no need to set
    app$set_inputs(!!ids$demand$k := "2", wait_ = FALSE)
    Sys.sleep(1)
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
    wait_for_calc(app)
    html <- app$get_html(".datatables")
    expect_true(any(nchar(html) > 0))
  })

  withr::defer(try(app$stop(), silent = TRUE), envir = teardown_env())
})

# ==========================================================================
# Two-Stage (Individual) analysis
# ==========================================================================
describe("Demand - two-stage analysis", {
  app <- NULL

  it("starts the app and uploads data", {
    app <<- create_app_driver()
    require_app(app)
    app$set_inputs(!!ids$nav := "Demand")
    app$wait_for_idle(duration = 1000)
    app$upload_file(
      !!ids$demand$upload := fixture_path("demand-minimal.csv")
    )
    app$wait_for_idle(duration = 2000)
    expect_equal(app$get_value(input = ids$nav), "Demand")
  })

  it("selects Two Stage (Ind) analysis type and configures k", {
    require_app(app)
    app$set_inputs(!!ids$demand$analysis_type := "Ind")
    app$wait_for_idle(duration = 1000)
    app$set_inputs(!!ids$demand$k := "2", wait_ = FALSE)
    Sys.sleep(1)
    expect_equal(app$get_value(input = ids$demand$analysis_type), "Ind")
  })

  it("runs two-stage calculation", {
    require_app(app)
    app$click(selector = paste0("#", ids$demand$calculate))
    wait_for_calc(app, sleep_secs = 10)
    html <- app$get_html(".datatables")
    expect_true(any(nchar(html) > 0))
  })

  withr::defer(try(app$stop(), silent = TRUE), envir = teardown_env())
})

# ==========================================================================
# Mean analysis
# ==========================================================================
describe("Demand - mean analysis", {
  app <- NULL

  it("starts the app and uploads data", {
    app <<- create_app_driver()
    require_app(app)
    app$set_inputs(!!ids$nav := "Demand")
    app$wait_for_idle(duration = 1000)
    app$upload_file(
      !!ids$demand$upload := fixture_path("demand-minimal.csv")
    )
    app$wait_for_idle(duration = 2000)
    expect_equal(app$get_value(input = ids$nav), "Demand")
  })

  it("selects Mean analysis type and configures k", {
    require_app(app)
    app$set_inputs(!!ids$demand$analysis_type := "Mean")
    app$wait_for_idle(duration = 1000)
    app$set_inputs(!!ids$demand$k := "2", wait_ = FALSE)
    Sys.sleep(1)
    expect_equal(app$get_value(input = ids$demand$analysis_type), "Mean")
  })

  it("runs mean calculation", {
    require_app(app)
    app$click(selector = paste0("#", ids$demand$calculate))
    wait_for_calc(app)
    html <- app$get_html(".datatables")
    expect_true(any(nchar(html) > 0))
  })

  withr::defer(try(app$stop(), silent = TRUE), envir = teardown_env())
})

# ==========================================================================
# Grouped analysis
# ==========================================================================
describe("Demand - grouped analysis", {
  app <- NULL

  it("starts the app and uploads grouped data", {
    app <<- create_app_driver()
    require_app(app)
    app$set_inputs(!!ids$nav := "Demand")
    app$wait_for_idle(duration = 1000)
    app$upload_file(
      !!ids$demand$upload := fixture_path("demand-minimal-grouped.csv")
    )
    app$wait_for_idle(duration = 2000)
    expect_equal(app$get_value(input = ids$nav), "Demand")
  })

  it("enables grouping checkbox and configures k", {
    require_app(app)
    app$set_inputs(!!ids$demand$group := TRUE)
    app$wait_for_idle(duration = 1000)
    app$set_inputs(!!ids$demand$k := "2", wait_ = FALSE)
    Sys.sleep(1)
    expect_true(app$get_value(input = ids$demand$group))
  })

  it("runs grouped pooled calculation", {
    require_app(app)
    app$click(selector = paste0("#", ids$demand$calculate))
    wait_for_calc(app)
    html <- app$get_html(".datatables")
    expect_true(any(nchar(html) > 0))
  })

  withr::defer(try(app$stop(), silent = TRUE), envir = teardown_env())
})

# ==========================================================================
# Full example (gated)
# ==========================================================================
describe("Demand - full 50-subject grouped example", {
  app <- NULL

  it("uploads and runs full 50-subject pooled analysis", {
    skip_if_not_full_tests()
    app <<- create_app_driver()
    require_app(app)
    app$set_inputs(!!ids$nav := "Demand")
    app$wait_for_idle(duration = 1000)
    app$upload_file(
      !!ids$demand$upload := example_path("shinybeez-ex-apt-50.csv")
    )
    app$wait_for_idle(duration = 2000)
    app$set_inputs(!!ids$demand$group := TRUE)
    app$wait_for_idle(duration = 1000)
    app$set_inputs(!!ids$demand$k := "2", wait_ = FALSE)
    Sys.sleep(1)
    app$click(selector = paste0("#", ids$demand$calculate))
    wait_for_calc(app, sleep_secs = 20)
    html <- app$get_html(".datatables")
    expect_true(any(nchar(html) > 0))
  })

  withr::defer(
    if (!is.null(app)) try(app$stop(), silent = TRUE),
    envir = teardown_env()
  )
})
