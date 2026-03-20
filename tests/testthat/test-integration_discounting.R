# Integration tests: Discounting tab workflows
# Tests all 4 scoring methods: MCQ 27-Item, IP Regression (Pooled & Two Stage),
# 5.5-Trial DD, 5.5-Trial PD.

# ==========================================================================
# MCQ 27-Item scoring (minimal fixture)
# ==========================================================================
describe("Discounting - MCQ 27-Item scoring", {
  app <- NULL
  result_id <- ns_id("discounting", "results_table_discounting", "results_table")

  it("starts the app, selects MCQ, and uploads data", {
    app <<- create_app_driver()
    require_app(app)
    navigate_to_tab(app, "Discounting")
    app$set_inputs(!!ids$discounting$calc_type := "27-Item MCQ")
    app$wait_for_idle(duration = 500)
    upload_and_wait(
      app, ids$discounting$upload,
      fixture_path("discounting-mcq-minimal.csv")
    )
    expect_equal(
      app$get_value(input = ids$discounting$calc_type),
      "27-Item MCQ"
    )
  })

  it("shows the calculate button after upload", {
    require_app(app)
    html <- app$get_html(paste0("#", ids$discounting$calculate))
    expect_true(any(nchar(html) > 0))
  })

  it("configures imputation and transform", {
    require_app(app)
    app$set_inputs(!!ids$discounting$imputation := "none", wait_ = FALSE)
    app$set_inputs(!!ids$discounting$trans := "none", wait_ = FALSE)
    app$wait_for_idle(duration = 500)
    expect_equal(app$get_value(input = ids$discounting$imputation), "none")
  })

  it("runs MCQ calculation and renders results", {
    require_app(app)
    app$click(selector = paste0("#", ids$discounting$calculate))
    wait_for_output(app, result_id, timeout_ms = 15000)
    html <- app$get_html(".datatables")
    expect_true(any(grepl("<td", html, fixed = TRUE)))
  })

  withr::defer(try(app$stop(), silent = TRUE), envir = teardown_env())
})

# ==========================================================================
# IP Regression - Pooled and Two Stage (shared fixture: discounting-ip-minimal)
# ==========================================================================
describe("Discounting - IP Regression Pooled and Two Stage", {
  app <- NULL
  result_id <- ns_id("discounting", "results_table_discounting", "results_table")

  it("starts the app, selects IP Regression, and uploads data", {
    app <<- create_app_driver()
    require_app(app)
    navigate_to_tab(app, "Discounting")
    app$set_inputs(
      !!ids$discounting$calc_type := "Indifference Point Regression"
    )
    app$wait_for_idle(duration = 500)
    upload_and_wait(
      app, ids$discounting$upload,
      fixture_path("discounting-ip-minimal.csv")
    )
    expect_equal(
      app$get_value(input = ids$discounting$calc_type),
      "Indifference Point Regression"
    )
  })

  it("configures Mazur equation (defaults to Pooled)", {
    require_app(app)
    app$set_inputs(!!ids$discounting$equation := "Mazur", wait_ = FALSE)
    app$wait_for_idle(duration = 500)
    val <- app$get_value(input = ids$discounting$analysis_type)
    expect_equal(val, "Pooled")
  })

  it("runs pooled IP regression and renders results", {
    require_app(app)
    app$click(selector = paste0("#", ids$discounting$calculate))
    wait_for_output(app, result_id, timeout_ms = 30000)
    html <- app$get_html(".datatables")
    expect_true(any(grepl("<td", html, fixed = TRUE)))
  })

  it("switches to Two Stage and runs calculation", {
    require_app(app)
    app$set_inputs(
      !!ids$discounting$analysis_type := "Two Stage", wait_ = FALSE
    )
    app$wait_for_idle(duration = 500)
    expect_equal(
      app$get_value(input = ids$discounting$analysis_type),
      "Two Stage"
    )
    app$click(selector = paste0("#", ids$discounting$calculate))
    wait_for_output(app, result_id, timeout_ms = 30000)
    html <- app$get_html(".datatables")
    expect_true(any(grepl("<td", html, fixed = TRUE)))
  })

  withr::defer(try(app$stop(), silent = TRUE), envir = teardown_env())
})

# ==========================================================================
# Full examples (gated)
# ==========================================================================
describe("Discounting - 5.5-Trial Delay Discounting (full)", {
  app <- NULL
  result_id <- ns_id("discounting", "results_table_discounting", "results_table")

  it("uploads and calculates 5.5-Trial DD", {
    skip_if_not_full_tests()
    app <<- create_app_driver()
    require_app(app)
    navigate_to_tab(app, "Discounting")
    app$set_inputs(
      !!ids$discounting$calc_type := "5.5 Trial Delay Discounting"
    )
    app$wait_for_idle(duration = 500)
    app$upload_file(
      !!ids$discounting$upload := example_path("shinybeez-ex-five.fivetrial_dd.csv")
    )
    app$wait_for_idle(duration = 2000, timeout = 30000)
    app$click(selector = paste0("#", ids$discounting$calculate))
    wait_for_output(app, result_id, timeout_ms = 45000)
    html <- app$get_html(".datatables")
    expect_true(any(grepl("<td", html, fixed = TRUE)))
  })

  withr::defer(
    if (!is.null(app)) try(app$stop(), silent = TRUE),
    envir = teardown_env()
  )
})

describe("Discounting - 5.5-Trial Probability Discounting (full)", {
  app <- NULL
  result_id <- ns_id("discounting", "results_table_discounting", "results_table")

  it("uploads and calculates 5.5-Trial PD", {
    skip_if_not_full_tests()
    app <<- create_app_driver()
    require_app(app)
    navigate_to_tab(app, "Discounting")
    app$set_inputs(
      !!ids$discounting$calc_type := "5.5 Trial Probability Discounting"
    )
    app$wait_for_idle(duration = 500)
    app$upload_file(
      !!ids$discounting$upload := example_path("shinybeez-ex-five.fivetrial_pd.csv")
    )
    app$wait_for_idle(duration = 2000, timeout = 30000)
    app$click(selector = paste0("#", ids$discounting$calculate))
    wait_for_output(app, result_id, timeout_ms = 45000)
    html <- app$get_html(".datatables")
    expect_true(any(grepl("<td", html, fixed = TRUE)))
  })

  withr::defer(
    if (!is.null(app)) try(app$stop(), silent = TRUE),
    envir = teardown_env()
  )
})

describe("Discounting - MCQ with GGM imputation (full)", {
  app <- NULL
  result_id <- ns_id("discounting", "results_table_discounting", "results_table")

  it("uploads MCQ with missings, applies GGM, and calculates", {
    skip_if_not_full_tests()
    app <<- create_app_driver()
    require_app(app)
    navigate_to_tab(app, "Discounting")
    app$set_inputs(!!ids$discounting$calc_type := "27-Item MCQ")
    app$wait_for_idle(duration = 500)
    upload_and_wait(
      app, ids$discounting$upload,
      example_path("shinybeez-ex-mcq-100-missings.csv")
    )
    app$set_inputs(!!ids$discounting$imputation := "GGM", wait_ = FALSE)
    app$set_inputs(!!ids$discounting$trans := "log", wait_ = FALSE)
    app$wait_for_idle(duration = 500)
    app$click(selector = paste0("#", ids$discounting$calculate))
    wait_for_output(app, result_id, timeout_ms = 45000)
    html <- app$get_html(".datatables")
    expect_true(any(grepl("<td", html, fixed = TRUE)))
  })

  withr::defer(
    if (!is.null(app)) try(app$stop(), silent = TRUE),
    envir = teardown_env()
  )
})
