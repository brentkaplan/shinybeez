# Integration tests: Discounting tab workflows
# Tests all 4 scoring methods: MCQ 27-Item, IP Regression (Pooled & Two Stage),
# 5.5-Trial DD, 5.5-Trial PD.

# Helper: wait for calculation to complete
wait_for_calc <- function(app, sleep_secs = 5) {
  Sys.sleep(sleep_secs)
  app$wait_for_idle(duration = 2000)
}

# ==========================================================================
# MCQ 27-Item scoring (minimal fixture)
# ==========================================================================
describe("Discounting - MCQ 27-Item scoring", {
  app <- NULL

  it("starts the app, selects MCQ, and uploads data", {

    app <<- create_app_driver()
    app$set_inputs(!!ids$nav := "Discounting")
    app$wait_for_idle(duration = 1000)
    app$set_inputs(!!ids$discounting$calc_type := "27-Item MCQ")
    app$wait_for_idle(duration = 1000)
    app$upload_file(
      !!ids$discounting$upload := fixture_path("discounting-mcq-minimal.csv")
    )
    app$wait_for_idle(duration = 2000)
    expect_equal(
      app$get_value(input = ids$discounting$calc_type),
      "27-Item MCQ"
    )
  })

  it("shows the calculate button after upload", {

    html <- app$get_html(paste0("#", ids$discounting$calculate))
    expect_true(any(nchar(html) > 0))
  })

  it("configures imputation and transform", {

    app$set_inputs(!!ids$discounting$imputation := "none", wait_ = FALSE)
    app$set_inputs(!!ids$discounting$trans := "none", wait_ = FALSE)
    Sys.sleep(1)
    expect_equal(app$get_value(input = ids$discounting$imputation), "none")
  })

  it("runs MCQ calculation and renders results", {

    app$click(selector = paste0("#", ids$discounting$calculate))
    wait_for_calc(app)
    html <- app$get_html(".datatables")
    expect_true(any(nchar(html) > 0))
  })

  withr::defer(try(app$stop(), silent = TRUE), envir = teardown_env())
})

# ==========================================================================
# IP Regression - Mazur Pooled (minimal fixture)
# ==========================================================================
describe("Discounting - IP Regression Pooled", {
  app <- NULL

  it("starts the app, selects IP Regression, and uploads data", {

    app <<- create_app_driver()
    app$set_inputs(!!ids$nav := "Discounting")
    app$wait_for_idle(duration = 1000)
    app$set_inputs(
      !!ids$discounting$calc_type := "Indifference Point Regression"
    )
    app$wait_for_idle(duration = 1000)
    app$upload_file(
      !!ids$discounting$upload := fixture_path("discounting-ip-minimal.csv")
    )
    app$wait_for_idle(duration = 2000)
    expect_equal(
      app$get_value(input = ids$discounting$calc_type),
      "Indifference Point Regression"
    )
  })

  it("configures Mazur equation (defaults to Pooled)", {

    app$set_inputs(!!ids$discounting$equation := "Mazur", wait_ = FALSE)
    Sys.sleep(1)
    val <- app$get_value(input = ids$discounting$analysis_type)
    expect_equal(val, "Pooled")
  })

  it("runs pooled IP regression and renders results", {

    app$click(selector = paste0("#", ids$discounting$calculate))
    wait_for_calc(app, sleep_secs = 10)
    html <- app$get_html(".datatables")
    expect_true(any(nchar(html) > 0))
  })

  withr::defer(try(app$stop(), silent = TRUE), envir = teardown_env())
})

# ==========================================================================
# IP Regression - Two Stage (minimal fixture)
# ==========================================================================
describe("Discounting - IP Regression Two Stage", {
  app <- NULL

  it("starts the app, selects IP Regression, and uploads data", {

    app <<- create_app_driver()
    app$set_inputs(!!ids$nav := "Discounting")
    app$wait_for_idle(duration = 1000)
    app$set_inputs(
      !!ids$discounting$calc_type := "Indifference Point Regression"
    )
    app$wait_for_idle(duration = 1000)
    app$upload_file(
      !!ids$discounting$upload := fixture_path("discounting-ip-minimal.csv")
    )
    app$wait_for_idle(duration = 2000)
    expect_equal(
      app$get_value(input = ids$discounting$calc_type),
      "Indifference Point Regression"
    )
  })

  it("configures Mazur equation and Two Stage analysis", {

    app$set_inputs(!!ids$discounting$equation := "Mazur", wait_ = FALSE)
    Sys.sleep(1)
    app$set_inputs(!!ids$discounting$analysis_type := "Two Stage", wait_ = FALSE)
    Sys.sleep(1)
    expect_equal(
      app$get_value(input = ids$discounting$analysis_type),
      "Two Stage"
    )
  })

  it("runs Two Stage IP regression and renders results", {

    app$click(selector = paste0("#", ids$discounting$calculate))
    wait_for_calc(app, sleep_secs = 10)
    html <- app$get_html(".datatables")
    expect_true(any(nchar(html) > 0))
  })

  withr::defer(try(app$stop(), silent = TRUE), envir = teardown_env())
})

# ==========================================================================
# Full examples (gated)
# ==========================================================================
describe("Discounting - 5.5-Trial Delay Discounting (full)", {
  app <- NULL

  it("uploads and calculates 5.5-Trial DD", {

    skip_if_not_full_tests()
    app <<- create_app_driver()
    app$set_inputs(!!ids$nav := "Discounting")
    app$wait_for_idle(duration = 1000)
    app$set_inputs(
      !!ids$discounting$calc_type := "5.5 Trial Delay Discounting"
    )
    app$wait_for_idle(duration = 1000)
    app$upload_file(
      !!ids$discounting$upload := example_path(
        "shinybeez-ex-five.fivetrial_dd.csv"
      )
    )
    app$wait_for_idle(duration = 2000)
    app$click(selector = paste0("#", ids$discounting$calculate))
    wait_for_calc(app, sleep_secs = 15)
    html <- app$get_html(".datatables")
    expect_true(any(nchar(html) > 0))
  })

  withr::defer(
    if (!is.null(app)) try(app$stop(), silent = TRUE),
    envir = teardown_env()
  )
})

describe("Discounting - 5.5-Trial Probability Discounting (full)", {
  app <- NULL

  it("uploads and calculates 5.5-Trial PD", {

    skip_if_not_full_tests()
    app <<- create_app_driver()
    app$set_inputs(!!ids$nav := "Discounting")
    app$wait_for_idle(duration = 1000)
    app$set_inputs(
      !!ids$discounting$calc_type := "5.5 Trial Probability Discounting"
    )
    app$wait_for_idle(duration = 1000)
    app$upload_file(
      !!ids$discounting$upload := example_path(
        "shinybeez-ex-five.fivetrial_pd.csv"
      )
    )
    app$wait_for_idle(duration = 2000)
    app$click(selector = paste0("#", ids$discounting$calculate))
    wait_for_calc(app, sleep_secs = 15)
    html <- app$get_html(".datatables")
    expect_true(any(nchar(html) > 0))
  })

  withr::defer(
    if (!is.null(app)) try(app$stop(), silent = TRUE),
    envir = teardown_env()
  )
})

describe("Discounting - MCQ with GGM imputation (full)", {
  app <- NULL

  it("uploads MCQ with missings, applies GGM, and calculates", {

    skip_if_not_full_tests()
    app <<- create_app_driver()
    app$set_inputs(!!ids$nav := "Discounting")
    app$wait_for_idle(duration = 1000)
    app$set_inputs(!!ids$discounting$calc_type := "27-Item MCQ")
    app$wait_for_idle(duration = 1000)
    app$upload_file(
      !!ids$discounting$upload := example_path(
        "shinybeez-ex-mcq-100-missings.csv"
      )
    )
    app$wait_for_idle(duration = 2000)
    app$set_inputs(!!ids$discounting$imputation := "GGM", wait_ = FALSE)
    app$set_inputs(!!ids$discounting$trans := "log", wait_ = FALSE)
    Sys.sleep(1)
    app$click(selector = paste0("#", ids$discounting$calculate))
    wait_for_calc(app, sleep_secs = 15)
    html <- app$get_html(".datatables")
    expect_true(any(nchar(html) > 0))
  })

  withr::defer(
    if (!is.null(app)) try(app$stop(), silent = TRUE),
    envir = teardown_env()
  )
})
