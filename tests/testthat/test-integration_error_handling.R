# Integration tests: Error handling across tabs
# Verifies that uploading wrong-format data shows error notifications,
# and that calculate buttons don't appear before data upload.

# ==========================================================================
# No data before calculate
# ==========================================================================
describe("Error handling - calculate button hidden without data", {
  app <- NULL

  it("starts the app", {
    app <<- create_app_driver()
    expect_false(is.null(app))
  })

  it("does not show demand calculate button before upload", {
    app$set_inputs(!!ids$nav := "Demand")
    app$wait_for_idle(duration = 1000)
    # The calculate button is rendered via renderUI only after data upload
    html <- app$get_html(paste0("#", ids$demand$calculate))
    expect_true(is.null(html) || !grepl("calculate_demand", html))
  })

  it("does not show discounting calculate button before upload", {
    app$set_inputs(!!ids$nav := "Discounting")
    app$wait_for_idle(duration = 1000)
    html <- app$get_html(paste0("#", ids$discounting$calculate))
    expect_true(is.null(html) || !grepl("calculate_discounting", html))
  })

  withr::defer(try(app$stop(), silent = TRUE), envir = teardown_env())
})

# ==========================================================================
# Wrong format data for demand tab
# ==========================================================================
describe("Error handling - wrong format for demand", {
  app <- NULL

  it("shows error when uploading MCQ data to demand tab", {
    app <<- create_app_driver()
    app$set_inputs(!!ids$nav := "Demand")
    app$wait_for_idle(duration = 1000)
    app$upload_file(
      !!ids$demand$upload := fixture_path("discounting-mcq-minimal.csv")
    )
    Sys.sleep(3)
    app$wait_for_idle(duration = 1000)
    html <- app$get_html(".shiny-notification-error")
    expect_true(!is.null(html) && any(nchar(html) > 0))
  })

  withr::defer(try(app$stop(), silent = TRUE), envir = teardown_env())
})

# ==========================================================================
# Wrong format data for discounting tab
# ==========================================================================
describe("Error handling - wrong format for discounting", {
  app <- NULL

  it("shows error when uploading demand data to discounting tab", {
    app <<- create_app_driver()
    app$set_inputs(!!ids$nav := "Discounting")
    app$wait_for_idle(duration = 1000)
    app$upload_file(
      !!ids$discounting$upload := fixture_path("demand-minimal.csv")
    )
    Sys.sleep(3)
    app$wait_for_idle(duration = 1000)
    html <- app$get_html(".shiny-notification-error")
    expect_true(!is.null(html) && any(nchar(html) > 0))
  })

  withr::defer(try(app$stop(), silent = TRUE), envir = teardown_env())
})

# ==========================================================================
# Wrong format data for mixed effects tab
# ==========================================================================
describe("Error handling - wrong format for mixed effects", {
  app <- NULL

  it("shows error when uploading MCQ data to mixed effects tab", {
    app <<- create_app_driver()
    app$set_inputs(!!ids$nav := "MixedEffectsDemand")
    app$wait_for_idle(duration = 2000)
    app$upload_file(
      !!ids$mixed$upload := fixture_path("discounting-mcq-minimal.csv")
    )
    Sys.sleep(3)
    app$wait_for_idle(duration = 1000)
    html <- app$get_html(".shiny-notification-error")
    expect_true(!is.null(html) && any(nchar(html) > 0))
  })

  withr::defer(try(app$stop(), silent = TRUE), envir = teardown_env())
})
