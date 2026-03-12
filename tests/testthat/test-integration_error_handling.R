# Integration tests: Error handling across tabs
# Verifies that uploading wrong-format data shows error notifications,
# and that calculate buttons don't appear before data upload.

describe("Error handling", {
  app <- NULL

  it("starts the app", {
    app <<- create_app_driver()
    expect_false(is.null(app))
  })

  it("does not show demand calculate button before upload", {
    require_app(app)
    navigate_to_tab(app, "Demand")
    # The calculate button is rendered via renderUI only after data upload
    html <- app$get_html(paste0("#", ids$demand$calculate))
    expect_true(is.null(html) || !grepl("calculate_demand", html))
  })

  it("does not show discounting calculate button before upload", {
    require_app(app)
    navigate_to_tab(app, "Discounting")
    html <- app$get_html(paste0("#", ids$discounting$calculate))
    expect_true(is.null(html) || !grepl("calculate_discounting", html))
  })

  it("shows error when uploading MCQ data to demand tab", {
    require_app(app)
    navigate_to_tab(app, "Demand")
    app$upload_file(
      !!ids$demand$upload := fixture_path("discounting-mcq-minimal.csv")
    )
    wait_for_notification(app, "error")
    html <- app$get_html(".shiny-notification-error")
    expect_true(!is.null(html) && any(nchar(html) > 0))
  })

  it("shows error when uploading demand data to discounting tab", {
    require_app(app)
    navigate_to_tab(app, "Discounting")
    app$upload_file(
      !!ids$discounting$upload := fixture_path("demand-minimal.csv")
    )
    wait_for_notification(app, "error")
    html <- app$get_html(".shiny-notification-error")
    expect_true(!is.null(html) && any(nchar(html) > 0))
  })

  it("shows error when uploading MCQ data to mixed effects tab", {
    require_app(app)
    navigate_to_tab(app, "MixedEffectsDemand")
    app$upload_file(
      !!ids$mixed$upload := fixture_path("discounting-mcq-minimal.csv")
    )
    wait_for_notification(app, "error")
    html <- app$get_html(".shiny-notification-error")
    expect_true(!is.null(html) && any(nchar(html) > 0))
  })

  withr::defer(try(app$stop(), silent = TRUE), envir = teardown_env())
})
