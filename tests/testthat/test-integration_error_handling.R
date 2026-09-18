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
    expect_true(!is.null(html) && any(grepl("shiny-notification", html)))
  })

  it("opens the reshape modal when uploading grouped demand data to the discounting tab, and cancel shows the error", {
    require_app(app)
    navigate_to_tab(app, "Discounting")
    app$upload_file(
      !!ids$discounting$upload := fixture_path("demand-minimal-grouped.csv")
    )
    app$wait_for_js("document.querySelector('.modal.show') !== null", timeout = 10000)
    # This is the first (and only, in this session) upload to the Discounting tab, so
    # file_input.R's per-tab token counter (bumped on every upload) is 1 here; every modal
    # input id carries that token (see rid() in app/view/wide_mapper.R).
    cancel_id <- ns_id("discounting", "discounting", "mapper", "r1_cancel")
    app$wait_for_js(sprintf("document.getElementById('%s') !== null", cancel_id), timeout = 10000)
    app$click(selector = paste0("#", cancel_id))
    wait_for_notification(app, "error")
    html <- app$get_html(".shiny-notification-error")
    expect_true(!is.null(html) && any(grepl("shiny-notification", html)))
  })

  it("shows error when uploading MCQ data to mixed effects tab", {
    require_app(app)
    navigate_to_tab(app, "MixedEffectsDemand")
    app$upload_file(
      !!ids$mixed$upload := fixture_path("discounting-mcq-minimal.csv")
    )
    wait_for_notification(app, "error")
    html <- app$get_html(".shiny-notification-error")
    expect_true(!is.null(html) && any(grepl("shiny-notification", html)))
  })

  local_app_stop()
})
