# Integration tests: Welcome tab and navigation
# These tests verify the app boots, the welcome tab is the default,
# navigation between tabs works, and the info modal opens/closes.

describe("Welcome tab and navigation", {
  app <- NULL

  it("boots the app successfully", {
    app <<- create_app_driver()
    expect_false(is.null(app))
  })

  it("shows Welcome as the default active tab", {
    require_app(app)
    nav_val <- app$get_value(input = ids$nav)
    expect_equal(nav_val, "Welcome")
  })

  it("renders welcome content in the main panel", {
    require_app(app)
    html <- app$get_html("main")
    expect_true(any(nchar(html) > 0))
  })

  it("navigates to Demand tab", {
    require_app(app)
    navigate_to_tab(app, "Demand")
    expect_equal(app$get_value(input = ids$nav), "Demand")
  })

  it("navigates to Discounting tab", {
    require_app(app)
    navigate_to_tab(app, "Discounting")
    expect_equal(app$get_value(input = ids$nav), "Discounting")
  })

  it("navigates to Mixed Effects Demand tab", {
    require_app(app)
    navigate_to_tab(app, "MixedEffectsDemand")
    expect_equal(app$get_value(input = ids$nav), "MixedEffectsDemand")
  })

  it("navigates back to Welcome tab", {
    require_app(app)
    navigate_to_tab(app, "Welcome")
    expect_equal(app$get_value(input = ids$nav), "Welcome")
  })

  it("opens the info modal", {
    require_app(app)
    app$click(selector = paste0("#", ids$info$trigger))
    app$wait_for_js(
      "document.querySelector('.modal-dialog') !== null",
      timeout = 5000
    )
    app$wait_for_idle(duration = 500)
    modal_html <- app$get_html(".modal-dialog")
    expect_true(grepl("About shinybeez", modal_html))
  })

  it("closes the info modal", {
    require_app(app)
    app$click(selector = ".modal-footer button")
    app$wait_for_js(
      "document.querySelector('.modal-dialog') === null",
      timeout = 5000
    )
    app$wait_for_idle(duration = 500)
    modal_html <- app$get_html(".modal-dialog")
    # After closing, modal-dialog should not be present or be empty
    expect_true(is.null(modal_html) || !grepl("About shinybeez", modal_html))
  })

  withr::defer(try(app$stop(), silent = TRUE), envir = teardown_env())
})
