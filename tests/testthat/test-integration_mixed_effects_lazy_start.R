# Integration tests: the Mixed Effects tab does no work until it is opened.
#
# The ME sidebar falls back to the example `ko` dataset when nothing is uploaded,
# and that used to happen at connect: every fresh session loaded ko, pushed the
# id/x/y select choices (three telemetry `input` rows) and computed systematic
# criteria, which showed a "Computing systematic criteria..." toast over the
# Welcome page. Under 1% of production sessions ever open the ME tab.
#
# The x-variable select is the observable signal for Task 1: it is filled only
# by the observer that runs on ko, so it stays empty until the first visit.

describe("Mixed Effects - lazy start", {
  app <- NULL

  it("does not load the example data on the Welcome page", {
    app <<- create_app_driver()
    require_app(app)
    app$wait_for_idle(duration = 1000)

    x <- app$get_value(input = ids$mixed$x_var)
    expect_true(is.null(x) || !nzchar(x))
    expect_length(app$get_html(".shiny-notification-error"), 0)
  })

  it("populates the variable selects on first visit", {
    require_app(app)
    navigate_to_tab(app, "MixedEffectsDemand")
    app$wait_for_value(input = ids$mixed$x_var, ignore = list(NULL, ""))

    expect_true(nzchar(app$get_value(input = ids$mixed$x_var)))
    expect_true(nzchar(app$get_value(input = ids$mixed$id_var)))
    expect_true(nzchar(app$get_value(input = ids$mixed$y_var)))
  })

  it("keeps the selections after leaving the tab and coming back", {
    require_app(app)
    before <- lapply(ids$mixed[c("id_var", "x_var", "y_var")], function(id) {
      app$get_value(input = id)
    })

    navigate_to_tab(app, "Welcome")
    navigate_to_tab(app, "MixedEffectsDemand")

    after <- lapply(ids$mixed[c("id_var", "x_var", "y_var")], function(id) {
      app$get_value(input = id)
    })
    expect_identical(after, before)
  })

  local_app_stop()
})
