# Integration tests: the #23 X-variable guard, at the module wiring level.
#
# tests/testthat/test-mixed_effects_x_var_guard.R covers the data_prep helpers in
# isolation (is_numeric_like, numeric_x_candidates, choose_x_selection,
# coerce_x_numeric). Those tests cannot show that the helpers are actually wired
# in, which is where production error e84804c9 lived: a character X column
# reached beezdemand::fit_demand_mixed() raw.
#
# Two claims are checked here, one per half of the guard:
#   1. the sidebar never OFFERS a column that would be rejected at fit time
#      (mixed_effects_demand_sidebar.R:381, choices = x_candidates)
#   2. a file whose `x` column is text is stopped at upload, naming the cells
#      (validate.R, check_mixed_effects_data). Until 1.5.0 the upload passed and a
#      guard before the fit caught it; that guard is still there for a selected
#      column that is mostly, but not wholly, numeric, and is unit-tested.
#
# Both fixtures carry text where numbers are expected: the first adds a
# `price_label` column alongside a numeric `x`, the second makes `x` itself
# unconvertible.
#
# One thing deliberately not tested here: selecting an offered-but-invalid
# column via set_inputs(). That is dropped client-side, because claim 1 keeps
# such columns out of the select entirely — verified while writing these tests.

describe("Mixed Effects - X variable guard (#23)", {
  app <- NULL
  summary_id <- ns_id("mixed_effects_demand", "model_summary_structured")

  it("starts the app and navigates to Mixed Effects Demand", {
    app <<- create_app_driver()
    require_app(app)
    navigate_to_tab(app, "MixedEffectsDemand")
    expect_equal(app$get_value(input = ids$nav), "MixedEffectsDemand")
  })

  it("does not offer a non-numeric column as the X variable", {
    require_app(app)
    upload_and_wait(
      app, ids$mixed$upload, fixture_path("mixed-effects-nonnumeric-x.csv")
    )

    x_var <- app$get_value(input = ids$mixed$x_var)

    # Non-vacuous on both sides: something must be selected, and it must not be
    # the unconvertible column. Asserting only "not price_label" would pass on
    # an empty selection.
    expect_true(!is.null(x_var) && nzchar(x_var))
    expect_false(identical(x_var, "price_label"))
  })

  it("rejects the upload, naming the cells, when the x column is text", {
    require_app(app)

    # Production error e84804c9: a column literally named `x` whose values are text. The
    # upload check used to look at column names only, the app selected `x` by name, and a
    # guard in the navpanel was all that stood between this file and fit_demand_mixed().
    # The upload now stops on the cells themselves.
    app$upload_file(
      !!ids$mixed$upload := fixture_path("mixed-effects-no-numeric-x.csv")
    )
    wait_for_notification(app, "error")
    err_html <- paste(app$get_html(".shiny-notification-error"), collapse = " ")
    expect_match(err_html, "must hold one number per cell", fixed = TRUE)
    expect_match(err_html, "ten", fixed = TRUE)
    # No reshape form: it could only drop the cells it cannot read.
    expect_false(app$get_js("document.querySelector('.modal.show') !== null"))
  })

  local_app_stop()
})
