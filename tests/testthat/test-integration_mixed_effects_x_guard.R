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
#   2. a non-numeric X that DOES get selected is stopped before the fit, with an
#      actionable message rather than beezdemand's raw error
#      (mixed_effects_demand_navpanel.R:735)
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

  it("blocks the fit and explains why when the x column is not numeric", {
    require_app(app)

    # This is production error e84804c9 itself: a column literally named `x`
    # whose values are text. Upload validation passes (it checks for the name),
    # and the app selects it by name, so the navpanel guard is the only thing
    # standing between this file and fit_demand_mixed().
    upload_and_wait(
      app, ids$mixed$upload, fixture_path("mixed-effects-no-numeric-x.csv")
    )
    expect_identical(app$get_value(input = ids$mixed$x_var), "x")

    app$click(selector = paste0("#", ids$mixed$run))
    app$wait_for_idle(duration = 3000)

    err_html <- paste(app$get_html(".shiny-notification-error"), collapse = " ")
    # The guard's own message, naming the column and the bad-value count,
    # rather than beezdemand's raw error.
    expect_match(err_html, "'x' is not numeric", fixed = TRUE)
    expect_match(err_html, "30 value(s) could not be read", fixed = TRUE)

    # And the fit must not have produced a model summary.
    summary_html <- paste(
      app$get_html(paste0("#", summary_id)) %||% "", collapse = " "
    )
    expect_false(grepl("Fixed Effects", summary_html, fixed = TRUE))
  })

  local_app_stop()
})
