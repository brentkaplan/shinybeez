# Integration tests: Mixed Effects Demand tab workflows
# Tests default ko data auto-load, upload minimal fixture, and full examples.

# ==========================================================================
# Default ko data (auto-loaded on tab navigation)
# ==========================================================================
describe("Mixed Effects - default ko data", {
  app <- NULL
  summary_id <- ns_id("mixed_effects_demand", "model_summary_structured")

  it("starts the app and navigates to Mixed Effects Demand", {
    app <<- create_app_driver()
    require_app(app)
    navigate_to_tab(app, "MixedEffectsDemand")
    expect_equal(app$get_value(input = ids$nav), "MixedEffectsDemand")
  })

  it("auto-loads default data and populates variable dropdowns", {
    require_app(app)
    # Wait for the auto-loaded data to populate dropdowns
    wait_for_input(app, ids$mixed$id_var)
    id_var <- app$get_value(input = ids$mixed$id_var)
    x_var <- app$get_value(input = ids$mixed$x_var)
    y_var <- app$get_value(input = ids$mixed$y_var)

    expect_true(!is.null(id_var) && nchar(id_var) > 0)
    expect_true(!is.null(x_var) && nchar(x_var) > 0)
    expect_true(!is.null(y_var) && nchar(y_var) > 0)
  })

  it("renders the data table for default data", {
    require_app(app)
    html <- app$get_html(".datatables")
    expect_true(any(grepl("<td", html, fixed = TRUE)))
  })

  it("selects drug as Factor 1", {
    require_app(app)
    app$set_inputs(!!ids$mixed$factor1 := "drug", wait_ = FALSE)
    app$wait_for_idle(duration = 500)
    expect_equal(app$get_value(input = ids$mixed$factor1), "drug")
  })

  it("runs the mixed effects model", {
    require_app(app)
    app$click(selector = paste0("#", ids$mixed$run))
    wait_for_output(app, summary_id, timeout_ms = 60000)
    err_html <- app$get_html(".shiny-notification-error")
    expect_true(is.null(err_html) || !grepl("Error", err_html))
  })

  it("renders model summary output", {
    require_app(app)
    html <- app$get_html(paste0("#", summary_id))
    # nchar() > 0 also passes on Shiny error markup, so assert the summary
    # actually rendered and that it is not an error surface.
    expect_true(
      any(grepl("Model Specification", html, fixed = TRUE)),
      info = "expected the Model Specification card in the model summary"
    )
    expect_false(any(grepl("shiny-output-error", html, fixed = TRUE)))
  })

  it("renders fixed effects table", {
    require_app(app)
    tabs_id <- ns_id("mixed_effects_demand", "results_display_tabs")
    fe_id <- ns_id("mixed_effects_demand", "fixed_effects_table")
    # The table only draws once its results tab is active. The previous
    # `nchar(html) > 0` check passed against the empty DTOutput placeholder
    # without ever activating the tab, so it never tested what it claimed to.
    app$set_inputs(!!tabs_id := "Fixed Effects")
    app$wait_for_js(
      sprintf("document.querySelector('#%s tbody tr') !== null", fe_id),
      timeout = 30000
    )
    app$wait_for_idle(duration = 500, timeout = 30000)
    expect_results_table(app, fe_id)
  })

  local_app_stop()
})

# ==========================================================================
# Upload minimal fixture
# ==========================================================================
describe("Mixed Effects - upload minimal fixture", {
  app <- NULL
  summary_id <- ns_id("mixed_effects_demand", "model_summary_structured")

  it("starts the app and navigates to Mixed Effects Demand", {
    app <<- create_app_driver()
    require_app(app)
    navigate_to_tab(app, "MixedEffectsDemand")
    expect_equal(app$get_value(input = ids$nav), "MixedEffectsDemand")
  })

  it("uploads mixed effects fixture data", {
    require_app(app)
    upload_and_wait(
      app, ids$mixed$upload, fixture_path("mixed-effects-minimal.csv")
    )
    id_var <- app$get_value(input = ids$mixed$id_var)
    expect_true(!is.null(id_var) && nchar(id_var) > 0)
  })

  it("selects drug as Factor 1 and runs model", {
    require_app(app)
    app$set_inputs(!!ids$mixed$factor1 := "drug", wait_ = FALSE)
    app$wait_for_idle(duration = 500)
    app$click(selector = paste0("#", ids$mixed$run))
    wait_for_output(app, summary_id, timeout_ms = 60000)
    err_html <- app$get_html(".shiny-notification-error")
    expect_true(is.null(err_html) || !grepl("Error", err_html))
  })

  it("renders model summary for uploaded data", {
    require_app(app)
    html <- app$get_html(paste0("#", summary_id))
    expect_true(any(nchar(html) > 0))
  })

  it("activates the EMMs tab and renders real Q0 estimates", {
    require_app(app)
    tabs_id <- ns_id("mixed_effects_demand", "results_display_tabs")
    q0_id <- ns_id("mixed_effects_demand", "emms_q0_table")

    app$set_inputs(!!tabs_id := "EMMs & EV")
    app$wait_for_js(
      sprintf("document.querySelector('#%s tbody tr') !== null", q0_id),
      timeout = 30000
    )
    app$wait_for_idle(duration = 500, timeout = 30000)

    html <- app$get_html(paste0("#", q0_id))
    # EMM table rendered data rows, not the empty-state fallback. This drives
    # the rewired emms_compute$run_observed_emms() wrapper end-to-end.
    # `<td` alone also matches DataTables' "No data available" row, so count
    # real rows too.
    expect_results_table(app, q0_id)
    expect_false(any(grepl("No Q0 EMMs available", html, fixed = TRUE)))

    err_html <- app$get_html(".shiny-notification-error")
    expect_true(is.null(err_html) || !grepl("Error", err_html))
  })

  it("activates the Pairwise Comparisons tab and renders Q0 contrasts", {
    require_app(app)
    tabs_id <- ns_id("mixed_effects_demand", "results_display_tabs")
    factor_id <- ns_id("mixed_effects_demand", "comparison_factor")
    comp_id <- ns_id("mixed_effects_demand", "comparisons_q0_table")

    app$set_inputs(!!tabs_id := "Pairwise Comparisons")
    # The factor selector auto-selects the first model factor (drug).
    app$wait_for_value(input = factor_id, timeout = 30000)
    app$wait_for_js(
      sprintf("document.querySelector('#%s tbody tr') !== null", comp_id),
      timeout = 30000
    )
    app$wait_for_idle(duration = 500, timeout = 30000)

    # Contrast rows rendered: drives comparisons$run_demand_comparisons().
    # Counting rows excludes DataTables' empty-state row, which `<td` matches.
    expect_results_table(app, comp_id)

    err_html <- app$get_html(".shiny-notification-error")
    expect_true(is.null(err_html) || !grepl("Error", err_html))

    # The wrapper uses the canonical param=; the deprecated params_to_compare
    # path must never be exercised by the rewired view.
    logs <- app$get_logs()
    log_messages <- if (is.data.frame(logs)) logs$message else unlist(logs)
    expect_false(any(grepl("params_to_compare", log_messages, fixed = TRUE)))
  })

  local_app_stop()
})

# ==========================================================================
# Full ko dataset with 2 factors (gated)
# ==========================================================================
describe("Mixed Effects - full ko dataset with 2 factors", {
  app <- NULL
  summary_id <- ns_id("mixed_effects_demand", "model_summary_structured")

  it("uploads full ko, configures 2 factors, and runs model", {
    skip_if_not_full_tests()
    app <<- create_app_driver()
    require_app(app)
    navigate_to_tab(app, "MixedEffectsDemand")
    upload_and_wait(
      app, ids$mixed$upload, example_path("shinybeez-ex-ko.csv")
    )
    app$set_inputs(!!ids$mixed$factor1 := "drug", wait_ = FALSE)
    app$wait_for_idle(duration = 500)
    app$set_inputs(!!ids$mixed$factor2 := "dose", wait_ = FALSE)
    app$wait_for_idle(duration = 500)
    app$click(selector = paste0("#", ids$mixed$run))
    wait_for_output(app, summary_id, timeout_ms = 120000)
    html <- app$get_html(paste0("#", summary_id))
    expect_true(any(nchar(html) > 0))
  })

  local_app_stop()
})
