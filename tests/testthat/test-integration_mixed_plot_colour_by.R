# Integration test: a refit with the Plot tab visible must render the coloured plot first.
# The defaults observer pushes the new colour selection to the browser; the plot render waits
# for it (aes_pending gate) instead of drawing with the old, not-yet-updated selection.
# Journey mirrors test-integration_plot_palette.R: default ko data, drug -> dose.

describe("Mixed Effects - refit with the Plot tab visible", {
  app <- NULL
  summary_id <- ns_id("mixed_effects_demand", "model_summary_structured")
  tabs_id <- ns_id("mixed_effects_demand", "results_display_tabs")
  plot_id <- ns_id("mixed_effects_demand", "mixed_model_plot", "plot")
  update_id <- ns_id("mixed_effects_demand", "update_plot_settings")
  color_id <- ns_id("mixed_effects_demand", "plot_color_by")
  src_js <- sprintf(
    "document.querySelector('#%s img') ? document.querySelector('#%s img').getAttribute('src') : null",
    plot_id, plot_id
  )
  src_drug <- NULL
  src_first <- NULL
  src_after <- NULL

  refit_with_factor1 <- function(app, level) {
    app$set_inputs(!!ids$mixed$factor1 := level, wait_ = FALSE)
    app$wait_for_idle(duration = 500)
    app$click(selector = paste0("#", ids$mixed$run))
    wait_for_output(app, summary_id, timeout_ms = 60000)
  }
  expect_plot_without_error <- function(app) {
    html <- app$get_html(paste0("#", plot_id))
    expect_false(any(grepl("shiny-output-error", html, fixed = TRUE)))
  }

  it("fits the default ko data with drug and shows the plot", {
    app <<- create_app_driver()
    require_app(app)
    navigate_to_tab(app, "MixedEffectsDemand")
    wait_for_input(app, ids$mixed$id_var)
    app$set_inputs(!!ids$mixed$factor1 := "drug", wait_ = FALSE)
    app$wait_for_idle(duration = 500)
    app$click(selector = paste0("#", ids$mixed$run))
    wait_for_output(app, summary_id, timeout_ms = 60000)
    app$set_inputs(!!tabs_id := "Plot")
    app$wait_for_js(sprintf("(%s) !== null", src_js), timeout = 60000)
    app$wait_for_idle(duration = 1000, timeout = 60000)
    src_drug <<- app$get_js(src_js)
    expect_true(is.character(src_drug) && nzchar(src_drug))
  })

  it("renders the coloured plot first after a dose refit, identical to Update Plot", {
    require_app(app)
    refit_with_factor1(app, "dose")
    app$wait_for_js(
      sprintf("(%s) !== null && (%s) !== %s", src_js, src_js, jsonlite::toJSON(src_drug, auto_unbox = TRUE)),
      timeout = 60000
    )
    app$wait_for_idle(duration = 1000, timeout = 60000)
    src_first <<- app$get_js(src_js)
    expect_equal(app$get_value(input = color_id), "dose")
    expect_plot_without_error(app)

    # Update Plot with no other change re-renders from the synced inputs: same key, same PNG.
    app$click(selector = paste0("#", update_id))
    app$wait_for_idle(duration = 1000, timeout = 60000)
    src_after <<- app$get_js(src_js)
    expect_identical(src_first, src_after)
    expect_plot_without_error(app)
  })

  it("refits with unchanged defaults without gating the plot", {
    require_app(app)
    refit_with_factor1(app, "dose")
    app$wait_for_idle(duration = 1000, timeout = 60000)
    src_refit <- app$get_js(src_js)
    expect_true(is.character(src_refit) && nzchar(src_refit))
    expect_plot_without_error(app)
    expect_equal(app$get_value(input = color_id), "dose")
    # An unchanged refit must not leave the plot blank or different from Update Plot.
    app$click(selector = paste0("#", update_id))
    app$wait_for_idle(duration = 1000, timeout = 60000)
    expect_identical(app$get_js(src_js), src_refit)
  })

  local_app_stop()
})
