# Integration tests: Mixed Effects Demand fit as an ExtendedTask (async).
#
# Four claims, one per journey:
#   1. clicking Run puts the task button into its busy state and the model still
#      lands (bslib$bind_task_button + the task's success path)
#   2. rerunning refreshes the comparisons table -- the bindCache key moved from
#      run_trigger() to fit_generation(), so a rerun must still invalidate it
#   3. Cancel is offered while a fit is running, stops it, and returns the UI to
#      idle with a "cancelled" warning rather than a result
#   4. SHINYBEEZ_DAEMONS=0 still fits, inline (the retained synchronous path)

# The task button is disabled exactly while its ExtendedTask is busy
# (bslib components.js: `el.disabled = state === "busy"`), so "enabled again"
# is the end-of-fit signal, and it is what the plain wait_for_* helpers cannot
# see now that the fit no longer blocks the session.
button_idle_js <- function(id) {
  sprintf(
    "(function(){var b=document.getElementById('%s');return !!b && !b.disabled;})()",
    id
  )
}

describe("Mixed Effects - async fit", {
  app <- NULL
  summary_id <- ns_id("mixed_effects_demand", "model_summary_structured")
  comps_id <- ns_id("mixed_effects_demand", "comparisons_q0_table")
  tabs_id <- ns_id("mixed_effects_demand", "results_display_tabs")

  it("starts the app and navigates to Mixed Effects Demand", {
    app <<- create_app_driver()
    require_app(app)
    navigate_to_tab(app, "MixedEffectsDemand")
    wait_for_input(app, ids$mixed$id_var)
    expect_equal(app$get_value(input = ids$nav), "MixedEffectsDemand")
  })

  it("fits the default ko data with drug as a factor via the task button", {
    require_app(app)
    app$set_inputs(!!ids$mixed$factor1 := "drug", wait_ = FALSE)
    app$wait_for_idle(duration = 500)
    app$click(selector = paste0("#", ids$mixed$run))
    app$wait_for_js(button_idle_js(ids$mixed$run), timeout = 60000)
    wait_for_output(app, summary_id, timeout_ms = 60000)
    expect_true(
      any(grepl("Model Specification", app$get_html(paste0("#", summary_id)),
                fixed = TRUE)),
      info = "expected the Model Specification card after the async fit"
    )
  })

  it("renders the comparisons table for the first fit", {
    require_app(app)
    app$set_inputs(!!tabs_id := "Pairwise Comparisons")
    app$wait_for_js(
      sprintf("document.querySelector('#%s tbody tr') !== null", comps_id),
      timeout = 60000
    )
    app$wait_for_idle(duration = 500, timeout = 60000)
    expect_results_table(app, comps_id)
  })

  it("refreshes the comparisons table when the model is refitted", {
    require_app(app)
    # Only a NEW FIT can refresh the table here: the covariance structure is not
    # one of comparisons_reactive's bindCache keys, so under the old
    # `run_trigger()` key the click recomputed against the previous model and the
    # success never touched the cache -- this must fail there and pass on
    # `fit_generation()`. Everything comparisons_reactive does key on (adjustment
    # method, comparison factor, display type, covariate controls) is held fixed.
    #
    # The container keeps the previous run's rows while DataTables fetches the
    # new ones, so remember the rendered text in the page and wait for it to
    # change; sampling right after Shiny goes idle is a race.
    app$get_js(sprintf(
      "window.__comps = document.querySelector('#%s tbody').innerText; true",
      comps_id
    ))
    first <- app$get_html(paste0("#", comps_id))
    expect_equal(app$get_value(input = ids$mixed$covariance), "pdDiag")
    app$set_inputs(!!ids$mixed$covariance := "pdSymm", wait_ = FALSE)
    app$wait_for_idle(duration = 500)
    app$click(selector = paste0("#", ids$mixed$run))
    app$wait_for_js(button_idle_js(ids$mixed$run), timeout = 60000)
    app$wait_for_js(
      sprintf(
        paste0(
          "(function(){var t=document.querySelector('#%s tbody');",
          "return !!t && t.innerText !== window.__comps;})()"
        ),
        comps_id
      ),
      timeout = 60000
    )
    second <- app$get_html(paste0("#", comps_id))
    expect_results_table(app, comps_id)
    expect_false(identical(first, second))
  })

  it("cancel returns the UI to idle without a new model", {
    require_app(app)
    skip_if_not_full_tests()
    upload_and_wait(
      app, ids$mixed$upload, heavy_mixed_fixture(), timeout_ms = 120000
    )
    wait_for_input(app, ids$mixed$id_var, timeout_ms = 30000)
    app$click(selector = paste0("#", ids$mixed$run))
    # The busy state itself, asserted once: the heavy fixture takes seconds, so
    # the disabled window is observable. Without this the suite only ever proves
    # the button comes BACK, which a button that never went busy also satisfies.
    app$wait_for_js(
      sprintf(
        "document.getElementById('%s').disabled === true", ids$mixed$run
      ),
      timeout = 5000
    )
    app$wait_for_js(
      sprintf("document.getElementById('%s') !== null", ids$mixed$cancel),
      timeout = 15000
    )
    app$click(selector = paste0("#", ids$mixed$cancel))
    wait_for_notification(app, type = "warning", timeout_ms = 60000)
    expect_true(
      grepl("cancelled", app$get_html(".shiny-notification-warning"),
            ignore.case = TRUE)
    )
    app$wait_for_js(button_idle_js(ids$mixed$run), timeout = 30000)
  })

  # The fit runs on a snapshot taken at invoke, but the session stays live, so a
  # sidebar edit made WHILE it runs must not leak into the finished fit's
  # post-hoc output. (The Model Specification card does not surface the
  # covariance structure, so the assertion lands on the fields it does show plus
  # the comparisons table, which is one of the readers that used to rebuild from
  # live state.)
  it("post-hoc output belongs to the fit that started, not to a mid-run edit", {
    require_app(app)
    skip_if_not_full_tests()
    started_with <- app$get_value(input = ids$mixed$covariance)
    other <- if (identical(started_with, "pdDiag")) "pdSymm" else "pdDiag"
    app$set_inputs(!!ids$mixed$factor1 := "drug", wait_ = FALSE)
    app$wait_for_idle(duration = 500, timeout = 60000)

    app$click(selector = paste0("#", ids$mixed$run))
    app$wait_for_js(
      sprintf(
        "document.getElementById('%s').disabled === true", ids$mixed$run
      ),
      timeout = 5000
    )
    # Mid-flight model-setting change: must be ignored until the next Run.
    app$set_inputs(!!ids$mixed$covariance := other, wait_ = FALSE)

    app$wait_for_js(button_idle_js(ids$mixed$run), timeout = 120000)
    wait_for_output(app, summary_id, timeout_ms = 60000)
    summary_html <- app$get_html(paste0("#", summary_id))
    expect_true(
      any(grepl("Model Specification", summary_html, fixed = TRUE)),
      info = "the completed fit still renders its own summary"
    )
    expect_true(
      any(grepl("drug", summary_html, fixed = TRUE)),
      info = "the summary reports the factors the fit was started with"
    )
    # The comparisons reader now conditions on the fit's own snapshot; before
    # the fix it rebuilt from live sidebar state and could error here.
    app$set_inputs(!!tabs_id := "Pairwise Comparisons")
    app$wait_for_js(
      sprintf("document.querySelector('#%s tbody tr') !== null", comps_id),
      timeout = 120000
    )
    expect_results_table(app, comps_id)
  })

  local_app_stop()
})

describe("Mixed Effects - synchronous fallback (SHINYBEEZ_DAEMONS=0)", {
  it("still fits when daemons are disabled", {
    skip_if_not_full_tests()
    withr::local_envvar(c(SHINYBEEZ_DAEMONS = "0"))
    summary_id <- ns_id("mixed_effects_demand", "model_summary_structured")
    app <- create_app_driver()
    on.exit(try(app$stop(), silent = TRUE), add = TRUE)
    require_app(app)
    navigate_to_tab(app, "MixedEffectsDemand")
    wait_for_input(app, ids$mixed$id_var)
    app$click(selector = paste0("#", ids$mixed$run))
    wait_for_output(app, summary_id, timeout_ms = 60000)
    expect_true(
      any(grepl("Model Specification", app$get_html(paste0("#", summary_id)),
                fixed = TRUE))
    )
  })
})
