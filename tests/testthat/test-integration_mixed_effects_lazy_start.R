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

# Records the text of every Shiny notification added to the page from now on. A
# progress toast for a fast computation is gone well before wait_for_idle()
# returns, so polling the DOM afterwards would miss it.
watch_notifications <- function(app) {
  app$run_js("
    window.__seenNotes = [];
    // A toast is inserted empty and its message is filled in by a later
    // mutation, so re-read every live toast on each batch instead of reading
    // the inserted node once.
    new MutationObserver(function() {
      document.querySelectorAll('.shiny-notification').forEach(function(c) {
        window.__seenNotes.push(c.innerText);
      });
    }).observe(document.body, { childList: true, subtree: true, characterData: true });
  ")
}

seen_notifications <- function(app) {
  unlist(app$get_js("window.__seenNotes || []"))
}

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

describe("Mixed Effects - systematic subtab is lazy", {
  app <- NULL
  table_sel <- paste0("#", ns_id("mixed_effects_demand", "systematic_table"))
  tbody_text_js <- sprintf(
    "(document.querySelector('%s tbody') || {}).innerText || ''", table_sel
  )

  it("does not compute systematic criteria while the Data subtab is shown", {
    app <<- create_app_driver()
    require_app(app)
    app$wait_for_idle(duration = 500)
    watch_notifications(app)

    navigate_to_tab(app, "MixedEffectsDemand")
    app$wait_for_value(input = ids$mixed$x_var, ignore = list(NULL, ""))
    app$wait_for_idle(duration = 1000)

    expect_false(any(grepl(
      "Computing systematic criteria", seen_notifications(app), fixed = TRUE
    )))
  })

  it("renders the table when its subtab is opened", {
    require_app(app)
    app$set_inputs(
      !!ns_id("mixed_effects_demand", "data_display_tabs") := "Systematic Criteria"
    )
    app$wait_for_js(
      sprintf("document.querySelectorAll('%s tbody tr td').length > 1", table_sel),
      timeout = 15000
    )

    rows <- app$get_js(
      sprintf("document.querySelectorAll('%s tbody tr').length", table_sel)
    )
    expect_gt(rows, 0)
  })

  it("recomputes when a threshold changes", {
    require_app(app)
    before <- app$get_js(tbody_text_js)

    expect_no_match(before, "\t0.5\t", fixed = TRUE)

    # Wait for the new threshold to appear in the trend_threshold cells, not just
    # for the text to differ: mid-redraw the tbody is briefly empty.
    app$set_inputs(!!ns_id("mixed_effects_demand", "deltaq") := 0.5)
    app$wait_for_js(
      sprintf("(%s).indexOf('\\t0.5\\t') !== -1", tbody_text_js),
      timeout = 15000
    )

    expect_match(app$get_js(tbody_text_js), "\t0.5\t", fixed = TRUE)
  })

  local_app_stop()
})
