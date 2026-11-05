# Integration tests: the palette picker with swatch previews and the preflight note under
# it, on the mixed-effects plot (this describe) and the demand plot (below). Mirrors the
# prominence journey in test-integration_mixed_effects.R. Runs in the ordinary suite like
# that journey: default ko data and the minimal grouped fixture, no full-examples gate.

# Open the bslib sidebar that holds `input_id` (both Plot Settings sidebars are
# `open = FALSE`, i.e. display: none) through its own toggle, and wait for the transition.
# bslib marks the layout `.sidebar-collapsed` while closed and `.transitioning` while moving.
open_sidebar_of <- function(app, input_id, timeout_ms = 10000) {
  layout_js <- sprintf("document.getElementById('%s').closest('.bslib-sidebar-layout')", input_id)
  app$run_js(sprintf("%s.querySelector(':scope > .collapse-toggle').click()", layout_js))
  is_open_js <- sprintf(
    paste0(
      "(function(){var l=%s;",
      "return !l.classList.contains('sidebar-collapsed') && !l.classList.contains('transitioning');})()"
    ),
    layout_js
  )
  app$wait_for_js(is_open_js, timeout = timeout_ms)
  app$wait_for_idle(duration = 500, timeout = timeout_ms)
}

# ==========================================================================
# Mixed effects: default ko data, Factor 1 = drug (3 levels)
# ==========================================================================
describe("Mixed Effects - palette picker", {
  app <- NULL
  summary_id <- ns_id("mixed_effects_demand", "model_summary_structured")
  tabs_id <- ns_id("mixed_effects_demand", "results_display_tabs")
  palette_id <- ns_id("mixed_effects_demand", "plot_palette")
  note_id <- ns_id("mixed_effects_demand", "palette_note")
  plot_id <- ns_id("mixed_effects_demand", "mixed_model_plot", "plot")
  update_id <- ns_id("mixed_effects_demand", "update_plot_settings")

  it("fits the default ko data with drug as Factor 1", {
    app <<- create_app_driver()
    require_app(app)
    navigate_to_tab(app, "MixedEffectsDemand")
    wait_for_input(app, ids$mixed$id_var)
    app$set_inputs(!!ids$mixed$factor1 := "drug", wait_ = FALSE)
    app$wait_for_idle(duration = 500)
    expect_equal(app$get_value(input = ids$mixed$factor1), "drug")
    app$click(selector = paste0("#", ids$mixed$run))
    wait_for_output(app, summary_id, timeout_ms = 60000)
  })

  it("draws swatches in the picker and shows no note for the default palette", {
    require_app(app)
    app$set_inputs(!!tabs_id := "Plot")
    app$wait_for_js(sprintf("document.querySelector('#%s img') !== null", plot_id), timeout = 30000)
    # The picker lives in the Plot Settings sidebar, collapsed by default (display: none).
    # Open it the way a user does, through its own toggle, and wait for the transition.
    open_sidebar_of(app, palette_id)
    expect_equal(app$get_value(input = palette_id), "Codedbx")
    expect_equal(app$get_js("typeof window.shinybeezPaletteOption"), "function")
    # The selected item is rendered through the custom renderer: six Codedbx squares.
    n_swatches <- app$get_js(sprintf(
      "document.querySelectorAll('#%s + .selectize-control .item .palette-swatch').length", palette_id
    ))
    expect_equal(n_swatches, 6)
    note_html <- app$get_html(paste0("#", note_id))
    expect_false(grepl("palette-note", note_html, fixed = TRUE))
  })

  it("lists every palette name in full, above its swatches", {
    require_app(app)
    # In the 250px sidebar a [swatches][name] row left 8-swatch palettes a 66px name, so
    # half the names were cut ("prism_light" and "prism_dark" both read "prism_..."). The
    # stacking check does not depend on font metrics, so it catches that layout locally.
    # The selected item keeps the one-row layout. setValue(.., true) is silent: no change
    # event, so Shiny never sees it and the plot does not re-render.
    n_palettes <- app$get_js("Object.keys(window.shinybeezPalettes).length")
    sel_js <- sprintf("$('#%s')[0].selectize", palette_id)
    # Close the picker even if the wait below times out, so the later tests start from a closed control.
    withr::defer(app$run_js(sprintf("var s = %s; s.close(); s.blur();", sel_js)))
    # Selectize draws the dropdown rows asynchronously after open().
    app$run_js(sprintf("var s = %s; s.focus(); s.open();", sel_js))
    app$wait_for_js(sprintf(
      "%s.$dropdown[0].querySelectorAll('.option.palette-option').length === %d", sel_js, n_palettes
    ), timeout = 10000)
    res <- app$get_js(sprintf("(function(){
      var s = %s, before = s.getValue(), out = {options: [], items: []};
      var box = function(el) { return el.getBoundingClientRect(); };
      try {
        s.$dropdown[0].querySelectorAll('.option.palette-option').forEach(function(o) {
          var n = o.querySelector('.palette-name'), sw = box(o.querySelector('.palette-swatches')), nb = box(n);
          out.options.push({name: n.textContent, w: nb.width, swh: sw.height,
            cut: n.scrollWidth > n.clientWidth, below: sw.top >= nb.bottom - 1});
        });
        Object.keys(window.shinybeezPalettes).forEach(function(p) {
          s.setValue(p, true);
          var it = s.$control[0].querySelector('.item.palette-option'), n = it.querySelector('.palette-name');
          var sw = box(it.querySelector('.palette-swatches')), nb = box(n);
          out.items.push({name: p, w: nb.width, cut: n.scrollWidth > n.clientWidth,
            row: Math.abs((sw.top + sw.bottom) / 2 - (nb.top + nb.bottom) / 2) < 4});
        });
      } finally {
        s.setValue(before, true);
      }
      return out;
    })()", sel_js))
    opts <- do.call(rbind, lapply(res$options, as.data.frame))
    items <- do.call(rbind, lapply(res$items, as.data.frame))
    expect_equal(nrow(opts), n_palettes)
    expect_true(all(opts$w > 0 & opts$swh > 0))
    expect_equal(opts$name[opts$cut], character(0))
    expect_equal(opts$name[!opts$below], character(0))
    expect_equal(nrow(items), n_palettes)
    expect_true(all(items$w > 0))
    expect_equal(items$name[items$cut], character(0))
    expect_equal(items$name[!items$row], character(0))
    expect_equal(app$get_value(input = palette_id), "Codedbx")
  })

  it("switches to viridis, warns about the yellow, and re-renders on Update Plot", {
    require_app(app)
    toggle_js <- "document.querySelector('bslib-input-dark-mode').shadowRoot.querySelector('button').click()"
    plot_src_js <- sprintf("document.querySelector('#%s img').getAttribute('src')", plot_id)

    # The app boots dark and the note must follow the colour mode (spec 6.3). Switch to
    # light the way a user does: bslib's dark-mode binding has no setValue, so click the
    # toggle inside the custom element's shadow root. The plot re-renders on the toggle
    # (its event list includes dark_mode) and then reads the live palette, so settle it
    # on Codedbx first; Update Plot below must be what brings viridis onto the page.
    src_dark <- app$get_js(plot_src_js)
    app$run_js(toggle_js)
    app$wait_for_js("document.documentElement.getAttribute('data-bs-theme') === 'light'", timeout = 10000)
    app$wait_for_js(
      sprintf("%s !== %s", plot_src_js, jsonlite::toJSON(src_dark, auto_unbox = TRUE)),
      timeout = 30000
    )
    app$wait_for_idle(duration = 500, timeout = 30000)
    expect_equal(app$get_value(input = "dark_mode"), "light")
    src_before <- app$get_js(plot_src_js)

    app$set_inputs(!!palette_id := "viridis")
    app$wait_for_idle(duration = 500, timeout = 30000)
    expect_equal(app$get_value(input = palette_id), "viridis")
    # Three drug levels -> viridis(3) ends in the yellow, 1.3:1 on white.
    note_html <- app$get_html(paste0("#", note_id))
    expect_match(note_html, "viridis colour 3 (#FDE725) is 1.3:1 against white: faint", fixed = TRUE)

    app$click(selector = paste0("#", update_id))
    app$wait_for_js(
      sprintf("%s !== %s", plot_src_js, jsonlite::toJSON(src_before, auto_unbox = TRUE)),
      timeout = 30000
    )
    app$wait_for_idle(duration = 500, timeout = 30000)
    html <- app$get_html(paste0("#", plot_id))
    expect_false(any(grepl("shiny-output-error", html, fixed = TRUE)))
    err_html <- app$get_html(".shiny-notification-error")
    expect_true(is.null(err_html) || !grepl("Error", err_html))

    # Back to dark (the boot state): the 3:1 lift pins viridis' dark end, so the note
    # switches to the greyscale clause.
    app$run_js(toggle_js)
    app$wait_for_js("document.documentElement.getAttribute('data-bs-theme') === 'dark'", timeout = 10000)
    app$wait_for_idle(duration = 500, timeout = 30000)
    expect_equal(app$get_value(input = "dark_mode"), "dark")
    note_html <- app$get_html(paste0("#", note_id))
    expect_match(
      note_html, "levels 1 and 2 differ by 16% luminance: hard to tell apart in greyscale",
      fixed = TRUE
    )
  })

  local_app_stop()
})

# ==========================================================================
# Demand: minimal grouped fixture (groups A and B), pooled fit
# ==========================================================================
describe("Demand - palette picker", {
  app <- NULL
  result_id <- ns_id("demand", "results_table_demand", "model_results_table")
  palette_id <- ns_id("demand", "results_table_demand", "palette")
  note_id <- ns_id("demand", "results_table_demand", "palette_note")
  plot_id <- ns_id("demand", "results_table_demand", "plot", "plot")
  update_id <- ns_id("demand", "results_table_demand", "update_plot_btn")

  it("runs a grouped pooled fit", {
    app <<- create_app_driver()
    require_app(app)
    navigate_to_tab(app, "Demand")
    upload_and_wait(app, ids$demand$upload, fixture_path("demand-minimal-grouped.csv"))
    app$set_inputs(!!ids$demand$group := TRUE)
    app$wait_for_idle(duration = 500)
    app$set_inputs(!!ids$demand$k := "2", wait_ = FALSE)
    app$wait_for_idle(duration = 500)
    app$click(selector = paste0("#", ids$demand$calculate))
    wait_for_output(app, result_id, timeout_ms = 60000)
    expect_true(app$get_js(sprintf("document.getElementById('%s') !== null", result_id)))
  })

  it("switches to Set2 on the Plots tab and re-renders with no note", {
    require_app(app)
    # The results navset has no input id; click its Plots tab from inside the results card
    # (the discounting page has a Plots tab too, so the selector is scoped).
    app$run_js(sprintf(
      "document.getElementById('%s').closest('.card').querySelector(\"a.nav-link[data-value='Plots']\").click()",
      result_id
    ))
    app$wait_for_js(sprintf("document.querySelector('#%s img') !== null", plot_id), timeout = 30000)
    open_sidebar_of(app, palette_id)
    expect_equal(app$get_value(input = palette_id), "Codedbx")
    n_swatches <- app$get_js(sprintf(
      "document.querySelectorAll('#%s + .selectize-control .item .palette-swatch').length", palette_id
    ))
    expect_equal(n_swatches, 6)

    # The app boots dark; the note messages below are the light-mode ones (in dark mode
    # viridis is lifted and its yellow is not faint). Switch to light the way a user does:
    # click the toggle inside the dark-mode element's shadow root. The plot re-renders on
    # the toggle, so take src_before only after it settles.
    app$run_js("document.querySelector('bslib-input-dark-mode').shadowRoot.querySelector('button').click()")
    app$wait_for_js("document.documentElement.getAttribute('data-bs-theme') === 'light'", timeout = 10000)
    app$wait_for_idle(duration = 500, timeout = 30000)
    expect_equal(app$get_value(input = "dark_mode"), "light")
    src_before <- app$get_js(sprintf("document.querySelector('#%s img').getAttribute('src')", plot_id))

    # Prove the note output renders at all before asserting it is empty: viridis always ends
    # in the yellow, so two groups -> viridis(2) -> colour 2 is faint on white.
    app$set_inputs(!!palette_id := "viridis")
    app$wait_for_idle(duration = 500, timeout = 30000)
    expect_match(
      app$get_html(paste0("#", note_id)),
      "viridis colour 2 (#FDE725) is 1.3:1 against white: faint",
      fixed = TRUE
    )

    app$set_inputs(!!palette_id := "Set2")
    app$wait_for_idle(duration = 500, timeout = 30000)
    expect_equal(app$get_value(input = palette_id), "Set2")
    # Two groups -> Set2's first two entries, both above 1.5:1 on white: nothing to say.
    expect_false(grepl("palette-note", app$get_html(paste0("#", note_id)), fixed = TRUE))

    app$click(selector = paste0("#", update_id))
    app$wait_for_js(
      sprintf(
        "document.querySelector('#%s img').getAttribute('src') !== %s",
        plot_id, jsonlite::toJSON(src_before, auto_unbox = TRUE)
      ),
      timeout = 30000
    )
    app$wait_for_idle(duration = 500, timeout = 30000)
    html <- app$get_html(paste0("#", plot_id))
    expect_false(any(grepl("shiny-output-error", html, fixed = TRUE)))
  })

  local_app_stop()
})
