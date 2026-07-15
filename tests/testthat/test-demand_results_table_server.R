# Module-level regression test for production error bce0bb1d.
#
#   "Insufficient values in manual scale. 3 needed but only 0 provided."
#
# The companion file test-demand_plot_scale.R proves the ggplot2-level mechanism and the
# purity of resolve_group_scale(). It deliberately does NOT drive the module — so on its own
# it would keep passing if someone reverted the observer fix. This file closes that gap: it
# runs the real Calculate -> re-upload -> Calculate sequence through demand_results_table's
# server and asserts the reactive state the bug depended on.
#
# Negative control: reverting app/view/demand_results_table.R to its pre-fix state makes the
# "clears the stale plot" test below fail — the stale three-group base_plot survives.

box::use(
  testthat[...],
  shiny,
)

box::use(
  app / view / demand_results_table,
)

# --- Fixtures -----------------------------------------------------------------

grouped_demand_data <- function() {
  prices <- c(0.01, 0.1, 1, 10, 100)
  data.frame(
    id = rep(as.character(1:3), each = 5),
    group = rep(c("a", "b", "c"), each = 5),
    x = rep(prices, 3),
    y = c(
      10, 8, 5, 2, 0,
      12, 9, 6, 3, 1,
      11, 7, 4, 2, 0
    )
  )
}

# Same shape, but with no `group` column at all — the dataset that detonated production.
ungrouped_demand_data <- function() {
  prices <- c(0.01, 0.1, 1, 10, 100)
  data.frame(
    id = rep(as.character(1:3), each = 5),
    x = rep(prices, 3),
    y = c(
      10, 8, 5, 2, 0,
      12, 9, 6, 3, 1,
      11, 7, 4, 2, 0
    )
  )
}

# The plot-decorating observer reads the Plot tab's inputs directly (`if (input$xlog)` and
# friends), which are NULL until the tab has rendered. Seed them with their UI defaults so the
# observer runs the same code path it does in the browser.
set_plot_inputs <- function(session) {
  session$setInputs(
    xtext = "x",
    ytext = "y",
    title = "",
    palette = "Codedbx",
    legend_title = "group",
    xlog = FALSE,
    ylog = FALSE,
    update_plot_btn = 0
  )
}

# The module is always driven with the group checkbox ticked, exactly as the user had it.
module_args <- function(data_r, calc) {
  list(
    data_r = data_r,
    eq = shiny$reactive("Exponential (with k)"),
    agg = shiny$reactive("Mean"),
    fix_q0 = shiny$reactive(FALSE),
    q0_val = shiny$reactive(NULL),
    groupcol = shiny$reactive(TRUE),
    kval = shiny$reactive("2"),
    calculate_btn = calc
  )
}

describe("demand results table plot state", {
  it("records the group levels the base plot was built with", {
    data_r <- shiny$reactiveValues(data_d = grouped_demand_data())
    calc <- shiny$reactiveVal(0)

    shiny$testServer(demand_results_table$server, args = module_args(data_r, calc), {
      set_plot_inputs(session)
      calc(1)
      session$flushReact()

      expect_setequal(res$plot_group_levels, c("a", "b", "c"))
      expect_false(is.null(res$base_plot))
    })
  })

  # The regression. Pre-fix, the base-plot observer returned early (no `group` column while
  # the checkbox is ticked) BEFORE resetting res$base_plot, so a stale three-group plot
  # survived, satisfied the decorator's req(), and was then handed a zero-length palette.
  it("clears the stale plot when new data has no group column (bce0bb1d)", {
    data_r <- shiny$reactiveValues(data_d = grouped_demand_data())
    calc <- shiny$reactiveVal(0)

    shiny$testServer(demand_results_table$server, args = module_args(data_r, calc), {
      set_plot_inputs(session)
      calc(1)
      session$flushReact()
      expect_setequal(res$plot_group_levels, c("a", "b", "c"))

      # User uploads a dataset with no `group` column and hits Calculate again.
      data_r$data_d <- ungrouped_demand_data()
      calc(2)
      session$flushReact()

      # Nothing stale may survive. A leftover three-level plot is precisely what got
      # coloured with zero values; if any of these is non-NULL the bug is reachable again.
      expect_null(res$plot_group_levels)
      expect_null(res$base_plot)
      expect_null(res$plot)
    })
  })

  it("does not error when the group box is ticked but the column is missing", {
    data_r <- shiny$reactiveValues(data_d = ungrouped_demand_data())
    calc <- shiny$reactiveVal(0)

    expect_no_error(
      shiny$testServer(demand_results_table$server, args = module_args(data_r, calc), {
        calc(1)
        session$flushReact()

        expect_null(res$base_plot)
        expect_null(res$plot_group_levels)
      })
    )
  })
})
