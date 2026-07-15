# Regression tests for the demand plot's discrete colour scale.
#
# Production error bce0bb1d (2026-07-13, shinybeez.app):
#   "Insufficient values in manual scale. 3 needed but only 0 provided."
#
# demand_results_table.R builds the base plot on Calculate, but re-derives the number of
# colours from the *live* uploaded data every time it decorates the plot. When those two
# disagree — a 3-group plot decorated against a dataset with no `group` column — ggplot2
# is handed a zero-length `values` vector and aborts at build time.

box::use(
  testthat[...],
  ggplot2,
)

box::use(
  app / logic / utils,
)

# --- Fixtures -----------------------------------------------------------------

# The plot as it exists after Calculate on grouped data: a colour aesthetic with 3 levels.
make_three_group_plot <- function() {
  dat <- data.frame(
    x = rep(c(1, 5, 10), 3),
    y = c(10, 6, 2, 9, 5, 1, 8, 4, 1),
    group = rep(c("a", "b", "c"), each = 3)
  )
  ggplot2$ggplot(dat, ggplot2$aes(x = x, y = y, colour = group)) +
    ggplot2$geom_line()
}

# The dataset the user uploads next: same shape, but no `group` column at all.
make_ungrouped_data <- function() {
  data.frame(x = c(1, 5, 10), y = c(10, 6, 2))
}

describe("get_palette_colors", {
  it("returns one colour per requested group", {
    expect_length(utils$get_palette_colors("Codedbx", 3), 3)
  })

  it("returns a zero-length vector when asked for zero colours", {
    # This is a correct guard in itself — the bug is a caller feeding the result to a scale.
    expect_length(utils$get_palette_colors("Codedbx", 0), 0)
  })

  # length(unique(NULL)) is 0 — this is exactly how n_groups collapses to zero in the app
  # when the group checkbox is ticked but the uploaded data has no `group` column.
  it("collapses to zero colours for a missing group column", {
    ungrouped <- make_ungrouped_data()
    n_groups <- length(unique(ungrouped$group))
    expect_identical(n_groups, 0L)
    expect_length(utils$get_palette_colors("Codedbx", n_groups), 0)
  })
})

describe("resolve_group_scale", {
  it("reproduces bce0bb1d: a 3-level plot given zero colours fails to build", {
    # Guard the RED: assert the *exact* production message, so this test can only pass
    # for the right reason.
    ungrouped <- make_ungrouped_data()
    n_groups <- length(unique(ungrouped$group))

    bad_plot <- make_three_group_plot() +
      ggplot2$scale_colour_manual(
        values = utils$get_palette_colors("Codedbx", n_groups)
      )

    expect_error(
      ggplot2$ggplot_build(bad_plot),
      "Insufficient values in manual scale. 3 needed but only 0 provided",
      fixed = TRUE
    )
  })

  it("returns NULL when the plot has no group levels, so no scale is added", {
    # The fix: the caller asks for a scale based on what the PLOT was built with, and gets
    # NULL when there is nothing to colour. A NULL added to a ggplot is a no-op.
    expect_null(utils$resolve_group_scale(NULL, "Codedbx"))
    expect_null(utils$resolve_group_scale(character(0), "Codedbx"))

    safe_plot <- make_three_group_plot() +
      utils$resolve_group_scale(NULL, "Codedbx")
    expect_s3_class(ggplot2$ggplot_build(safe_plot), "ggplot_built")
  })

  it("colours a plot using the levels it was actually built with", {
    scale <- utils$resolve_group_scale(c("a", "b", "c"), "Codedbx")
    built <- ggplot2$ggplot_build(make_three_group_plot() + scale)
    expect_s3_class(built, "ggplot_built")
  })

  # The desync the naive "does the group column exist?" guard does NOT catch: the new
  # dataset has a group column, just fewer levels than the plot was built with.
  it("is immune to a new dataset with fewer groups than the fitted plot", {
    two_group_data <- data.frame(
      x = rep(c(1, 5), 2), y = c(9, 4, 8, 3), group = rep(c("a", "b"), each = 2)
    )
    # Deriving n from the LIVE data (2) while the plot has 3 levels is the bug.
    expect_error(
      ggplot2$ggplot_build(
        make_three_group_plot() +
          ggplot2$scale_colour_manual(
            values = utils$get_palette_colors(
              "Codedbx", length(unique(two_group_data$group))
            )
          )
      ),
      "Insufficient values in manual scale. 3 needed but only 2 provided",
      fixed = TRUE
    )
    # Deriving it from the plot's own levels is always consistent.
    built <- ggplot2$ggplot_build(
      make_three_group_plot() + utils$resolve_group_scale(c("a", "b", "c"), "Codedbx")
    )
    expect_s3_class(built, "ggplot_built")
  })

  it("drops NA levels rather than reserving a colour for them", {
    expect_length(
      utils$resolve_group_scale(c("a", NA, "b"), "Codedbx")$palette(2), 2
    )
  })
})
