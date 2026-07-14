# Regression tests for apply_color_palette — the second instance of the bce0bb1d defect.
#
#   "Insufficient values in manual scale. 3 needed but only 0 provided."
#
# Found by the automated error investigator on 2026-07-14, independently of the Demand-plot
# fix in the same branch. Same defect class, different module: the palette is SIZED from one
# frame (`model_fit$data`) while the plot DRAWS its colour groups from another (the prediction
# grid that plot.beezdemand builds, conditioned via `at`). When those disagree, ggplot2 is
# handed a values vector that is too short and aborts at build time.
#
# Two ways they disagree:
#   * the colour column in $data is entirely NA -> zero levels -> character(0)
#   * the prediction grid spans levels that $data does not -> too few values
#
# Established behaviour of scale_colour_manual (verified, not assumed):
#   exact count -> ok | MORE values than needed -> ok | fewer -> abort | zero -> abort
# So over-provisioning is always safe, and sizing from the union of both frames cannot come up
# short.

box::use(
  testthat[...],
  ggplot2,
)

box::use(
  app / logic / mixed_effects / plotting,
  app / logic / utils,
)

# --- Fixtures -----------------------------------------------------------------

# A plot whose colour aesthetic has 3 levels — what plot(model_fit) actually draws.
three_level_plot <- function() {
  grid <- data.frame(
    x = rep(c(1, 5, 10), 3),
    y = c(9, 5, 1, 8, 4, 1, 7, 3, 1),
    dose = factor(rep(c("low", "mid", "high"), each = 3))
  )
  ggplot2$ggplot(grid, ggplot2$aes(x = x, y = y, colour = dose)) +
    ggplot2$geom_line()
}

palette_fn <- function(name, n) utils$get_palette_colors(name, n)

describe("apply_color_palette", {
  it("survives a colour column that is entirely NA (bce0bb1d)", {
    # The model's stored $data has the column, but every value is NA — so the old code
    # computed zero levels and asked for zero colours, while the plot draws three.
    fit_data <- data.frame(
      x = c(1, 5, 10),
      y = c(9, 5, 1),
      dose = factor(rep(NA_character_, 3), levels = c("low", "mid", "high"))
    )

    p <- plotting$apply_color_palette(
      three_level_plot(), "dose", fit_data, "Codedbx", palette_fn
    )

    expect_s3_class(ggplot2$ggplot_build(p), "ggplot_built")
  })

  it("survives a prediction grid with more levels than the fitted data", {
    # $data saw only two doses; the plot's grid spans three. The old code sized the palette
    # from $data and produced "3 needed but only 2 provided".
    fit_data <- data.frame(
      x = c(1, 5),
      y = c(9, 5),
      dose = factor(c("low", "mid"), levels = c("low", "mid", "high"))
    )

    p <- plotting$apply_color_palette(
      three_level_plot(), "dose", fit_data, "Codedbx", palette_fn
    )

    expect_s3_class(ggplot2$ggplot_build(p), "ggplot_built")
  })

  it("still colours the normal case", {
    fit_data <- data.frame(
      x = rep(c(1, 5, 10), 3),
      y = c(9, 5, 1, 8, 4, 1, 7, 3, 1),
      dose = factor(rep(c("low", "mid", "high"), each = 3))
    )

    p <- plotting$apply_color_palette(
      three_level_plot(), "dose", fit_data, "Codedbx", palette_fn
    )

    expect_s3_class(ggplot2$ggplot_build(p), "ggplot_built")
    # A manual scale really was applied, rather than the crash merely being dodged.
    expect_true(
      any(vapply(p$scales$scales, function(s) "colour" %in% s$aesthetics, logical(1)))
    )
  })

  it("adds no scale when there is no colour variable", {
    p <- three_level_plot()
    expect_identical(
      plotting$apply_color_palette(p, NULL, data.frame(), "Codedbx", palette_fn),
      p
    )
  })

  it("adds no scale when the colour column is absent from both frames", {
    plain <- ggplot2$ggplot(
      data.frame(x = 1:3, y = 1:3), ggplot2$aes(x = x, y = y)
    ) + ggplot2$geom_line()

    p <- plotting$apply_color_palette(
      plain, "missing_col", data.frame(x = 1:3), "Codedbx", palette_fn
    )
    expect_s3_class(ggplot2$ggplot_build(p), "ggplot_built")
  })
})
