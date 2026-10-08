# Tests for app/logic/plot_style.R — the reusable plot style schema.

box::use(
  testthat[...],
)

box::use(
  app / logic / plot_style,
)

nlme_defaults <- list(
  layers = list(
    population = list(show = TRUE, alpha = 0.9, width = 1.0),
    individual = list(show = FALSE, alpha = 0.3, width = 0.6),
    observed = list(show = TRUE, alpha = 0.6, size = 2.0)
  )
)

nlme_plot_formals <- function() {
  formals(getS3method("plot", "beezdemand_nlme", envir = asNamespace("beezdemand")))
}

describe("style_defaults", {
  it("returns the nlme package defaults", {
    expect_equal(plot_style$style_defaults("beezdemand_nlme"), nlme_defaults)
    expect_equal(plot_style$style_defaults(), nlme_defaults)
  })

  it("matches the installed beezdemand nlme plot method (contract)", {
    skip_if_not_installed("beezdemand")
    f <- nlme_plot_formals()
    d <- plot_style$style_defaults("beezdemand_nlme")$layers
    expect_equal(d$population$alpha, f$pop_line_alpha)
    expect_equal(d$population$width, f$pop_line_size)
    expect_equal(d$individual$alpha, f$ind_line_alpha)
    expect_equal(d$individual$width, f$ind_line_size)
    expect_equal(d$observed$alpha, f$observed_point_alpha)
    expect_equal(d$observed$size, f$observed_point_size)
  })

  it("knows the tmb engine defaults", {
    d <- plot_style$style_defaults("beezdemand_tmb")$layers
    expect_equal(d$population, list(show = TRUE, alpha = 1.0, width = 1.2))
    expect_equal(d$individual, list(show = FALSE, alpha = 0.3, width = 0.5))
    expect_equal(d$observed, list(show = TRUE, alpha = 0.3, size = 1.5))
  })

  it("errors on an unknown engine", {
    expect_error(plot_style$style_defaults("ggplot"), "Unknown plot engine")
  })
})

describe("prominence_defaults", {
  it("gives the slider start positions per engine", {
    expect_equal(plot_style$prominence_defaults("beezdemand_nlme"), list(population = 75, individual = 25))
    expect_equal(plot_style$prominence_defaults("beezdemand_tmb"), list(population = 100, individual = 20))
  })

  it("start positions reproduce the nlme population and individual defaults", {
    p <- plot_style$prominence_defaults("beezdemand_nlme")
    expect_equal(
      plot_style$prominence_to_layer(p$population / 100, "population"),
      list(alpha = 0.9, width = 1.0)
    )
    expect_equal(
      plot_style$prominence_to_layer(p$individual / 100, "individual"),
      list(alpha = 0.3, width = 0.6)
    )
  })
})

describe("prominence_to_layer", {
  it("maps the population layer linearly and caps alpha at 1", {
    expect_equal(plot_style$prominence_to_layer(0, "population"), list(alpha = 0.3, width = 0.4))
    expect_equal(plot_style$prominence_to_layer(0.25, "population"), list(alpha = 0.5, width = 0.6))
    expect_equal(plot_style$prominence_to_layer(1, "population"), list(alpha = 1.0, width = 1.2))
  })

  it("maps the individual layer linearly", {
    expect_equal(plot_style$prominence_to_layer(0, "individual"), list(alpha = 0.1, width = 0.2))
    expect_equal(plot_style$prominence_to_layer(0.75, "individual"), list(alpha = 0.7, width = 1.4))
    expect_equal(plot_style$prominence_to_layer(1, "individual"), list(alpha = 0.9, width = 1.8))
  })

  it("clamps p into [0, 1]", {
    expect_equal(
      plot_style$prominence_to_layer(-3, "population"),
      plot_style$prominence_to_layer(0, "population")
    )
    expect_equal(
      plot_style$prominence_to_layer(7, "individual"),
      plot_style$prominence_to_layer(1, "individual")
    )
  })

  it("rejects a non-numeric or NA p", {
    expect_error(plot_style$prominence_to_layer(NA_real_, "population"), "single number")
    expect_error(plot_style$prominence_to_layer("a", "population"), "single number")
  })
})

describe("validate_style", {
  it("returns the defaults for NULL or a non-list", {
    expect_equal(plot_style$validate_style(NULL), nlme_defaults)
    expect_equal(plot_style$validate_style("nope"), nlme_defaults)
  })

  it("clamps alpha, width and size at both bounds", {
    s <- nlme_defaults
    s$layers$population$alpha <- 5
    s$layers$individual$alpha <- -1
    s$layers$population$width <- 10
    s$layers$individual$width <- 0
    s$layers$observed$size <- 100
    v <- plot_style$validate_style(s)$layers
    expect_equal(v$population$alpha, 1)
    expect_equal(v$individual$alpha, 0.05)
    expect_equal(v$population$width, 4)
    expect_equal(v$individual$width, 0.1)
    expect_equal(v$observed$size, 8)
  })

  it("replaces NA, NULL and non-numeric values with the engine default for that field only", {
    s <- nlme_defaults
    s$layers$population$alpha <- NA
    s$layers$observed$size <- "x"
    s$layers$individual$width <- 1.5
    v <- plot_style$validate_style(s)$layers
    expect_equal(v$population$alpha, 0.9)
    expect_equal(v$observed$size, 2.0)
    expect_equal(v$individual$width, 1.5)
  })

  it("defaults a missing show flag to the engine value and coerces a present one with isTRUE", {
    s <- list(layers = list(population = list(alpha = 0.5), individual = list(show = "yes")))
    v <- plot_style$validate_style(s)$layers
    expect_true(v$population$show)
    expect_false(v$individual$show)
    expect_true(v$observed$show)
  })

  it("drops unknown keys and returns the canonical shape", {
    s <- nlme_defaults
    s$layers$population$colour <- "red"
    s$extra <- 1
    v <- plot_style$validate_style(s)
    expect_identical(names(v), "layers")
    expect_identical(names(v$layers), c("population", "individual", "observed"))
    expect_identical(names(v$layers$population), c("show", "alpha", "width"))
    expect_identical(names(v$layers$observed), c("show", "alpha", "size"))
    expect_type(v$layers$population$alpha, "double")
  })

  it("coerces numeric strings", {
    s <- nlme_defaults
    s$layers$population$alpha <- "0.4"
    expect_equal(plot_style$validate_style(s)$layers$population$alpha, 0.4)
  })

  it("does not partial-match unknown keys onto schema fields", {
    prefixed_layers <- plot_style$validate_style(list(layers_x = list(population = list(alpha = 0.2))))
    expect_equal(prefixed_layers$layers$population$alpha, 0.9)
    prefixed_fields <- list(layers = list(population = list(alpha_extra = 0.2, showing = FALSE)))
    v <- plot_style$validate_style(prefixed_fields)
    expect_equal(v$layers$population$alpha, 0.9)
    expect_true(v$layers$population$show)
  })
})

describe("limits and engines", {
  it("exposes the clamp ranges and the known engines", {
    expect_equal(plot_style$limits(), list(alpha = c(0.05, 1), width = c(0.1, 4), size = c(0.5, 8)))
    expect_equal(plot_style$engines(), c("beezdemand_nlme", "beezdemand_tmb"))
  })
})

describe("plot_args_from_style", {
  it("maps the nlme defaults onto the plot() argument names", {
    args <- plot_style$plot_args_from_style(nlme_defaults, "beezdemand_nlme")
    expect_equal(args, list(
      show_observed = TRUE,
      show_pred = "population",
      observed_point_alpha = 0.6,
      observed_point_size = 2.0,
      pop_line_alpha = 0.9,
      pop_line_size = 1.0,
      ind_line_alpha = 0.3,
      ind_line_size = 0.6
    ))
  })

  it("only uses argument names the installed plot method accepts (contract)", {
    skip_if_not_installed("beezdemand")
    f <- names(nlme_plot_formals())
    args <- plot_style$plot_args_from_style(nlme_defaults, "beezdemand_nlme")
    expect_true(all(names(args) %in% f), info = paste(setdiff(names(args), f), collapse = ", "))
  })

  it("builds show_pred from the two line flags", {
    s <- nlme_defaults
    s$layers$population$show <- FALSE
    s$layers$individual$show <- FALSE
    expect_false(plot_style$plot_args_from_style(s)$show_pred)
    s$layers$individual$show <- TRUE
    expect_equal(plot_style$plot_args_from_style(s)$show_pred, "individual")
    s$layers$population$show <- TRUE
    expect_equal(plot_style$plot_args_from_style(s)$show_pred, c("population", "individual"))
  })

  it("validates before mapping", {
    s <- nlme_defaults
    s$layers$population$alpha <- 9
    expect_equal(plot_style$plot_args_from_style(s)$pop_line_alpha, 1)
  })

  it("refuses the tmb engine until it is wired", {
    expect_error(plot_style$plot_args_from_style(nlme_defaults, "beezdemand_tmb"), "not wired")
  })

  it("errors on an unknown engine", {
    expect_error(plot_style$plot_args_from_style(nlme_defaults, "lm"), "Unknown plot engine")
  })
})

describe("has_content", {
  it("is FALSE only when every layer is hidden", {
    s <- nlme_defaults
    expect_true(plot_style$has_content(s))
    s$layers$population$show <- FALSE
    s$layers$observed$show <- FALSE
    expect_false(plot_style$has_content(s))
    s$layers$individual$show <- TRUE
    expect_true(plot_style$has_content(s))
  })
})

describe("style_telemetry_payload", {
  it("flattens to nine fixed names, rounded to 2 dp, with extras appended", {
    s <- nlme_defaults
    s$layers$population$alpha <- 0.123456
    out <- plot_style$style_telemetry_payload(s, extras = list(palette = "Codedbx", dark_mode = FALSE))
    expect_identical(
      names(out),
      c(
        "pop_show", "pop_alpha", "pop_width", "ind_show", "ind_alpha", "ind_width",
        "obs_show", "obs_alpha", "obs_size", "palette", "dark_mode"
      )
    )
    expect_equal(out$pop_alpha, 0.12)
    expect_true(out$pop_show)
    expect_false(out$ind_show)
    expect_equal(out$palette, "Codedbx")
  })

  it("works with no extras", {
    expect_length(plot_style$style_telemetry_payload(nlme_defaults), 9)
  })
})

describe("snapshot_recorder", {
  it("records once per distinct (payload, generation) and reports whether it recorded", {
    seen <- list()
    rec <- plot_style$snapshot_recorder(function(payload) seen[[length(seen) + 1]] <<- payload)
    p1 <- list(pop_alpha = 0.9, palette = "Codedbx")
    expect_true(rec(p1, 1L))
    expect_false(rec(p1, 1L))
    expect_length(seen, 1)
    expect_true(rec(p1, 2L))
    p2 <- p1
    p2$pop_alpha <- 0.5
    expect_true(rec(p2, 2L))
    expect_false(rec(p2, 2L))
    expect_length(seen, 3)
    expect_equal(seen[[3]], p2)
  })
})
