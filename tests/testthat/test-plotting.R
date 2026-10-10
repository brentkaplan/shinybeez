# Tests for plotting module

box::use(
  app / logic / mixed_effects / plotting
)

# Test validate_aesthetic
describe("validate_aesthetic", {
  it("returns NULL for NULL selection", {
    result <- plotting$validate_aesthetic(NULL, c("A", "B"))
    expect_null(result)
  })

  it("returns NULL for empty string selection", {
    result <- plotting$validate_aesthetic("", c("A", "B"))
    expect_null(result)
  })

  it("returns NULL for invalid selection not in factors", {
    result <- plotting$validate_aesthetic("C", c("A", "B"))
    expect_null(result)
  })

  it("returns selection if valid", {
    result <- plotting$validate_aesthetic("A", c("A", "B"))
    expect_equal(result, "A")
  })
})

# Test compute_aesthetic_defaults
describe("compute_aesthetic_defaults", {
  it("resets invalid selections to empty string", {
    result <- plotting$compute_aesthetic_defaults(
      current_color = "invalid",
      current_linetype = "invalid",
      current_facet = "invalid",
      factors_in_model = c("A", "B")
    )
    # Invalid selections get reset, then smart defaults applied
    expect_equal(result$color, "A") # First factor as default
    expect_equal(result$linetype, "B") # Second factor as default
    expect_equal(result$facet, "")
  })

  it("preserves valid selections", {
    result <- plotting$compute_aesthetic_defaults(
      current_color = "B",
      current_linetype = "A",
      current_facet = "",
      factors_in_model = c("A", "B")
    )
    expect_equal(result$color, "B")
    expect_equal(result$linetype, "A")
    expect_equal(result$facet, "")
  })

  it("sets smart defaults when selections are empty and factors exist", {
    result <- plotting$compute_aesthetic_defaults(
      current_color = "",
      current_linetype = "",
      current_facet = "",
      factors_in_model = c("Factor1", "Factor2")
    )
    expect_equal(result$color, "Factor1")
    expect_equal(result$linetype, "Factor2")
    expect_equal(result$facet, "")
  })

  it("handles single factor", {
    result <- plotting$compute_aesthetic_defaults(
      current_color = "",
      current_linetype = "",
      current_facet = "",
      factors_in_model = c("OnlyFactor")
    )
    expect_equal(result$color, "OnlyFactor")
    expect_equal(result$linetype, "")
    expect_equal(result$facet, "")
  })

  it("handles no factors", {
    result <- plotting$compute_aesthetic_defaults(
      current_color = "",
      current_linetype = "",
      current_facet = "",
      factors_in_model = character(0)
    )
    expect_equal(result$color, "")
    expect_equal(result$linetype, "")
    expect_equal(result$facet, "")
  })

  it("does not set color if linetype already uses first factor", {
    result <- plotting$compute_aesthetic_defaults(
      current_color = "",
      current_linetype = "A",
      current_facet = "",
      factors_in_model = c("A", "B")
    )
    expect_equal(result$color, "") # Not A, since linetype has it
    expect_equal(result$linetype, "A")
  })

  it("keeps a valid shape and resets an invalid one", {
    keep <- plotting$compute_aesthetic_defaults("", "", "", c("A", "B"), current_shape = "B")
    expect_equal(keep$shape, "B")
    drop <- plotting$compute_aesthetic_defaults("", "", "", c("A", "B"), current_shape = "gone")
    expect_equal(drop$shape, "")
    default <- plotting$compute_aesthetic_defaults("", "", "", character(0))
    expect_equal(default$shape, "")
  })

  it("treats NULL, NA and zero-length current selections as None", {
    for (empty in list(NULL, NA, NA_character_, character(0))) {
      shape <- plotting$compute_aesthetic_defaults("", "", "", c("A", "B"), current_shape = empty)
      expect_equal(shape$shape, "")
      color <- plotting$compute_aesthetic_defaults(empty, "", "", c("A", "B"))
      expect_equal(color$color, "A")
      linetype <- plotting$compute_aesthetic_defaults("", empty, "", c("A", "B"))
      expect_equal(linetype$linetype, "B")
      facet <- plotting$compute_aesthetic_defaults("", "", empty, c("A", "B"))
      expect_equal(facet$facet, "")
    }
  })
})

describe("aesthetics_in_sync", {
  sel <- function(color = "", linetype = "", facet = "", shape = "") {
    list(color = color, linetype = linetype, facet = facet, shape = shape)
  }
  it("is TRUE when every selection matches", {
    expect_true(plotting$aesthetics_in_sync(sel("drug", "dose"), sel("drug", "dose")))
  })
  it("is FALSE when one selection still lags", {
    expect_false(plotting$aesthetics_in_sync(sel("dose"), sel("drug")))
    expect_false(plotting$aesthetics_in_sync(sel(shape = "drug"), sel()))
  })
  it("treats NULL, NA and empty as None", {
    current <- list(color = NULL, linetype = NA_character_, facet = character(0), shape = "")
    expect_true(plotting$aesthetics_in_sync(sel(), current))
  })
})

# Test build_facet_formula
describe("build_facet_formula", {
  it("returns NULL for NULL facet_var", {
    result <- plotting$build_facet_formula(NULL, c("A", "B"))
    expect_null(result)
  })

  it("returns NULL for empty string facet_var", {
    result <- plotting$build_facet_formula("", c("A", "B"))
    expect_null(result)
  })

  it("returns NULL for invalid facet_var", {
    result <- plotting$build_facet_formula("C", c("A", "B"))
    expect_null(result)
  })

  it("returns formula for valid facet_var", {
    result <- plotting$build_facet_formula("A", c("A", "B"))
    expect_s3_class(result, "formula")
    expect_equal(as.character(result)[2], "A")
  })
})

# Test build_validated_aesthetics
describe("build_validated_aesthetics", {
  it("validates and returns all aesthetics", {
    result <- plotting$build_validated_aesthetics(
      color_input = "A",
      linetype_input = "B",
      facet_input = "A",
      valid_factors = c("A", "B")
    )
    expect_equal(result$color, "A")
    expect_equal(result$linetype, "B")
    expect_null(result$shape)
    expect_s3_class(result$facet_formula, "formula")
  })

  it("returns NULL for invalid inputs", {
    result <- plotting$build_validated_aesthetics(
      color_input = "invalid",
      linetype_input = "",
      facet_input = NULL,
      valid_factors = c("A", "B")
    )
    expect_null(result$color)
    expect_null(result$linetype)
    expect_null(result$shape)
    expect_null(result$facet_formula)
  })

  it("validates shape_input when provided", {
    result <- plotting$build_validated_aesthetics(
      color_input = "A",
      linetype_input = "B",
      facet_input = "",
      valid_factors = c("A", "B"),
      shape_input = "A"
    )
    expect_equal(result$shape, "A")
  })

  it("returns NULL for invalid shape_input", {
    result <- plotting$build_validated_aesthetics(
      color_input = "A",
      linetype_input = "B",
      facet_input = "",
      valid_factors = c("A", "B"),
      shape_input = "invalid"
    )
    expect_null(result$shape)
  })
})

# Test apply_plot_theme (requires ggplot2)
describe("apply_plot_theme", {
  it("applies prism theme", {
    skip_if_not_installed("ggprism")
    box::use(ggplot2)
    p <- ggplot2$ggplot()
    result <- plotting$apply_plot_theme(p, "prism", 14)
    expect_s3_class(result, "gg")
  })

  it("applies classic theme", {
    box::use(ggplot2)
    p <- ggplot2$ggplot()
    result <- plotting$apply_plot_theme(p, "classic", 14)
    expect_s3_class(result, "gg")
  })

  it("applies minimal theme", {
    box::use(ggplot2)
    p <- ggplot2$ggplot()
    result <- plotting$apply_plot_theme(p, "minimal", 14)
    expect_s3_class(result, "gg")
  })

  it("returns unchanged plot for unknown theme", {
    box::use(ggplot2)
    p <- ggplot2$ggplot()
    result <- plotting$apply_plot_theme(p, "unknown", 14)
    expect_s3_class(result, "gg")
  })
})

# Test apply_legend_position
describe("apply_legend_position", {
  it("applies legend position", {
    box::use(ggplot2)
    p <- ggplot2$ggplot()
    result <- plotting$apply_legend_position(p, "bottom")
    expect_s3_class(result, "gg")
  })

  it("defaults to 'right' when NULL", {
    box::use(ggplot2)
    p <- ggplot2$ggplot()
    result <- plotting$apply_legend_position(p, NULL)
    expect_s3_class(result, "gg")
  })
})

# Test apply_color_palette
describe("apply_color_palette", {
  it("returns unchanged plot when color_var is NULL", {
    box::use(ggplot2)
    p <- ggplot2$ggplot()
    result <- plotting$apply_color_palette(
      p,
      color_var = NULL,
      fit_data = data.frame(A = c(1, 2, 3)),
      palette_name = "default",
      get_palette_fn = function(name, n, dark = FALSE) rep("blue", n)
    )
    expect_s3_class(result, "gg")
  })

  it("applies palette when color_var exists in data", {
    box::use(ggplot2)
    p <- ggplot2$ggplot()
    result <- plotting$apply_color_palette(
      p,
      color_var = "group",
      fit_data = data.frame(group = c("A", "B", "A", "B")),
      palette_name = "default",
      get_palette_fn = function(name, n, dark = FALSE) rep("blue", n)
    )
    expect_s3_class(result, "gg")
  })
})
