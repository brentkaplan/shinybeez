# Tests for general logic utilities (palette helpers)

box::use(
  app / logic / utils
)

# codedbx "Refined Contemporary" brand palette (6 colors)
codedbx_hex <- c(
  "#534B7A",
  "#A25F5F",
  "#5D8AA8",
  "#7D9C7F",
  "#2B4560",
  "#B08C6A"
)

describe("get_palette_colors - codedbx brand palette", {
  it("returns the six brand colors in order for n = 6", {
    expect_equal(utils$get_palette_colors("Codedbx", 6), codedbx_hex)
  })

  it("returns the first n brand colors for n < 6", {
    expect_equal(utils$get_palette_colors("Codedbx", 3), codedbx_hex[1:3])
  })

  it("recycles brand colors for n > 6", {
    result <- utils$get_palette_colors("Codedbx", 8)
    expect_length(result, 8)
    expect_equal(result[1:6], codedbx_hex)
    expect_equal(result[7:8], codedbx_hex[1:2])
  })

  it("matches the palette name case-insensitively", {
    expect_equal(utils$get_palette_colors("codedbx", 6), codedbx_hex)
  })

  it("is the default palette when no name is supplied", {
    expect_equal(utils$get_palette_colors(n = 4), codedbx_hex[1:4])
  })

  it("falls back to the brand palette for empty or NULL names", {
    expect_equal(utils$get_palette_colors("", 2), codedbx_hex[1:2])
    expect_equal(utils$get_palette_colors(NULL, 2), codedbx_hex[1:2])
  })
})

describe("get_palette_colors - existing palettes are preserved", {
  it("returns Okabe-Ito colorblind-safe colors when requested", {
    expect_equal(
      utils$get_palette_colors("Okabe-Ito", 3),
      c("#000000", "#E69F00", "#56B4E9")
    )
  })

  it("returns n colors for the HCL palettes", {
    expect_length(utils$get_palette_colors("HCL Light", 5), 5)
    expect_length(utils$get_palette_colors("HCL Dark", 5), 5)
  })

  it("returns an empty vector for non-positive n", {
    expect_length(utils$get_palette_colors("Codedbx", 0), 0)
  })
})

describe("apply_dark_mode_theme", {
  it("returns unchanged plot when mode is light", {
    box::use(ggplot2)
    p <- ggplot2$ggplot()
    result <- utils$apply_dark_mode_theme(p, "light")
    expect_s3_class(result, "gg")
  })

  it("paints the dark page background in dark mode", {
    box::use(ggplot2)
    p <- ggplot2$ggplot() + ggplot2$geom_point(ggplot2$aes(1, 1))
    result <- utils$apply_dark_mode_theme(p, "dark")
    expect_s3_class(result, "gg")
    built <- ggplot2$ggplot_build(result)
    theme <- built$plot$theme
    # Solid fill matching the app's dark --bs-body-bg so the rendered PNG
    # blends with the page/card (an opaque canvas would show through a
    # transparent fill).
    expect_equal(theme$plot.background$fill, "#2d2d2d")
    expect_equal(theme$panel.background$fill, "#2d2d2d")
  })

  it("defaults to light when called with no argument", {
    box::use(ggplot2)
    p <- ggplot2$ggplot()
    result <- utils$apply_dark_mode_theme(p)
    expect_s3_class(result, "gg")
  })
})

describe("apply_dark_mode_theme - geom contrast", {
  df <- data.frame(x = 1:3, y = 1:3, g = c("a", "b", "a"))

  it("lightens a default-colored (black) geom so it shows on the dark canvas", {
    box::use(ggplot2)
    p <- ggplot2$ggplot(df, ggplot2$aes(x, y)) + ggplot2$geom_line()
    result <- utils$apply_dark_mode_theme(p, "dark")
    expect_equal(result$layers[[1]]$aes_params$colour, "#dee2e6")
  })

  it("preserves colour that is mapped to data (the brand palette)", {
    box::use(ggplot2)
    p <- ggplot2$ggplot(df, ggplot2$aes(x, y, colour = g)) + ggplot2$geom_line()
    result <- utils$apply_dark_mode_theme(p, "dark")
    expect_null(result$layers[[1]]$aes_params$colour)
  })

  it("preserves an explicit saturated colour (e.g. a red fit line)", {
    box::use(ggplot2)
    p <- ggplot2$ggplot(df, ggplot2$aes(x, y)) +
      ggplot2$geom_line(colour = "red")
    result <- utils$apply_dark_mode_theme(p, "dark")
    expect_equal(result$layers[[1]]$aes_params$colour, "red")
  })

  it("preserves a light fill such as white points", {
    box::use(ggplot2)
    p <- ggplot2$ggplot(df, ggplot2$aes(x, y)) +
      ggplot2$geom_point(shape = 21, fill = "white")
    result <- utils$apply_dark_mode_theme(p, "dark")
    expect_equal(result$layers[[1]]$aes_params$fill, "white")
  })

  it("does not recolor geoms in light mode", {
    box::use(ggplot2)
    p <- ggplot2$ggplot(df, ggplot2$aes(x, y)) + ggplot2$geom_line()
    result <- utils$apply_dark_mode_theme(p, "light")
    expect_null(result$layers[[1]]$aes_params$colour)
  })
})

describe("contrast helpers", {
  it("computes WCAG relative luminance and contrast", {
    expect_equal(utils$relative_luminance("#FFFFFF"), 1)
    expect_equal(utils$relative_luminance("#000000"), 0)
    expect_equal(utils$contrast_ratio("#FFFFFF", "#000000"), 21)
    expect_equal(utils$contrast_ratio("#000000", "#FFFFFF"), 21)
  })

  it("exports the dark canvas colour used by the dark theme", {
    expect_identical(utils$DARK_CANVAS, "#2d2d2d")
  })

  it("lightens a colour along a ramp to white until it reaches the target", {
    expect_identical(utils$lighten_to_contrast("#000000"), "#777777")
    expect_identical(utils$lighten_to_contrast("#2B4560"), "#66798C")
    expect_identical(utils$lighten_to_contrast("#534B7A"), "#787297")
    expect_gte(utils$contrast_ratio(utils$lighten_to_contrast("#000000"), utils$DARK_CANVAS), 3)
  })

  it("returns a colour unchanged when it already meets the target", {
    expect_identical(utils$lighten_to_contrast("#5D8AA8"), "#5D8AA8")
    expect_identical(utils$lighten_to_contrast("#FFFFFF"), "#FFFFFF")
  })
})

describe("get_palette_colors - dark mode", {
  palettes <- c("Codedbx", "Okabe-Ito", "HCL Light", "HCL Dark")

  # Baselines were computed from develop's pre-branch get_palette_colors (b58ecf2), so they pin the
  # historical light-mode output rather than comparing the function with itself.
  light_baseline <- list(
    "Codedbx" = rep(codedbx_hex, length.out = 8),
    "Okabe-Ito" = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
    "HCL Light" = c("#FFC4C0", "#EFD09E", "#C4DD9D", "#93E5BE", "#83E4E7", "#AED9FF", "#E5C9FF", "#FFC0EA"),
    "HCL Dark" = c("#BC3F33", "#956300", "#497A00", "#00882D", "#008C91", "#0078CD", "#973CD2", "#C80099")
  )

  it("keeps the light-mode output identical to the pre-dark-mode palettes (n = 8)", {
    for (p in palettes) {
      expect_identical(utils$get_palette_colors(p, 8), light_baseline[[p]], info = p)
      expect_identical(utils$get_palette_colors(p, 8, dark = FALSE), light_baseline[[p]], info = p)
    }
  })

  it("lifts every entry to at least 3:1 against the dark canvas when dark = TRUE", {
    for (p in palettes) {
      cols <- utils$get_palette_colors(p, 8, dark = TRUE)
      ratios <- vapply(cols, utils$contrast_ratio, numeric(1), b = utils$DARK_CANVAS)
      expect_true(all(ratios >= 3), info = paste(p, paste(round(ratios, 2), collapse = " ")))
      expect_length(cols, 8)
    }
  })

  it("keeps entries that already pass byte-identical, so group identity holds across modes", {
    light <- utils$get_palette_colors("Codedbx", 6)
    dark <- utils$get_palette_colors("Codedbx", 6, dark = TRUE)
    passing <- vapply(light, utils$contrast_ratio, numeric(1), b = utils$DARK_CANVAS) >= 3
    expect_true(any(passing) && any(!passing))
    expect_identical(dark[passing], light[passing])
    expect_false(identical(dark[!passing], light[!passing]))
  })

  it("turns Okabe-Ito black into the lightened grey", {
    expect_identical(utils$get_palette_colors("Okabe-Ito", 1, dark = TRUE), "#777777")
  })

  it("lightens after recycling so repeated entries stay identical", {
    cols <- utils$get_palette_colors("Codedbx", 12, dark = TRUE)
    expect_identical(cols[1:6], cols[7:12])
  })
})

describe("resolve_group_scale - dark mode", {
  it("uses the dark palette values", {
    scale <- utils$resolve_group_scale(c("a", "b", "c"), "Okabe-Ito", dark = TRUE)
    expect_identical(scale$palette(3), utils$get_palette_colors("Okabe-Ito", 3, dark = TRUE))
  })
})
