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
