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

describe("palette registry", {
  # Light-mode baselines at n = 8, computed once from the source packages in this renv
  # (RColorBrewer 1.1-3, ggprism 1.0.7, viridisLite 0.4.3, grDevices) and pasted in, so a
  # package bump that changes a colour fails here instead of silently changing plots.
  new_baseline <- list(
    "Dark2" = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666"),
    "Set2" = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3"),
    "Paired" = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00"),
    "prism_light" = c("#2C1453", "#114CE8", "#0E6F7C", "#FB4F06", "#FB0005", "#A48AD3", "#1CC5FE", "#6FC7CF"),
    "prism_dark" = c("#A48AD3", "#1CC5FE", "#6FC7CF", "#FBA27D", "#FB7D80", "#2C1453", "#114CE8", "#0E6F7C"),
    "floral" = c("#285291", "#4F2B8E", "#91188E", "#539027", "#0D405B", "#34274D", "#91181D", "#2C3324"),
    "winter_bright" = c("#077E97", "#800080", "#000080", "#8D8DFF", "#C000C0", "#056943", "#077E97", "#800080"),
    "candy_bright" = c("#F71480", "#FF8000", "#808000", "#008000", "#0000FF", "#76069A", "#F71480", "#FF8000"),
    "pastels" = c("#CCCCFF", "#99CCFF", "#6699CC", "#666699", "#9370DB", "#996699", "#CC6699", "#CCCCFF"),
    "viridis" = c("#440154", "#46337E", "#365C8D", "#277F8E", "#1FA187", "#4AC16D", "#9FDA3A", "#FDE725"),
    "cividis" = c("#00204D", "#16396D", "#4B546C", "#6C6E72", "#8E8A79", "#B3A772", "#DBC761", "#FFEA46"),
    "Grayscale" = c("#333333", "#5A5A5A", "#737373", "#868686", "#979797", "#A6A6A6", "#B3B3B3", "#BFBFBF")
  )

  it("pins the light-mode output of every new palette at n = 8", {
    for (p in names(new_baseline)) {
      expect_identical(utils$get_palette_colors(p, 8), new_baseline[[p]], info = p)
      expect_identical(utils$get_palette_colors(p, 8, dark = FALSE), new_baseline[[p]], info = p)
    }
  })

  it("lists the sixteen palettes in picker order", {
    expect_identical(
      utils$palette_names(),
      c(
        "Codedbx", "Okabe-Ito", "Dark2", "Set2", "Paired",
        "prism_light", "prism_dark", "floral", "winter_bright", "candy_bright", "pastels",
        "viridis", "cividis", "HCL Light", "HCL Dark", "Grayscale"
      )
    )
  })

  it("reports the distinct size of fixed sets and NA for generators and unknown names", {
    sizes <- c(
      "Codedbx" = 6L, "Okabe-Ito" = 8L, "Dark2" = 8L, "Set2" = 8L, "Paired" = 12L,
      "prism_light" = 10L, "prism_dark" = 10L, "floral" = 12L,
      "winter_bright" = 6L, "candy_bright" = 6L, "pastels" = 7L
    )
    for (p in names(sizes)) {
      expect_identical(utils$palette_size(p), sizes[[p]], info = p)
    }
    for (p in c("viridis", "cividis", "HCL Light", "HCL Dark", "Grayscale", "nope")) {
      expect_identical(utils$palette_size(p), NA_integer_, info = p)
    }
  })

  it("stores only distinct colours for the padded ggprism sets", {
    # ggprism pads winter_bright/candy_bright (9 entries, 6 distinct) and pastels (9, 7) by repeating
    # colours; the registry keeps the distinct ones so recycling starts where the repeats really start.
    for (p in c("prism_light", "prism_dark", "floral", "winter_bright", "candy_bright", "pastels")) {
      cols <- utils$get_palette_colors(p, utils$palette_size(p))
      expect_identical(anyDuplicated(cols), 0L, info = p)
      expect_null(names(cols), info = p)
    }
    expect_identical(utils$get_palette_colors("winter_bright", 7)[7], utils$get_palette_colors("winter_bright", 1))
    expect_identical(utils$get_palette_colors("pastels", 8)[8], utils$get_palette_colors("pastels", 1))
  })

  it("groups every palette", {
    expect_identical(utils$palette_group("Codedbx"), "Brand")
    expect_identical(utils$palette_group("Okabe-Ito"), "Colourblind-safe")
    expect_identical(utils$palette_group("Paired"), "Colourblind-safe")
    expect_identical(utils$palette_group("floral"), "Prism")
    expect_identical(utils$palette_group("viridis"), "Generated")
    expect_identical(utils$palette_group("HCL Dark"), "Generated")
    expect_identical(utils$palette_group("Grayscale"), "Print")
    expect_identical(utils$palette_group("nope"), NA_character_)
  })

  it("matches any palette name case-insensitively and still falls back to HCL Light", {
    expect_identical(utils$get_palette_colors("VIRIDIS", 3), utils$get_palette_colors("viridis", 3))
    expect_identical(utils$get_palette_colors("hcl dark", 3), utils$get_palette_colors("HCL Dark", 3))
    expect_identical(utils$get_palette_colors("nope", 5), utils$get_palette_colors("HCL Light", 5))
    expect_identical(utils$palette_group("codedbx"), "Brand")
  })

  it("strips the alpha suffix from viridisLite output", {
    expect_true(all(nchar(utils$get_palette_colors("viridis", 8)) == 7L))
    expect_true(all(nchar(utils$get_palette_colors("cividis", 8)) == 7L))
  })

  it("draws up to eight swatch colours per palette", {
    expect_identical(utils$palette_swatch("Codedbx"), codedbx_hex)
    expect_length(utils$palette_swatch("Paired"), 8)
    expect_length(utils$palette_swatch("viridis"), 8)
    expect_identical(utils$palette_swatch("winter_bright"), utils$get_palette_colors("winter_bright", 6))
    expect_length(utils$palette_swatch("nope"), 8)
  })

  it("lifts every registry palette to at least 3:1 against the dark canvas", {
    for (p in utils$palette_names()) {
      cols <- utils$get_palette_colors(p, 8, dark = TRUE)
      ratios <- vapply(cols, utils$contrast_ratio, numeric(1), b = utils$DARK_CANVAS)
      expect_true(all(ratios >= 3), info = paste(p, paste(round(ratios, 2), collapse = " ")))
      expect_length(cols, 8)
    }
  })
})

describe("palette_preflight", {
  it("exports the two thresholds", {
    expect_identical(utils$PREFLIGHT_MIN_CONTRAST, 1.5)
    expect_identical(utils$PREFLIGHT_MIN_GREY_RATIO, 1.2)
  })

  it("flags recycling when the levels outnumber a fixed set", {
    res <- utils$palette_preflight("Dark2", n_levels = 10)
    expect_identical(res$flags, "recycled")
    expect_identical(res$n, 10L)
    expect_identical(res$message, "10 levels, Dark2 has 8: 2 colours repeat")
    # Qualitative palettes get no greyscale check, so recycling is the only flag here.
    expect_identical(utils$palette_preflight("Codedbx", n_levels = 12)$flags, "recycled")
    # Distinct size, not ggprism's padded length.
    expect_identical(
      utils$palette_preflight("winter_bright", n_levels = 7)$message,
      "7 levels, winter_bright has 6: 1 colour repeats"
    )
  })

  it("flags entries that are faint on white in light mode", {
    res <- utils$palette_preflight("Okabe-Ito", n_levels = 8)
    expect_identical(res$flags, "low_contrast")
    expect_identical(res$message, "Okabe-Ito colour 5 (#F0E442) is 1.3:1 against white: faint")
    res <- utils$palette_preflight("viridis", n_levels = 8)
    expect_identical(res$flags, "low_contrast")
    expect_identical(res$message, "viridis colour 8 (#FDE725) is 1.3:1 against white: faint")
    expect_match(utils$palette_preflight("HCL Light", n_levels = 8)$message, "(and 5 more)", fixed = TRUE)
  })

  it("flags adjacent steps of a sequential ramp that are too close in luminance", {
    res <- utils$palette_preflight("Grayscale", n_levels = 8)
    expect_identical(res$flags, "grayscale_collision")
    expect_match(
      res$message,
      "levels 7 and 8 differ by 14% luminance: hard to tell apart in greyscale",
      fixed = TRUE
    )
    # In dark mode the 3:1 lift pins the dark end of the ramp to one luminance.
    res <- utils$palette_preflight("Grayscale", n_levels = 8, dark = TRUE)
    expect_identical(res$flags, "grayscale_collision")
    expect_match(res$message, "differ by 0% luminance", fixed = TRUE)
    expect_identical(utils$palette_preflight("viridis", n_levels = 3, dark = TRUE)$flags, "grayscale_collision")
  })

  it("raises both notes for viridis at twelve levels and only the contrast note below", {
    res <- utils$palette_preflight("viridis", n_levels = 12)
    expect_setequal(res$flags, c("low_contrast", "grayscale_collision"))
    expect_match(res$message, "; levels ", fixed = TRUE)
    expect_identical(utils$palette_preflight("viridis", n_levels = 11)$flags, "low_contrast")
  })

  it("is silent for the clean cases", {
    cases <- list(
      list("Codedbx", 3, FALSE), list("Codedbx", 6, FALSE), list("Codedbx", 6, TRUE),
      list("Dark2", 8, FALSE), list("Dark2", 8, TRUE), list("Grayscale", 6, FALSE)
    )
    for (case in cases) {
      res <- utils$palette_preflight(case[[1]], n_levels = case[[2]], dark = case[[3]])
      expect_identical(res$flags, character(0), info = paste(case[[1]], case[[2]], case[[3]]))
      expect_null(res$message)
    }
  })

  it("skips the recycling check and uses the palette size when the level count is unknown", {
    res <- utils$palette_preflight("Dark2")
    expect_identical(res$n, 8L)
    expect_false("recycled" %in% res$flags)
    expect_identical(utils$palette_preflight("viridis")$n, 8L)
    expect_identical(utils$palette_preflight("Paired")$n, 12L)
  })

  it("never reports low contrast in dark mode, because the lift already guarantees 3:1", {
    for (p in utils$palette_names()) {
      expect_false("low_contrast" %in% utils$palette_preflight(p, n_levels = 8, dark = TRUE)$flags, info = p)
    }
  })

  it("never runs the greyscale check on a qualitative palette", {
    qualitative <- setdiff(utils$palette_names(), c("viridis", "cividis", "Grayscale"))
    for (p in qualitative) {
      size <- utils$palette_size(p)
      n <- if (is.na(size)) 8L else size
      for (dark in c(FALSE, TRUE)) {
        flags <- utils$palette_preflight(p, n_levels = n, dark = dark)$flags
        expect_false("grayscale_collision" %in% flags, info = paste(p, dark))
      }
    }
  })

  it("copes with one level, zero levels and a missing palette name", {
    expect_identical(utils$palette_preflight("Grayscale", n_levels = 1)$flags, character(0))
    expect_identical(utils$palette_preflight("Grayscale", n_levels = 0)$flags, character(0))
    expect_identical(utils$palette_preflight(NULL)$flags, character(0))
    expect_identical(utils$palette_preflight("")$n, 6L)
    # An unknown name renders as HCL Light, so the note says so.
    expect_match(utils$palette_preflight("nope", n_levels = 8)$message, "HCL Light colour", fixed = TRUE)
  })
})
