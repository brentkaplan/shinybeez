box::use(
  ggplot2,
  ggprism,
  grid,
  png,
  grDevices,
  RColorBrewer,
  viridisLite,
)

#' @export
get_png_br <- function(filename, height_pt = 50, alpha = 0.9) {
  grid$rasterGrob(
    png$readPNG(filename),
    interpolate = TRUE,
    x = grid$unit(1, "npc"),
    y = grid$unit(0, "npc") + grid$unit(35, "pt"),
    height = grid$unit(height_pt, "pt"),
    hjust = 1,
    vjust = 1,
    gp = grid$gpar(alpha = alpha)
  )
}

#' @export
get_png_tr <- function(filename, height_pt = 50, alpha = 0.9) {
  grid$rasterGrob(
    png$readPNG(filename),
    interpolate = TRUE,
    x = grid$unit(1, "npc") - grid$unit(10, "pt"),
    y = grid$unit(1, "npc"),
    height = grid$unit(height_pt, "pt"),
    hjust = 1,
    vjust = 1,
    gp = grid$gpar(alpha = alpha)
  )
}

#' @export
add_shiny_logo <- function(logo) {
  list(
    ggplot2$annotation_custom(logo),
    ggplot2$coord_cartesian(clip = "off"),
    ggplot2$theme(plot.margin = ggplot2$unit(c(1, 1, 3, 1), "lines"))
  )
}

# Eagerly load the decorative watermark grobs. Wrapped in tryCatch so the
# module stays importable when the working directory is not the app root
# (e.g. testthat runs from tests/testthat, where the relative path won't
# resolve). The grobs are only consumed by render paths, which always run
# with the app root as the working directory, so production behaviour is
# unchanged; only out-of-app contexts get NULL.
#' @export
watermark_br <- tryCatch(
  get_png_br("./app/static/img/shinybeez-watermark-alpha.png"),
  error = function(e) NULL
)

#' @export
watermark_tr <- tryCatch(
  get_png_tr("./app/static/img/shinybeez-watermark-alpha.png"),
  error = function(e) NULL
)

# -----------------------------------------------------------------------------
# Palette helpers (discrete)
# -----------------------------------------------------------------------------

#' Canvas colour painted behind plots in dark mode
#' @export
DARK_CANVAS <- "#2d2d2d" # nolint: object_name_linter

#' WCAG 2 relative luminance of one colour
#' @export
relative_luminance <- function(col) {
  rgb <- grDevices$col2rgb(col)[, 1] / 255
  lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  sum(c(0.2126, 0.7152, 0.0722) * lin)
}

#' WCAG 2 contrast ratio between two colours (symmetric, 1..21)
#' @export
contrast_ratio <- function(a, b) {
  la <- relative_luminance(a)
  lb <- relative_luminance(b)
  (max(la, lb) + 0.05) / (min(la, lb) + 0.05)
}

#' Lighten a colour toward white just enough to reach `target` contrast on `bg`
#'
#' Hue is kept (the ramp runs from the colour to white); a colour that already
#' meets the target is returned unchanged.
#' @export
lighten_to_contrast <- function(col, bg = DARK_CANVAS, target = 3) {
  if (contrast_ratio(col, bg) >= target) {
    return(col)
  }
  ramp <- grDevices$colorRampPalette(c(col, "#FFFFFF"))(101)
  ok <- vapply(ramp, function(x) contrast_ratio(x, bg) >= target, logical(1))
  if (!any(ok)) {
    return("#FFFFFF")
  }
  ramp[[which(ok)[1]]]
}

# -----------------------------------------------------------------------------
# Palette registry
# -----------------------------------------------------------------------------
#
# Every discrete palette the app offers, in picker order. A fixed set carries its
# colours (unnamed 6-digit hex, distinct) and recycles past its size; a generator is
# a function of n. `sequential` marks the luminance-ordered ramps, the only ones the
# greyscale preflight check applies to.

hcl_hues <- function(n) {
  seq(15, 375, length.out = n + 1)[1:n]
}

# ggprism pads some palettes by repeating colours (winter_bright 9 entries / 6 distinct);
# keep the distinct ones so the size and the recycling note are truthful.
prism_colors <- function(name) {
  unique(unname(ggprism$ggprism_data$colour_palettes[[name]]))
}

fixed_palette <- function(group, colors) {
  list(group = group, colors = colors, generator = NULL, sequential = FALSE)
}

generated_palette <- function(group, generator, sequential = FALSE) {
  list(group = group, colors = NULL, generator = generator, sequential = sequential)
}

PALETTES <- list( # nolint: object_name_linter
  "Codedbx" = fixed_palette(
    "Brand",
    c("#534B7A", "#A25F5F", "#5D8AA8", "#7D9C7F", "#2B4560", "#B08C6A")
  ),
  "Okabe-Ito" = fixed_palette(
    "Colourblind-safe",
    c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  ),
  "Dark2" = fixed_palette("Colourblind-safe", RColorBrewer$brewer.pal(8, "Dark2")),
  "Set2" = fixed_palette("Colourblind-safe", RColorBrewer$brewer.pal(8, "Set2")),
  "Paired" = fixed_palette("Colourblind-safe", RColorBrewer$brewer.pal(12, "Paired")),
  "prism_light" = fixed_palette("Prism", prism_colors("prism_light")),
  "prism_dark" = fixed_palette("Prism", prism_colors("prism_dark")),
  "floral" = fixed_palette("Prism", prism_colors("floral")),
  "winter_bright" = fixed_palette("Prism", prism_colors("winter_bright")),
  "candy_bright" = fixed_palette("Prism", prism_colors("candy_bright")),
  "pastels" = fixed_palette("Prism", prism_colors("pastels")),
  "viridis" = generated_palette(
    "Generated",
    function(n) substr(viridisLite$viridis(n), 1, 7),
    sequential = TRUE
  ),
  "cividis" = generated_palette(
    "Generated",
    function(n) substr(viridisLite$cividis(n), 1, 7),
    sequential = TRUE
  ),
  "HCL Light" = generated_palette("Generated", function(n) grDevices$hcl(h = hcl_hues(n), c = 45, l = 85)),
  "HCL Dark" = generated_palette("Generated", function(n) grDevices$hcl(h = hcl_hues(n), c = 100, l = 45)),
  "Grayscale" = generated_palette(
    "Print",
    function(n) grDevices$gray.colors(n, start = 0.2, end = 0.75),
    sequential = TRUE
  )
)

# Canonical registry name for a user-supplied one, or NA when nothing matches. Exact
# match first, then case-insensitive. NULL, NA, empty and non-length-1 input give NA.
match_palette_name <- function(name) {
  if (is.null(name) || length(name) != 1L) {
    return(NA_character_)
  }
  name <- as.character(name)
  if (is.na(name) || !nzchar(name)) {
    return(NA_character_)
  }
  if (!is.null(PALETTES[[name]])) {
    return(name)
  }
  hit <- match(tolower(name), tolower(names(PALETTES)))
  if (is.na(hit)) NA_character_ else names(PALETTES)[[hit]]
}

# Registry entry for a name: exact match first, then case-insensitive; NULL when unknown.
palette_entry <- function(name) {
  canonical <- match_palette_name(name)
  if (is.na(canonical)) NULL else PALETTES[[canonical]]
}

# Canonical registry name for a user-supplied one. Empty or NULL means the brand palette
# (the app default); an unknown name falls back to HCL Light, as it always has.
resolve_palette_name <- function(name) {
  if (is.null(name) || length(name) != 1L) {
    return("Codedbx")
  }
  name <- as.character(name)
  if (is.na(name) || !nzchar(name)) {
    return("Codedbx")
  }
  canonical <- match_palette_name(name)
  if (is.na(canonical)) "HCL Light" else canonical
}

#' Palette names in picker order
#' @export
palette_names <- function() {
  names(PALETTES)
}

#' Picker group of a palette (NA for an unknown name)
#' @export
palette_group <- function(name) {
  entry <- palette_entry(name)
  if (is.null(entry)) NA_character_ else entry$group
}

#' Number of distinct colours in a fixed palette; NA for a generator or an unknown name
#' @export
palette_size <- function(name) {
  entry <- palette_entry(name)
  if (is.null(entry) || is.null(entry$colors)) NA_integer_ else length(entry$colors)
}

# TRUE for the luminance-ordered ramps (viridis, cividis, Grayscale).
palette_is_sequential <- function(name) {
  entry <- palette_entry(name)
  !is.null(entry) && isTRUE(entry$sequential)
}

#' The colours the picker draws for a palette: its whole fixed set up to eight, or eight
#' steps of a generator. Light mode, so the swatches match the classic palettes.
#' @export
palette_swatch <- function(name) {
  size <- palette_size(name)
  n <- if (is.na(size)) 8L else min(8L, size)
  get_palette_colors(name, n, dark = FALSE)
}

# Light-mode base palette (internal). Public entry point is get_palette_colors().
palette_base <- function(name = "Codedbx", n = 2L) {
  if (is.null(n) || is.na(n) || n <= 0) {
    return(character(0))
  }
  entry <- PALETTES[[resolve_palette_name(name)]]
  if (is.null(entry$generator)) {
    rep(entry$colors, length.out = n)
  } else {
    entry$generator(as.integer(n))
  }
}

#' Get a vector of colors for a named discrete palette
#'
#' @param name Character palette name; see palette_names(). Matched case-insensitively;
#'   empty or NULL means "Codedbx", an unknown name falls back to "HCL Light".
#' @param n Integer number of colors required.
#' @param dark Logical. When TRUE every entry is lightened (hue preserved) until it
#'   reaches 3:1 contrast against `DARK_CANVAS`; entries that already pass are untouched.
#' @return Character vector of hex colors of length n.
#' @export
get_palette_colors <- function(name = "Codedbx", n = 2L, dark = FALSE) {
  cols <- palette_base(name, n)
  if (!isTRUE(dark) || length(cols) == 0L) {
    return(cols)
  }
  vapply(cols, lighten_to_contrast, character(1), USE.NAMES = FALSE)
}

#' Build the discrete colour scale for a plot's group levels
#'
#' Callers must pass the levels the plot was *actually built with*, never the levels of
#' whatever data happens to be loaded now. Deriving the colour count from live data while
#' the plot was built from an earlier dataset is what produced production error bce0bb1d:
#' "Insufficient values in manual scale. 3 needed but only 0 provided."
#'
#' @param levels Character/factor vector of the group levels the plot was built with, or
#'   NULL when the plot has no colour aesthetic. NA levels are dropped: ggplot2 colours
#'   NA via `na.value`, not from the manual palette.
#' @param palette_name Character palette name, passed to [get_palette_colors()].
#' @param dark Logical, TRUE in dark mode (passed to [get_palette_colors()]).
#' @return A ggplot2 discrete colour scale, or NULL when there is nothing to colour.
#'   NULL added to a ggplot is a no-op, so callers can add the result unconditionally.
#' @export
resolve_group_scale <- function(levels, palette_name = "Codedbx", dark = FALSE) {
  levels <- unique(levels[!is.na(levels)])
  if (length(levels) == 0L) {
    return(NULL)
  }
  ggplot2$scale_colour_manual(
    values = get_palette_colors(palette_name, length(levels), dark = dark)
  )
}

# -----------------------------------------------------------------------------
# Accessibility preflight
# -----------------------------------------------------------------------------

#' Contrast ratio against the canvas below which a palette entry is reported as faint.
#' Deliberately lower than the 3:1 the dark lift enforces: light mode never lifts (the
#' classic palettes must stay byte-identical), and at 3:1 thirteen of the sixteen
#' palettes would carry the note. At 1.5 it names exactly the yellows and the pale HCL set.
#' @export
PREFLIGHT_MIN_CONTRAST <- 1.5 # nolint: object_name_linter

#' Luminance ratio below which two adjacent steps of a sequential ramp do not separate
#' in a black-and-white print.
#' @export
PREFLIGHT_MIN_GREY_RATIO <- 1.2 # nolint: object_name_linter

#' Warn about a palette choice before the plot renders
#'
#' Pure. Three checks, each adding one flag and one clause to a single-line message:
#' `recycled` (fixed sets only, when the level count is known and exceeds the set),
#' `low_contrast` (any entry under `PREFLIGHT_MIN_CONTRAST` against the current canvas,
#' after the dark lift when `dark`), and `grayscale_collision` (sequential ramps only:
#' adjacent entries whose luminance ratio is under `PREFLIGHT_MIN_GREY_RATIO`).
#' Qualitative palettes are exempt from the last check: they separate levels by hue and
#' every one of them has near-equal-luminance pairs by design.
#'
#' @param name Palette name (resolved like [get_palette_colors()]).
#' @param n_levels Number of groups to colour, or NULL when not yet known (before a fit,
#'   or with no colour-by). Then the checks run at the fixed set's size, or 8 for a generator.
#' @param dark TRUE in dark mode.
#' @return `list(flags = character(), message = NULL or one string, n = integer)`.
#' @export
palette_preflight <- function(name, n_levels = NULL, dark = FALSE) {
  display <- resolve_palette_name(name)
  size <- palette_size(display)
  known_n <- !is.null(n_levels) && length(n_levels) == 1L && !is.na(n_levels) && is.finite(n_levels)
  n <- if (known_n) as.integer(n_levels) else if (is.na(size)) 8L else size
  flags <- character(0)
  clauses <- character(0)
  if (n < 1L) {
    return(list(flags = flags, message = NULL, n = n))
  }

  if (!is.na(size) && known_n && n > size) {
    extra <- n - size
    flags <- c(flags, "recycled")
    clauses <- c(clauses, sprintf(
      "%d levels, %s has %d: %s",
      n, display, size,
      if (extra == 1L) "1 colour repeats" else sprintf("%d colours repeat", extra)
    ))
  }

  canvas <- if (isTRUE(dark)) DARK_CANVAS else "#FFFFFF"
  cols <- get_palette_colors(display, n, dark = dark)
  ratios <- vapply(cols, contrast_ratio, numeric(1), b = canvas, USE.NAMES = FALSE)
  faint <- which(ratios < PREFLIGHT_MIN_CONTRAST)
  if (length(faint) > 0L) {
    first <- faint[[1]]
    more <- length(faint) - 1L
    flags <- c(flags, "low_contrast")
    clauses <- c(clauses, sprintf(
      "%s colour %d (%s) is %.1f:1 against %s: faint%s",
      display, first, cols[[first]], ratios[[first]],
      if (isTRUE(dark)) "the dark canvas" else "white",
      if (more > 0L) sprintf(" (and %d more)", more) else ""
    ))
  }

  if (palette_is_sequential(display) && n >= 2L) {
    lum <- vapply(cols, relative_luminance, numeric(1), USE.NAMES = FALSE)
    hi <- pmax(lum[-1], lum[-n])
    lo <- pmin(lum[-1], lum[-n])
    adjacent <- (hi + 0.05) / (lo + 0.05)
    close <- which(adjacent < PREFLIGHT_MIN_GREY_RATIO)
    if (length(close) > 0L) {
      worst <- close[[which.min(adjacent[close])]]
      more <- length(close) - 1L
      flags <- c(flags, "grayscale_collision")
      clauses <- c(clauses, sprintf(
        "levels %d and %d differ by %d%% luminance: hard to tell apart in greyscale%s",
        worst, worst + 1L, as.integer(round((adjacent[[worst]] - 1) * 100)),
        if (more > 0L) sprintf(" (and %d more pair%s)", more, if (more == 1L) "" else "s") else ""
      ))
    }
  }

  list(
    flags = flags,
    message = if (length(clauses) > 0L) paste(clauses, collapse = "; ") else NULL,
    n = n
  )
}

# TRUE when a colour is missing (NULL -> the geom default, which is black for
# lines/points/paths) or a dark, near-neutral tone that would vanish on the
# dark canvas. Saturated hues (the brand palette, a red fit line, a blue
# smooth) return FALSE so they are preserved.
is_dark_neutral <- function(col) {
  if (is.null(col)) {
    return(TRUE)
  }
  if (length(col) != 1L || is.na(col)) {
    return(FALSE)
  }
  rgb <- tryCatch(grDevices$col2rgb(col)[, 1], error = function(e) NULL)
  if (is.null(rgb)) {
    return(FALSE)
  }
  mx <- max(rgb)
  saturation <- if (mx == 0) 0 else (mx - min(rgb)) / mx
  luminance <- (0.2126 * rgb[1] + 0.7152 * rgb[2] + 0.0722 * rgb[3]) / 255
  luminance < 0.4 && saturation < 0.25
}

# Lighten geoms that would otherwise render in a default / near-black color and
# disappear on the dark canvas (e.g. a black prediction line, dark data
# points). Layers that map colour/fill to data — the brand palette via
# scale_color_manual — are left untouched so their true colors show through.
recolor_default_geoms <- function(p, light_color) {
  plot_aes <- names(p$mapping)
  for (i in seq_along(p$layers)) {
    layer <- p$layers[[i]]
    layer_aes <- names(layer$mapping)

    colour_mapped <- any(c("colour", "color") %in% c(layer_aes, plot_aes))
    if (!colour_mapped) {
      current <- layer$aes_params$colour
      if (is.null(current)) {
        current <- layer$aes_params$color
      }
      if (is_dark_neutral(current)) {
        p$layers[[i]]$aes_params$colour <- light_color
      }
    }

    fill_mapped <- "fill" %in% c(layer_aes, plot_aes)
    if (!fill_mapped) {
      current_fill <- layer$aes_params$fill
      # Only adjust an explicitly dark fill; leave NULL (most geoms have no
      # fill) and light fills (e.g. white points) as they are.
      if (!is.null(current_fill) && is_dark_neutral(current_fill)) {
        p$layers[[i]]$aes_params$fill <- light_color
      }
    }
  }
  p
}

#' Apply dark mode styling to a ggplot object
#'
#' Paints plot backgrounds with the app's dark page color, adjusts text/grid
#' colors, and lightens default/near-black geoms so they remain visible on the
#' dark canvas (palette-mapped colors are preserved). No-op when dark_mode is
#' "light". A solid fill (rather than transparent) is used because the plots
#' render to an opaque PNG canvas, so a transparent fill would show through as
#' white; the fill matches the app's dark `--bs-body-bg`/card background so the
#' image blends seamlessly with the page.
#'
#' @param p A ggplot object
#' @param dark_mode Character, "dark" or "light"
#' @return Modified ggplot object
#' @export
apply_dark_mode_theme <- function(p, dark_mode = "light") {
  if (!identical(dark_mode, "dark")) {
    return(p)
  }

  bg_color <- DARK_CANVAS
  text_color <- "#dee2e6"
  grid_color <- "#495057"

  p <- recolor_default_geoms(p, text_color)

  p +
    ggplot2$theme(
      plot.background = ggplot2$element_rect(fill = bg_color, color = NA),
      panel.background = ggplot2$element_rect(fill = bg_color, color = NA),
      legend.background = ggplot2$element_rect(fill = bg_color, color = NA),
      legend.key = ggplot2$element_rect(fill = bg_color, color = NA),
      text = ggplot2$element_text(color = text_color),
      axis.text = ggplot2$element_text(color = text_color),
      axis.title = ggplot2$element_text(color = text_color),
      plot.title = ggplot2$element_text(color = text_color),
      plot.subtitle = ggplot2$element_text(color = text_color),
      legend.text = ggplot2$element_text(color = text_color),
      legend.title = ggplot2$element_text(color = text_color),
      panel.grid.major = ggplot2$element_line(color = grid_color),
      panel.grid.minor = ggplot2$element_line(color = grid_color),
      axis.ticks = ggplot2$element_line(color = grid_color)
    )
}

#' @export
geomean <- function(x) {
  return(round(exp(mean(log((x + 1)))) - 1, 2))
}

#' Plot title, or NULL when there is nothing to show
#'
#' `ggplot2::ggtitle(NULL)` adds no title strip; `ggtitle("")` leaves a blank one.
#' Text inputs hand back "" (or NULL before the browser reports them), so route them through here.
#'
#' @param x A title: character, NULL or NA.
#' @return NULL for NULL, NA, zero-length, empty and whitespace-only input; otherwise `x` unchanged.
#' @export
plot_title_or_null <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]]) || !nzchar(trimws(x[[1]]))) NULL else x
}
