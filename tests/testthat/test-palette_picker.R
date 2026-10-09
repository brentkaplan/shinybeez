# The palette picker: a grouped selectize select whose options draw the palette's colours,
# and the note that sits under it. Rendered to HTML and inspected as text; the swatch
# drawing itself happens in the browser (covered by the integration test).

box::use(
  testthat[...],
  htmltools,
)

box::use(
  app / logic / utils,
  app / view / shared / palette_picker[palette_note, palette_picker],
)

option_values <- function(html) {
  found <- regmatches(html, gregexpr('<option value="[^"]*"', html))[[1]]
  sub('"$', "", sub('<option value="', "", found))
}

optgroup_labels <- function(html) {
  found <- regmatches(html, gregexpr('<optgroup label="[^"]*"', html))[[1]]
  sub('"$', "", sub('<optgroup label="', "", found))
}

describe("palette_picker", {
  html <- as.character(palette_picker("x"))

  it("renders a select with the given id and the brand palette selected", {
    expect_match(html, '<select class="shiny-input-select form-control" id="x">', fixed = TRUE)
    expect_match(html, '<option value="Codedbx" selected>Codedbx</option>', fixed = TRUE)
    expect_match(html, 'for="x">Color Palette</label>', fixed = TRUE)
  })

  it("lists every registry palette once, grouped, in registry order", {
    expect_identical(option_values(html), utils$palette_names())
    expect_identical(optgroup_labels(html), c("Brand", "Colourblind-safe", "Prism", "Generated", "Print"))
    # One-member groups must still be optgroups (Shiny flattens a one-member character vector).
    expect_match(html, '<optgroup label="Brand">\n<option value="Codedbx" selected>Codedbx</option>', fixed = TRUE)
    expect_match(html, '<optgroup label="Print">\n<option value="Grayscale">Grayscale</option>', fixed = TRUE)
  })

  it("asks selectize to evaluate the swatch renderer for options and for the selected item", {
    expect_match(html, 'data-eval="[&quot;render&quot;]"', fixed = TRUE)
    expect_match(html, "window.shinybeezPaletteOption(item.value, escape, 'option')", fixed = TRUE)
    expect_match(html, "window.shinybeezPaletteOption(item.value, escape, 'item')", fixed = TRUE)
    # The renderer restores the classes selectize's default templates would have added.
    expect_match(html, "(kind === 'item' ? 'item' : 'option') + ' palette-option'", fixed = TRUE)
  })

  it("emits the hex map once per page with the swatch colours", {
    two <- as.character(htmltools$tagList(palette_picker("a"), palette_picker("b")))
    expect_identical(lengths(regmatches(two, gregexpr("window.shinybeezPalettes = {", two, fixed = TRUE))), 1L)
    expect_match(two, '"Codedbx": ["#534B7A", "#A25F5F", "#5D8AA8", "#7D9C7F", "#2B4560", "#B08C6A"]', fixed = TRUE)
    viridis <- paste0('"', utils$palette_swatch("viridis"), '"', collapse = ", ")
    expect_match(two, sprintf('"viridis": [%s]', viridis), fixed = TRUE)
    expect_match(two, '<select class="shiny-input-select form-control" id="a">', fixed = TRUE)
    expect_match(two, '<select class="shiny-input-select form-control" id="b">', fixed = TRUE)
  })

  it("honours a different label and selection", {
    other <- as.character(palette_picker("y", label = "Palette", selected = "viridis"))
    expect_match(other, 'for="y">Palette</label>', fixed = TRUE)
    expect_match(other, '<option value="viridis" selected>viridis</option>', fixed = TRUE)
    expect_false(grepl('<option value="Codedbx" selected>', other, fixed = TRUE))
  })

  it("renders a stale selection without error", {
    stale <- as.character(palette_picker("z", selected = "nope"))
    expect_false(grepl(" selected>", stale, fixed = TRUE))
    expect_identical(option_values(stale), utils$palette_names())
  })
})

describe("palette_note", {
  it("renders nothing when the preflight is clean", {
    expect_null(palette_note(utils$palette_preflight("Codedbx", n_levels = 3)))
  })

  it("renders a small warning line with an icon and the message", {
    html <- as.character(palette_note(utils$palette_preflight("Dark2", n_levels = 10)))
    expect_match(html, '<small class="palette-note text-warning">', fixed = TRUE)
    expect_match(html, "<svg", fixed = TRUE)
    expect_match(html, "10 levels, Dark2 has 8: 2 colours repeat", fixed = TRUE)
  })
})
