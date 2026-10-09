#' Palette picker with swatch previews, and the note under it
#'
#' A grouped selectize input whose options draw the palette's colours. The input id,
#' the plain-name values and the default are exactly what the two plotting tabs used
#' with their old selectInput, so `input$plot_palette` / `input$palette` are untouched.

box::use(
  bsicons,
  htmltools,
  shiny,
)

box::use(
  app / logic / utils,
)

js_string <- function(x) {
  paste0('"', x, '"')
}

# One JS map of palette name -> light-mode hex, plus the renderer selectize calls for every
# option and for the selected item. Wrapped in singleton() so two pickers on one page
# (mixed effects + demand) emit it once.
swatch_script <- function() {
  entries <- vapply(utils$palette_names(), function(name) {
    sprintf("%s: [%s]", js_string(name), paste(js_string(utils$palette_swatch(name)), collapse = ", "))
  }, character(1), USE.NAMES = FALSE)
  # `kind` is 'option' (dropdown row) or 'item' (the selected value). Selectize's DEFAULT
  # templates are what add the .option / .item classes; a custom renderer must add them
  # itself or the control loses its highlight styling and tests cannot find the item.
  js <- paste0(
    "window.shinybeezPalettes = {", paste(entries, collapse = ", "), "};\n",
    "window.shinybeezPaletteOption = function(value, escape, kind) {\n",
    "  var hex = window.shinybeezPalettes[value] || [];\n",
    "  var swatches = hex.map(function(h) {\n",
    "    return '<span class=\"palette-swatch\" style=\"background:' + h + '\"></span>';\n",
    "  }).join('');\n",
    "  var cls = (kind === 'item' ? 'item' : 'option') + ' palette-option';\n",
    "  return '<div class=\"' + cls + '\"><span class=\"palette-swatches\">' + swatches +\n",
    "    '</span><span class=\"palette-name\">' + escape(value) + '</span></div>';\n",
    "};"
  )
  htmltools$singleton(htmltools$tags$script(htmltools$HTML(js)))
}

render_option_js <- paste0(
  "{option: function(item, escape) { return window.shinybeezPaletteOption(item.value, escape, 'option'); }, ",
  "item: function(item, escape) { return window.shinybeezPaletteOption(item.value, escape, 'item'); }}"
)

#' Grouped palette select with swatch previews
#'
#' @param input_id Namespaced input id (`ns("plot_palette")`).
#' @param label Label text.
#' @param selected Initial palette name.
#' @param width Passed to `selectizeInput()`.
#' @return An `htmltools::tagList` (the singleton script plus the input).
#' @export
palette_picker <- function(input_id, label = "Color Palette", selected = "Codedbx", width = NULL) {
  names <- utils$palette_names()
  groups <- vapply(names, utils$palette_group, character(1), USE.NAMES = FALSE)
  # Each group as a list, never a character vector: Shiny renders a one-member character
  # group (Brand, Print) as a single option labelled with the group name.
  choices <- lapply(split(names, factor(groups, levels = unique(groups))), as.list)
  htmltools$tagList(
    swatch_script(),
    shiny$selectizeInput(
      input_id,
      label,
      choices = choices,
      selected = selected,
      width = width,
      options = list(render = I(render_option_js))
    )
  )
}

#' The accessibility note under a picker
#'
#' @param preflight The list returned by `utils$palette_preflight()`.
#' @return A `<small>` tag, or NULL when there is nothing to say (NULL from `renderUI`
#'   renders nothing).
#' @export
palette_note <- function(preflight) {
  if (is.null(preflight$message)) {
    return(NULL)
  }
  shiny$tags$small(
    class = "palette-note text-warning",
    bsicons$bs_icon("exclamation-triangle"),
    " ",
    preflight$message
  )
}
