#' Shared Plot Settings Component
#'
#' Reusable plot settings UI for theme, palette, axis labels, and legend.

box::use(
  bslib,
  shiny
)

#' Create plot settings sidebar UI
#'
#' @param id Module namespace ID
#' @param title Sidebar title
#' @param show_log_axes Show log axis checkboxes
#' @param show_palette Show palette selector
#' @param show_legend Show legend position selector
#' @param show_font_size Show font size input
#' @param open Whether sidebar is open by default
#' @return bslib sidebar element
#' @export
sidebar_ui <- function(
  id,
  title = "Plot Settings",
  show_log_axes = TRUE,
  show_palette = TRUE,
  show_legend = TRUE,
  show_font_size = TRUE,
  open = FALSE
) {
  ns <- shiny$NS(id)

  elements <- list(
    shiny$textInput(
      inputId = ns("title"),
      label = "Title",
      value = ""
    ),
    shiny$textInput(
      inputId = ns("xlab"),
      label = "X-Axis Label",
      value = "Price"
    ),
    shiny$textInput(
      inputId = ns("ylab"),
      label = "Y-Axis Label",
      value = "Consumption"
    ),
    shiny$selectInput(
      inputId = ns("theme"),
      label = "Theme",
      choices = c(
        "Prism" = "prism",
        "Classic" = "classic",
        "Minimal" = "minimal"
      ),
      selected = "prism"
    )
  )

  if (show_palette) {
    elements <- c(
      elements,
      list(
        shiny$selectInput(
          inputId = ns("palette"),
          label = "Color Palette",
          choices = c("Okabe-Ito", "HCL Light", "HCL Dark"),
          selected = "Okabe-Ito"
        )
      )
    )
  }

  if (show_font_size) {
    elements <- c(
      elements,
      list(
        shiny$numericInput(
          inputId = ns("font_size"),
          label = "Font Size",
          value = 14,
          min = 8,
          max = 24,
          step = 1
        )
      )
    )
  }

  if (show_legend) {
    elements <- c(
      elements,
      list(
        shiny$selectInput(
          inputId = ns("legend_position"),
          label = "Legend Position",
          choices = c(
            "Right" = "right",
            "Bottom" = "bottom",
            "Top" = "top",
            "Left" = "left",
            "None" = "none"
          ),
          selected = "right"
        )
      )
    )
  }

  if (show_log_axes) {
    elements <- c(
      elements,
      list(
        shiny$checkboxInput(
          inputId = ns("xlog"),
          label = "Log X-Axis",
          value = FALSE
        ),
        shiny$checkboxInput(
          inputId = ns("ylog"),
          label = "Log Y-Axis",
          value = FALSE
        )
      )
    )
  }

  elements <- c(
    elements,
    list(
      shiny$actionButton(
        inputId = ns("update"),
        label = "Update Plot",
        class = "btn-primary"
      )
    )
  )

  bslib$sidebar(
    title = title,
    open = open,
    shiny$tagList(elements)
  )
}

#' Plot settings server - returns reactive list of settings
#'
#' @param id Module namespace ID
#' @return Reactive list with plot settings
#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    # Return reactive with all settings
    settings <- shiny$reactive({
      list(
        title = input$title,
        xlab = input$xlab,
        ylab = input$ylab,
        theme = input$theme,
        palette = input$palette,
        font_size = input$font_size,
        legend_position = input$legend_position,
        xlog = isTRUE(input$xlog),
        ylog = isTRUE(input$ylog)
      )
    })

    # Return list with settings reactive and update trigger
    list(
      settings = settings,
      update_trigger = shiny$reactive(input$update)
    )
  })
}
