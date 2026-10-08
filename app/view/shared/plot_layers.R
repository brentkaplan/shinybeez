#' Shared Plot Layers Section
#'
#' Show/hide and prominence controls for the population, individual and observed
#' layers of a fitted-model plot. Hosts embed `ui()` inside their own plot
#' sidebar, call `server()` with the same `engine`, and read the validated
#' `style()` reactive. The six numeric boxes under "Advanced" are the source of
#' truth; each prominence slider is a one-way macro that writes its pair of
#' boxes. Nothing here records telemetry: the host decides what "rendered" means.

box::use(
  bslib,
  shiny,
)

box::use(
  app / logic / plot_style,
)

numeric_box <- function(ns, id, label, value, range, step) {
  shiny$numericInput(ns(id), label, value = value, min = range[1], max = range[2], step = step)
}

#' @param id Module id.
#' @param engine One of `plot_style$engines()`; must match the `server()` call.
#' @export
ui <- function(id, engine = "beezdemand_nlme") {
  ns <- shiny$NS(id)
  d <- plot_style$style_defaults(engine)$layers
  prom <- plot_style$prominence_defaults(engine)
  lim <- plot_style$limits()

  shiny$tagList(
    shiny$tags$h6("Layers", class = "mt-2"),
    shiny$checkboxInput(ns("show_population"), "Show Population Lines", value = d$population$show),
    shiny$sliderInput(
      ns("prom_population"), "Population prominence",
      min = 0, max = 100, value = prom$population, step = 5, ticks = FALSE
    ),
    shiny$checkboxInput(ns("show_individual"), "Show Individual Lines", value = d$individual$show),
    shiny$sliderInput(
      ns("prom_individual"), "Individual prominence",
      min = 0, max = 100, value = prom$individual, step = 5, ticks = FALSE
    ),
    shiny$checkboxInput(ns("show_observed"), "Show Observed Points", value = d$observed$show),
    bslib$accordion(
      id = ns("advanced"),
      open = FALSE,
      bslib$accordion_panel(
        "Advanced (overrides the sliders)",
        numeric_box(ns, "pop_alpha", "Population alpha", d$population$alpha, lim$alpha, 0.05),
        numeric_box(ns, "pop_width", "Population width", d$population$width, lim$width, 0.1),
        numeric_box(ns, "ind_alpha", "Individual alpha", d$individual$alpha, lim$alpha, 0.05),
        numeric_box(ns, "ind_width", "Individual width", d$individual$width, lim$width, 0.1),
        numeric_box(ns, "obs_alpha", "Observed alpha", d$observed$alpha, lim$alpha, 0.05),
        numeric_box(ns, "obs_size", "Observed size", d$observed$size, lim$size, 0.1)
      )
    )
  )
}

#' @return `list(style = reactive)` — the validated style for `engine`.
#' @export
server <- function(id, engine = "beezdemand_nlme") {
  shiny$moduleServer(id, function(input, output, session) {
    # Slider -> numeric boxes only. Nothing writes back to a slider, so no loop.
    shiny$observeEvent(input$prom_population, {
      v <- plot_style$prominence_to_layer(input$prom_population / 100, "population")
      shiny$updateNumericInput(session, "pop_alpha", value = v$alpha)
      shiny$updateNumericInput(session, "pop_width", value = v$width)
    }, ignoreInit = TRUE)

    shiny$observeEvent(input$prom_individual, {
      v <- plot_style$prominence_to_layer(input$prom_individual / 100, "individual")
      shiny$updateNumericInput(session, "ind_alpha", value = v$alpha)
      shiny$updateNumericInput(session, "ind_width", value = v$width)
    }, ignoreInit = TRUE)

    style <- shiny$reactive({
      plot_style$validate_style(
        list(
          layers = list(
            population = list(show = input$show_population, alpha = input$pop_alpha, width = input$pop_width),
            individual = list(show = input$show_individual, alpha = input$ind_alpha, width = input$ind_width),
            observed = list(show = input$show_observed, alpha = input$obs_alpha, size = input$obs_size)
          )
        ),
        engine
      )
    })

    list(style = style)
  })
}
