box::use(
  bslib,
  shiny,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  bslib$nav_item(
    shiny$actionLink(
      ns("info"),
      shiny$tags$span(class = "visually-hidden", "App information"),
      icon = shiny$icon("info-circle"),
      `aria-label` = "App information"
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    shiny$observeEvent(input$info, {
      shiny$showModal(shiny$modalDialog(
        title = "About shinybeez",
        size = "l",
        shiny$div(
          class = "d-flex align-items-start gap-3",
          shiny$tags$img(
            src = "static/img/hex-shinybeez.png",
            height = 115,
            width = 100,
            alt = "Shinybeez hex logo"
          ),
          shiny$div(
            shiny$tags$h5("shinybeez"),
            shiny$tags$p(
              class = "text-muted mb-2",
              "Behavioral Economic Easy Demand and Discounting"
            ),
            shiny$tags$p(
              class = "small",
              "Kaplan, B. A. & Reed, D. D. (2025).",
              shiny$tags$em(
                "shinybeez: A Shiny app for Behavioral Economic",
                "Easy Demand and Discounting."
              ),
              "Journal of the Experimental Analysis of Behavior.",
              shiny$tags$a(
                href = "https://doi.org/10.1002/JEAB.70000",
                target = "_blank",
                "doi:10.1002/JEAB.70000"
              )
            )
          )
        ),
        shiny$tags$hr(),
        shiny$div(
          class = "small",
          shiny$tags$p(
            shiny$tags$strong("Links: "),
            shiny$tags$a(
              href = "https://github.com/brentkaplan/shinybeez",
              target = "_blank",
              "GitHub"
            ),
            " | ",
            shiny$tags$a(
              href = "https://shinybeez.app",
              target = "_blank",
              "shinybeez.app"
            )
          ),
          shiny$tags$p(
            class = "text-muted",
            "See the Welcome tab for full documentation,",
            "templates, and installation instructions."
          )
        ),
        footer = shiny$tagList(
          shiny$modalButton("Close")
        ),
        easyClose = TRUE
      ))
    })
  })
}
