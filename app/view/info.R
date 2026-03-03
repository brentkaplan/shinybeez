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
        title = "Helpful information about the app",
        size = "l",
        shiny$tags$img(
          src = "static/img/hex-shinybeez.png",
          height = 115,
          width = 100,
          alt = "Shinybeez hex logo"
        ),
        shiny$HTML(
          paste0(
            "
              <br>
              <br>
              Please refer to the welcome page for an overview on
              using the app. We plan to include quick reference items
              here in the future.
              "
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
