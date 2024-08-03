box::use(
  bslib,
  shiny,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  bslib$nav_item(
    shiny$actionLink(ns("info"), "", icon = shiny$icon("info-circle"))
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
          height = 115, width = 100
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
        # v2 future addition
        # shiny$tabsetPanel(
        #   id = "modal_tabs",
        #   shiny$tabPanel(
        #     title = "Demand",
        #     value = "tab_demand",
        #   ),
        #   shiny$tabPanel(
        #     title = "Discounting",
        #     value = "tab_discounting"
        #   )
        # ),
        footer = shiny$tagList(
          shiny$modalButton('Close')
        ),
        easyClose = TRUE
        ))
    })
  })
}
