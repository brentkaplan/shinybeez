box::use(
  bslib,
  rhino,
  shiny,
)

box::use(
  app/view/demand,
  app/view/discounting,
  app/view/info,
  app/view/welcome,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  rhino$log$info("Starting")
  bslib$page_navbar(
    title = shiny$tags$span(
      style = "font-size: 200%;",
      shiny$tags$img(
        src = "static/img/hex-shinybeez.png",
        width = "95px",
        height = "auto",
        class = "me-3",
        alt = "Shiny Beez"
      ),
      "Shinybeez"
    ),
    id = "nav",
    sidebar = bslib$sidebar(
      id = "sidebar",
      open = TRUE,
      width = 350,
      shiny$conditionalPanel(
        "input.nav === 'Welcome'",
        welcome$sidebar_ui(ns("welcomeui"))
      ),
      shiny$conditionalPanel(
        "input.nav === 'Demand'",
        demand$sidebar_ui(ns("demand")),
      ),
      shiny$conditionalPanel(
        "input.nav === 'Discounting'",
        discounting$sidebar_ui(ns("discounting"))
      )
    ),
    bslib$nav_panel(
      title = "Welcome",
      welcome$body_ui(ns("welcomebody"))
    ),
    bslib$nav_panel(
      value = "Demand",
      title = "Demand",
      demand$navpanel_ui(ns("demand"))
    ),
    bslib$nav_panel(
      value = "Discounting",
      title = "Discounting",
      discounting$navpanel_ui(ns("discounting"))
    ),
    bslib$nav_spacer(),
    bslib$nav_item(
      bslib$input_dark_mode(id = "dark_mode", mode = "light")
    ),
    info$ui(ns("info"))
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    welcome$body_server("welcomebody")
    session$userData$data <- shiny$reactiveValues()
    demand$sidebar_server("demand")
    demand$navpanel_server("demand")
    discounting$sidebar_server("discounting")
    discounting$navpanel_server("discounting")
    info$server("info")
    session$onSessionEnded(function() {
      rhino$log$info("Stopping")
    })
  })
}
