box::use(
  bslib,
  rhino,
  sass,
  shiny,
)

box::use(
  app / view / mixed_effects_demand,
  app / view / demand,
  app / view / discounting,
  app / view / info,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  rhino$log$info("Starting")

  # # Get Google Analytics ID from environment variable, with fallback
  # ga_id <- Sys.getenv("GOOGLE_ANALYTICS_ID", "G-Q5TL32WDK3") # Default to shinyapps.io ID

  # # Read the template and replace the placeholder
  # ga_template <- readLines("app/static/html/g_tag_dynamic.html", warn = FALSE)
  # ga_script <- paste(gsub("{{GOOGLE_ANALYTICS_ID}}", ga_id, ga_template), collapse = "\n")

  bslib$page_navbar(
    header = shiny$tags$head(
      shiny$includeHTML(
        "app/static/html/g_tag.html"
      )
    ),
    # header = shiny$tags$head(
    #   shiny$HTML(ga_script)
    # ),
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
        shiny$includeHTML("app/static/html/welcome_sidebar.html")
      ),
      shiny$conditionalPanel(
        "input.nav === 'Demand'",
        demand$sidebar_ui(ns("demand")),
      ),
      shiny$conditionalPanel(
        "input.nav === 'Discounting'",
        discounting$sidebar_ui(ns("discounting"))
      ),
      shiny$conditionalPanel(
        "input.nav === 'MixedEffectsDemand'",
        mixed_effects_demand$sidebar_ui(ns("mixed_effects_demand"))
      )
    ),
    bslib$nav_panel(
      "Welcome",
      shiny$includeHTML("app/static/html/welcome.html")
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
    bslib$nav_panel(
      value = "MixedEffectsDemand",
      title = "Mixed Effects Demand",
      mixed_effects_demand$navpanel_ui(ns("mixed_effects_demand"))
    ),
    bslib$nav_spacer(),
    bslib$nav_item(
      bslib$input_dark_mode(id = "dark_mode", mode = "dark")
    ),
    info$ui(ns("info"))
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session$userData$data <- shiny$reactiveValues()
    demand$sidebar_server("demand")
    demand$navpanel_server("demand")
    discounting$sidebar_server("discounting")
    discounting$navpanel_server("discounting")
    # # <<< MIXED EFFECTS DEMAND SERVER CALLS >>>
    # Assuming file upload for this tab might be handled differently or use a shared mechanism later.
    # For now, the mixed_effects_demand module itself loads `ko` data.
    # We pass the data_reactive from where the data is managed.
    # If each tab has its own file input, session$userData$data[[current_tab_data_name]] would be passed.
    # For this example, let's assume a shared data source for simplicity initially or handle data within the module.
    # For 'ko' data, it's loaded within the module.
    mmd_sidebar_reactives <- mixed_effects_demand$sidebar_server(
      "mixed_effects_demand",
      data_reactive = shiny$reactive(session$userData$data$mixed_effects_demand)
    ) # Example placeholder if data was uploaded
    mixed_effects_demand$navpanel_server(
      "mixed_effects_demand",
      sidebar_reactives = mmd_sidebar_reactives
    )
    # # <<< END MIXED EFFECTS DEMAND SERVER CALLS >>>
    info$server("info")
    session$onSessionEnded(function() {
      rhino$log$info("Stopping")
    })
  })
}
