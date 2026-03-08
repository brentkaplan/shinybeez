box::use(
  bslib,
  DT,
  shiny,
)

box::use(
  app / logic / demand / empirical,
  app / logic / demand / systematic,
  app / logic / logging_utils,
  app / logic / validate,
  app / view / shared / data_table[build_datatable],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  bslib$navset_card_tab(
    title = "Data Table",
    bslib$nav_panel(
      id = "data",
      "Data",
      DT$DTOutput(ns("data_table"))
    ),
    bslib$nav_panel(
      id = "descriptives",
      "Descriptives",
      DT$DTOutput(ns("descriptives_table"))
    ),
    bslib$nav_panel(
      id = "empirical",
      "Empirical Measures",
      DT$DTOutput(ns("empirical_table"))
    ),
    bslib$nav_panel(
      style = "padding: 5px !important;",
      id = "systematic",
      "Systematic Criteria",
      bslib$card(
        style = "border: none; ",
        bslib$layout_sidebar(
          fillable = TRUE,
          sidebar = bslib$sidebar(
            title = "Systematic Settings",
            open = FALSE,
            shiny$numericInput(
              inputId = ns("deltaq"),
              label = "Criteria #1: Relative change (DeltaQ)",
              value = .025,
              min = 0,
              max = 1,
              step = .005
            ),
            shiny$numericInput(
              inputId = ns("bounce"),
              label = "Criteria #2: Price to price increases (Bounce)",
              value = .10,
              min = 0,
              max = 1,
              step = .01
            ),
            shiny$numericInput(
              inputId = ns("reversals"),
              label = "Criteria #3: Number of reversals (Reversals)",
              value = 0,
              min = 0,
              max = 4,
              step = 1
            ),
            shiny$numericInput(
              inputId = ns("ncons0"),
              label = "Criteria #3a: Number of consecutive 0s used to flag reversals",
              value = 2,
              min = 0,
              max = 4,
              step = 1
            )
          ),
          DT$DTOutput(ns("systematic_table"))
        )
      )
    )
  )
}

#' @export
server <- function(id, isgroup = NULL, data_r) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create session-specific logger
    session_logger <- logging_utils$create_session_logger(session)

    # Lazy reactive: only computes when the Descriptives tab is viewed
    descriptives_r <- shiny$reactive({
      shiny$req(session$userData$data$demand)
      session_logger$info("Computing demand descriptives", "data_processing")
      shiny$withProgress(message = "Computing descriptives...", {
        tryCatch(
          session_logger$with_performance("demand_descriptives", function() {
            empirical$compute_descriptives(data_r$data_d, is_grouped = isgroup())
          }),
          error = function(e) {
            shiny$showNotification(e$message, type = "error", duration = NULL)
            NULL
          }
        )
      })
    })

    # Lazy reactive: only computes when the Empirical Measures tab is viewed
    empirical_r <- shiny$reactive({
      shiny$req(session$userData$data$demand)
      session_logger$info(
        paste0("Computing empirical measures, grouped = ", isgroup()),
        "data_processing"
      )
      shiny$withProgress(message = "Computing empirical measures...", {
        tryCatch(
          session_logger$with_performance("demand_empirical_measures", function() {
            empirical$compute_empirical_measures(
              data_r$data_d,
              is_grouped = isgroup()
            )
          }),
          error = function(e) {
            shiny$showNotification(e$message, type = "error", duration = NULL)
            NULL
          }
        )
      })
    })

    # Lazy reactive: re-computes when data or systematic settings change
    systematic_r <- shiny$reactive({
      shiny$req(session$userData$data$demand)
      session_logger$info("Computing systematic criteria", "data_processing")
      shiny$withProgress(message = "Computing systematic criteria...", {
        tryCatch(
          session_logger$with_performance("demand_systematic_check", function() {
            systematic$compute_systematic(
              data_r$data_d,
              deltaq = input$deltaq,
              bounce = input$bounce,
              reversals = input$reversals,
              ncons0 = input$ncons0,
              is_grouped = isgroup()
            )
          }),
          error = function(e) {
            shiny$showNotification(e$message, type = "error", duration = NULL)
            NULL
          }
        )
      })
    })

    # Render data table
    output$data_table <- DT$renderDT(server = FALSE, {
      shiny$req(session$userData$data$demand)
      session_logger$info("Rendering Demand data table", category = "module_init")
      build_datatable(
        data_r$data_d,
        filename_prefix = "shinybeez_Demand_Data",
        scroll_y = 250,
        fixed_columns = 1L
      )
    })

    # Render descriptives table
    output$descriptives_table <- DT$renderDT(server = FALSE, {
      shiny$req(descriptives_r())
      build_datatable(
        descriptives_r(),
        filename_prefix = "shinybeez_Demand_Descriptives",
        scroll_y = 250
      )
    })

    # Render empirical table
    output$empirical_table <- DT$renderDT(server = FALSE, {
      shiny$req(empirical_r())
      build_datatable(
        empirical_r(),
        filename_prefix = "shinybeez_Demand_Empirical_Measures",
        scroll_y = 250
      )
    })

    # Render systematic table
    output$systematic_table <- DT$renderDT(server = FALSE, {
      shiny$req(systematic_r())
      build_datatable(
        systematic_r(),
        filename_prefix = "shinybeez_Demand_Systematic_Criteria",
        scroll_y = 250,
        fixed_columns = 1L,
        auto_width = FALSE
      )
    })
  })
}
