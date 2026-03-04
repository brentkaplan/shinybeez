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

    # show data table
    shiny$observe({
      shiny$req(session$userData$data$demand)
      session_logger$info(
        "Rendering Demand data table",
        category = "module_init"
      )
      output$data_table <- DT$renderDT(server = FALSE, {
        DT$datatable(
          data_r$data_d,
          rownames = FALSE,
          extensions = c("Buttons", "Scroller", "FixedColumns"),
          fillContainer = FALSE,
          options = list(
            autoWidth = TRUE,
            ordering = TRUE,
            dom = "Bti",
            buttons = list(
              list(extend = "copy"),
              list(extend = "print"),
              list(
                extend = "csv",
                filename = "shinybeez_Demand_Data",
                title = NULL
              ),
              list(
                extend = "excel",
                filename = "shinybeez_Demand_Data",
                title = NULL
              ),
              list(
                extend = "pdf",
                filename = "shinybeez_Demand_Data",
                title = NULL
              )
            ),
            fixedColumns = list(leftColumns = 1),
            deferRender = TRUE,
            scrollY = 250,
            scrollX = TRUE,
            scroller = TRUE
          )
        )
      })
    })

    # show descriptives table
    shiny$observe({
      shiny$req(session$userData$data$demand)
      session_logger$info("Computing demand descriptives", "data_processing")

      descriptives <- shiny$withProgress(message = "Computing descriptives...", {
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
      shiny$req(descriptives)

      output$descriptives_table <- DT$renderDT(server = FALSE, {
        DT$datatable(
          descriptives,
          rownames = FALSE,
          extensions = c("Buttons", "Scroller"),
          fillContainer = FALSE,
          options = list(
            autoWidth = TRUE,
            ordering = TRUE,
            dom = "Bti",
            buttons = list(
              list(extend = "copy"),
              list(extend = "print"),
              list(
                extend = "csv",
                filename = "shinybeez_Demand_Descriptives",
                title = NULL
              ),
              list(
                extend = "excel",
                filename = "shinybeez_Demand_Descriptives",
                title = NULL
              ),
              list(
                extend = "pdf",
                filename = "shinybeez_Demand_Descriptives",
                title = NULL
              )
            ),
            deferRender = TRUE,
            scrollY = 250,
            scroller = TRUE
          )
        )
      })
    })

    # show empirical table
    shiny$observe({
      shiny$req(session$userData$data$demand)
      session_logger$info(
        paste0("Computing empirical measures, grouped = ", isgroup()),
        "data_processing"
      )

      emp_result <- shiny$withProgress(message = "Computing empirical measures...", {
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
      shiny$req(emp_result)

      output$empirical_table <- DT$renderDT(server = FALSE, {
        DT$datatable(
          emp_result,
          rownames = FALSE,
          extensions = c("Buttons", "Scroller"),
          fillContainer = FALSE,
          options = list(
            autoWidth = TRUE,
            ordering = TRUE,
            dom = "Bti",
            buttons = list(
              list(extend = "copy"),
              list(extend = "print"),
              list(
                extend = "csv",
                filename = "shinybeez_Demand_Empirical_Measures",
                title = NULL
              ),
              list(
                extend = "excel",
                filename = "shinybeez_Demand_Empirical_Measures",
                title = NULL
              ),
              list(
                extend = "pdf",
                filename = "shinybeez_Demand_Empirical_Measures",
                title = NULL
              )
            ),
            deferRender = TRUE,
            scrollY = 250,
            scroller = TRUE
          )
        )
      })
    })

    # show systematic table
    shiny$observe({
      shiny$req(session$userData$data$demand)
      session_logger$info("Computing systematic criteria", "data_processing")

      sys_result <- shiny$withProgress(message = "Computing systematic criteria...", {
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
      shiny$req(sys_result)

      output$systematic_table <- DT$renderDT(server = FALSE, {
        DT$datatable(
          sys_result,
          rownames = FALSE,
          extensions = c("Buttons", "Scroller", "FixedColumns"),
          fillContainer = FALSE,
          options = list(
            autoWidth = FALSE,
            ordering = TRUE,
            dom = "Bti",
            buttons = list(
              list(extend = "copy"),
              list(extend = "print"),
              list(
                extend = "csv",
                filename = "shinybeez_Demand_Systematic_Criteria",
                title = NULL
              ),
              list(
                extend = "excel",
                filename = "shinybeez_Demand_Systematic_Criteria",
                title = NULL
              ),
              list(
                extend = "pdf",
                filename = "shinybeez_Demand_Systematic_Criteria",
                title = NULL
              )
            ),
            deferRender = TRUE,
            scrollY = 250,
            scroller = TRUE,
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 1)
          )
        )
      })
    })
  })
}
