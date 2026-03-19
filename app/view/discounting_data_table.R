box::use(
  bslib,
  dplyr,
  DT,
  shiny,
  stats,
)

box::use(
  app / logic / discounting / systematic,
  app / logic / logging_utils,
  app / view / shared / data_table[build_datatable],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$uiOutput(
    ns("data_box")
  )
}

#' @export
server <- function(id, data_r, type = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create session-specific logger
    session_logger <- logging_utils$create_session_logger(session)

    shiny$observe({
      shiny$req(session$userData$data$discounting)

      output$data_box <- shiny$renderUI({
        shiny$req(data_r$data_d)
        shiny$req(type())

        if (type() == "27-Item MCQ" & !("I16" %in% names(data_r$data_d))) {
          bslib$navset_card_tab(
            title = "Data Table",
            bslib$nav_panel(
              id = "data",
              "Data",
              shiny$div(
                DT$DTOutput(ns("data_table"))
              )
            ),
            bslib$nav_panel(
              id = "missings",
              "Missing Summary",
              DT$DTOutput(ns("missings_table"))
            )
          )
        } else {
          bslib$navset_card_tab(
            title = "Data Table",
            bslib$nav_panel(
              id = "data",
              "Data",
              shiny$div(
                style = "min-height:700px; max-height:100vh; overflow:auto;",
                DT$DTOutput(ns("data_table"))
              )
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
                      inputId = ns("c1"),
                      label = "Criteria #1: Local change",
                      value = .2,
                      min = 0,
                      max = 1,
                      step = .005
                    ),
                    shiny$numericInput(
                      inputId = ns("c2"),
                      label = "Criteria #2: Global trend",
                      value = .10,
                      min = 0,
                      max = 1,
                      step = .01
                    )
                  ),
                  DT$DTOutput(ns("systematic_table"))
                )
              )
            )
          )
        }
      })

      shiny$req(data_r$data_d)
      if (type() %in% "27-Item MCQ") {
        if (any(is.na((data_r$data_d$response)))) {
          shiny$showNotification(
            "There appears to be missing data in the 27-Item MCQ. Please be
          cautious when interpreting the results and choosing an appropriate
          imputation method.",
            type = "error",
            duration = NULL
          )
        }
      } else if (type() %in% "Indifference Point Regression") {
        if (any(is.na((data_r$data_d$y)))) {
          shiny$showNotification(
            "There appears to be missing data. Please be
          cautious when interpreting the results.",
            type = "error",
            duration = NULL
          )
        }
      }

      session_logger$info("Printing Discounting Datatable", "module_init")
      output$data_table <- DT$renderDT(server = FALSE, {
        build_datatable(
          data_r$data_d,
          filename_prefix = "shinybeez_Discounting_Data",
          scroll_y = 500,
          page_length = 10
        )
      })

      output$missings_table <- DT$renderDT(server = FALSE, {
        missings <- NULL
        if (type() %in% "27-Item MCQ") {
          missings <- data_r$data_d |>
            dplyr$group_by(subjectid) |>
            dplyr$summarise(
              prop_missing = round(sum(is.na(response)) / dplyr$n(), 2)
            ) |>
            dplyr$ungroup()
        } else if (type() %in% "Indifference Point Regression") {
          missings <- data_r$data_d[!stats$complete.cases(data_r$data_d), ]
        }
        shiny$req(missings)
        build_datatable(
          missings,
          filename_prefix = "shinybeez_Discounting_Missings",
          scroll_y = 300,
          page_length = 10
        )
      })

      output$systematic_table <- DT$renderDT(server = FALSE, {
        sys_result <- NULL
        if (type() %in% "27-Item MCQ") {
          # MCQ has no systematic criteria
        } else if (type() %in% "Indifference Point Regression") {
          sys_result <- tryCatch(
            session_logger$with_performance(
              "discounting_systematic_check",
              function() {
                systematic$compute_systematic_discounting(
                  data_r$data_d,
                  c1 = input$c1,
                  c2 = input$c2
                )
              },
              always_log = TRUE
            ),
            error = function(e) {
              session_logger$error_enhanced(e$message, e, context = "discounting_systematic_check")
              shiny$showNotification(e$message, type = "error", duration = 10)
              NULL
            }
          )
        }
        shiny$req(sys_result)
        build_datatable(
          sys_result,
          filename_prefix = "shinybeez_Discounting_SystematicCriteria",
          scroll_y = 300,
          page_length = 10
        )
      })
    })
  })
}
