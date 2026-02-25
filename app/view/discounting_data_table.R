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
            duration = 10
          )
        }
      } else if (type() %in% "Indifference Point Regression") {
        if (any(is.na((data_r$data_d$y)))) {
          shiny$showNotification(
            "There appears to be missing data. Please be
          cautious when interpreting the results.",
            type = "error",
            duration = 10
          )
        }
      }

      session_logger$info("Printing Discounting Datatable", "module_init")
      output$data_table <- DT$renderDT(server = FALSE, {
        DT$datatable(
          data_r$data_d,
          rownames = FALSE,
          extensions = c("Buttons", "Scroller"),
          fillContainer = FALSE,
          autoHideNavigation = TRUE,
          options = list(
            pageLength = 10,
            autoWidth = TRUE,
            ordering = TRUE,
            dom = "Bti",
            buttons = list(
              list(extend = "copy"),
              list(extend = "print"),
              list(
                extend = "csv",
                filename = "shinybeez_Discounting_Data",
                title = NULL
              ),
              list(
                extend = "excel",
                filename = "shinybeez_Discounting_Data",
                title = NULL
              ),
              list(
                extend = "pdf",
                filename = "shinybeez_Discounting_Data",
                title = NULL
              )
            ),
            deferRender = TRUE,
            scrollY = 500,
            scroller = TRUE
          )
        )
      })

      output$missings_table <- DT$renderDT(server = FALSE, {
        if (type() %in% "27-Item MCQ") {
          missings <- data_r$data_d |>
            dplyr$group_by(subjectid) |>
            dplyr$summarise(
              prop_missing = round(sum(is.na("response")) / dplyr$n(), 2)
            ) |>
            dplyr$ungroup()
        } else if (type() %in% "Indifference Point Regression") {
          missings <- data_r$data_d[!stats$complete.cases(data_r$data_d), ]
        }
        DT$datatable(
          missings,
          rownames = FALSE,
          extensions = c("Buttons", "Scroller"),
          fillContainer = FALSE,
          autoHideNavigation = TRUE,
          options = list(
            pageLength = 10,
            autoWidth = TRUE,
            ordering = TRUE,
            dom = "Bti",
            buttons = list(
              list(extend = "copy"),
              list(extend = "print"),
              list(
                extend = "csv",
                filename = "shinybeez_Discounting_Missings",
                title = NULL
              ),
              list(
                extend = "excel",
                filename = "shinybeez_Discounting_Missings",
                title = NULL
              ),
              list(
                extend = "pdf",
                filename = "shinybeez_Discounting_Missings",
                title = NULL
              )
            ),
            deferRender = TRUE,
            scrollY = 300,
            scroller = TRUE
          )
        )
      })

      output$systematic_table <- DT$renderDT(server = FALSE, {
        if (type() %in% "27-Item MCQ") {
          sys_result <- NULL
        } else if (type() %in% "Indifference Point Regression") {
          sys_result <- tryCatch(
            systematic$compute_systematic_discounting(
              data_r$data_d,
              c1 = input$c1,
              c2 = input$c2
            ),
            error = function(e) {
              shiny$showNotification(e$message, type = "error", duration = 10)
              NULL
            }
          )
        }
        shiny$req(sys_result)
        DT$datatable(
          sys_result,
          rownames = FALSE,
          extensions = c("Buttons", "Scroller"),
          fillContainer = FALSE,
          autoHideNavigation = TRUE,
          options = list(
            pageLength = 10,
            autoWidth = TRUE,
            ordering = TRUE,
            dom = "Bti",
            buttons = list(
              list(extend = "copy"),
              list(extend = "print"),
              list(
                extend = "csv",
                filename = "shinybeez_Discounting_SystematicCriteria",
                title = NULL
              ),
              list(
                extend = "excel",
                filename = "shinybeez_Discounting_SystematicCriteria",
                title = NULL
              ),
              list(
                extend = "pdf",
                filename = "shinybeez_Discounting_SystematicCriteria",
                title = NULL
              )
            ),
            deferRender = TRUE,
            scrollY = 300,
            scroller = TRUE
          )
        )
      })
    })
  })
}
