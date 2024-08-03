box::use(
  bslib,
  dplyr,
  DT,
  rhino,
  shiny,
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
      id = "missings",
      "Missing Summary",
      DT$DTOutput(ns("missings_table"))
    )
  )
}

#' @export
server <- function(id, data_r, type = NULL) {
  shiny$moduleServer(id, function(input, output, session) {

    shiny$observe({
      shiny$req(session$userData$data$discounting)

      if (type() %in% "27-Item MCQ" & any(is.na((data_r$data_d$response)))) {
        shiny$showNotification(
          "There appears to be missing data in the 27-Item MCQ. Please be
          cautious when interpreting the results and choosing an appropriate
          imputation method.",
          type = "error",
          duration = 10
        )
      }

      rhino$log$info("Printing Discounting Datatable")
      output$data_table <- DT$renderDT(server = FALSE, {
        DT$datatable(
          data_r$data_d,
          rownames = FALSE,
          extensions = c('Buttons', "Scroller"),
          fillContainer = FALSE,
          autoHideNavigation = TRUE,
          options = list(
            pageLength = 10,
            autoWidth = TRUE,
            ordering = TRUE,
            dom = 'Bti',
            buttons = list(
              list(extend = 'copy'),
              list(extend = 'print'),
              list(extend = 'csv', filename = "ShinyBeez_Discounting_Data", title = NULL),
              list(extend = 'excel', filename = "ShinyBeez_Discounting_Data", title = NULL),
              list(extend = 'pdf', filename = "ShinyBeez_Discounting_Data", title = NULL)
            ),
            deferRender = TRUE,
            scrollY = 300,
            scroller = TRUE
          )
        )
      })

      output$missings_table <- DT$renderDT(server = FALSE, {
        missings <- data_r$data_d |>
          dplyr$group_by(subjectid) |>
          dplyr$summarise(prop_missing = round(sum(is.na("response")) / dplyr$n(), 2)) |>
          dplyr$ungroup()
        DT$datatable(
          missings,
          rownames = FALSE,
          extensions = c('Buttons', "Scroller"),
          fillContainer = FALSE,
          autoHideNavigation = TRUE,
          options = list(
            pageLength = 10,
            autoWidth = TRUE,
            ordering = TRUE,
            dom = 'Bti',
            buttons = list(
              list(extend = 'copy'),
              list(extend = 'print'),
              list(extend = 'csv', filename = "ShinyBeez_Discounting_Missings", title = NULL),
              list(extend = 'excel', filename = "ShinyBeez_Discounting_Missings", title = NULL),
              list(extend = 'pdf', filename = "ShinyBeez_Discounting_Missings", title = NULL)
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
