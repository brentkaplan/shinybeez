box::use(
  beezdemand[CheckUnsystematic, GetDescriptives, GetEmpirical],
  bslib,
  dplyr,
  DT,
  rhino,
  shiny,
  stats,
)

box::use(
  app/logic/utils,
  app/logic/validate,
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

    # show data table
    shiny$observe({
      shiny$req(session$userData$data$demand)
      rhino$log$info("Printing Demand Datatable")
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
              list(extend = "csv", filename = "ShinyBeez_Demand_Data", title = NULL),
              list(extend = "excel", filename = "ShinyBeez_Demand_Data", title = NULL),
              list(extend = "pdf", filename = "ShinyBeez_Demand_Data", title = NULL)
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
      rhino$log$info("Printing Demand Descriptives Table")
      descriptives <- NULL
      if (isgroup()) {
        if (!"group" %in% colnames(data_r$data_d)) {
          shiny$showNotification(
            "You have selected to group the data but there is no
            'group' column in the data.",
            type = "error",
            duration = 10
          )
          return()
        }
        data_g <- data_r$data_d |>
          dplyr$mutate(group = "aggregate")
        data_g_e <- data_g |>
          GetDescriptives(dat = _, bwplot = FALSE) |>
          dplyr$mutate(group = "aggregate")
        data_d_e <- data_r$data_d |>
          dplyr$group_by(group) |>
          dplyr$group_modify(~ GetDescriptives(dat = .x, bwplot = FALSE))
        descriptives <- dplyr$bind_rows(data_g_e, data_d_e) |>
          dplyr$relocate(group, .before = Price)
      } else {
        descriptives <- data_r$data_d |>
          GetDescriptives(dat = _, bwplot = FALSE)
      }

      descriptives <- descriptives |>
        dplyr$mutate(dplyr$across(dplyr$where(is.numeric), round, 2))

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
              list(extend = "csv", filename = "ShinyBeez_Demand_Descriptives", title = NULL),
              list(extend = "excel", filename = "ShinyBeez_Demand_Descriptives", title = NULL),
              list(extend = "pdf", filename = "ShinyBeez_Demand_Descriptives", title = NULL)
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
      empirical <- NULL
      rhino$log$info(
        paste0(
          "Calculating empirical demand data with grouping = ", isgroup()
        )
      )
      data_g <- stats$aggregate(y ~ x, data_r$data_d, mean, na.rm = TRUE)
      data_g$id <- "aggregate"
      if (isgroup()) {
        if (!"group" %in% colnames(data_r$data_d)) {
          shiny$showNotification(
            "You have selected to group the data but there is no
            'group' column in the data.",
            type = "error",
            duration = 10
          )
          return()
        }
        data_gg <- stats$aggregate(y ~ x + group, data_r$data_d, mean, na.rm = TRUE)
        data_gg$id <- "group aggregate"
        data_g_emp <- GetEmpirical(data_g) |>
          dplyr$mutate(
                 # `GM(O)` = utils$geomean(data_g$x * data_g$y),
                 dplyr$across(dplyr$where(is.numeric), round, 1),
                 group = "aggregate") |>
          dplyr$relocate(group, .before = id)
        data_gg_emp <- dplyr$group_by(data_gg, group) |>
          dplyr$group_modify(~ GetEmpirical(.x))
        data_d_emp <- dplyr$group_by(data_r$data_d, group) |>
          dplyr$group_modify(~ GetEmpirical(.x))
        empirical <- dplyr$bind_rows(data_g_emp, data_gg_emp, data_d_emp)
      } else {
        data_g_emp <- GetEmpirical(data_g)
        data_d_emp <- try(GetEmpirical(data_r$data_d), silent = TRUE)
        if (inherits(data_d_emp, "try-error")) {
          shiny$showNotification(
            "Do you have a grouping variable you didn't specify in the 'Specs' dropdown?",
            type = "error",
            duration = 10
          )
          return()
        } else {
          empirical <- dplyr$bind_rows(data_g_emp, data_d_emp)
        }
      }

      empirical <- empirical |>
        dplyr$mutate(dplyr$across(dplyr$where(is.numeric), round, 2))

      output$empirical_table <- DT$renderDT(server = FALSE, {
        DT$datatable(
          empirical,
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
              list(extend = "csv", filename = "ShinyBeez_Demand_Empirical_Measures", title = NULL),
              list(extend = "excel", filename = "ShinyBeez_Demand_Empirical_Measures", title = NULL),
              list(extend = "pdf", filename = "ShinyBeez_Demand_Empirical_Measures", title = NULL)
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
      systematic <- NULL
      rhino$log$info("Calculating systematic criteria")
      if (isgroup()) {
        if (!"group" %in% colnames(data_r$data_d)) {
          shiny$showNotification(
            "You have selected to group the data but there is no
            'group' column in the data.",
            type = "error",
            duration = 10
          )
          return()
        }
        # for each unique group, calculate the systematic criteria
        systematic <- data_r$data_d |>
          dplyr$group_by(group) |>
          dplyr$group_modify(~ CheckUnsystematic(
            dat = .x,
            deltaq = input$deltaq,
            bounce = input$bounce,
            reversals = input$reversals,
            ncons0 = input$ncons0
          ))
      } else {
        systematic <- data_r$data_d |>
          CheckUnsystematic(
            dat = _,
            deltaq = input$deltaq,
            bounce = input$bounce,
            reversals = input$reversals,
            ncons0 = input$ncons0
          )
      }

      output$systematic_table <- DT$renderDT(server = FALSE, {
        DT$datatable(
          systematic,
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
              list(extend = "csv", filename = "ShinyBeez_Demand_Systematic_Criteria", title = NULL),
              list(extend = "excel", filename = "ShinyBeez_Demand_Systematic_Criteria", title = NULL),
              list(extend = "pdf", filename = "ShinyBeez_Demand_Systematic_Criteria", title = NULL)
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
