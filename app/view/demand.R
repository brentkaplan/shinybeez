box::use(
  bsicons,
  bslib,
  shiny,
)

box::use(
  app / logic / validate,
  app / logic / logging_utils,
  app / view / demand_data_table,
  app / view / demand_results_table,
  app / view / file_input,
)

#' @export
sidebar_ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(
    shiny$div(
      class = "mb-3 small text-muted",
      shiny$tags$ol(
        class = "ps-3",
        shiny$tags$li("Upload your data"),
        shiny$tags$li("Configure settings"),
        shiny$tags$li("Run model")
      )
    ),
    file_input$ui(ns("upload_demand")),
    shiny$checkboxInput(
      inputId = ns("group"),
      label = bslib$tooltip(
        trigger = list(
          "Do you have a grouping variable?",
          bsicons$bs_icon("info-circle")
        ),
        "Grouping will be disregarded if Analysis Type = Two Stage."
      ),
      value = FALSE
    ),
    bslib$accordion(
      open = FALSE,
      style = "margin-bottom: 25px;",
      bslib$accordion_panel(
        title = "Specs",
        icon = bsicons$bs_icon("gear"),
        shiny$checkboxInput(
          inputId = ns("check_free"),
          label = bslib$tooltip(
            trigger = list(
              "Do you want to constrain Q0 to a specific value?",
              bsicons$bs_icon("info-circle")
            ),
            "Currently an experimental feature. Selecting Yes will only work
            with a fixed k value.",
          ),
          value = FALSE
        ),
        shiny$uiOutput(
          ns("num_free")
        ),
      )
    ),
    shiny$selectInput(
      inputId = ns("equation"),
      label = "Select equation:",
      choices = c(
        "Exponentiated (with k)",
        "Exponential (with k)",
        "Simplified (no k)"
      )
    ),
    shiny$uiOutput(
      ns("k_value")
    ),
    shiny$radioButtons(
      inputId = ns("analysis_type"),
      label = "Analysis Type",
      choices = list(
        "Fit to Group (pooled)" = "Pooled",
        "Fit to Group (mean)" = "Mean",
        "Two Stage" = "Ind"
      ),
      selected = "Pooled"
    ),
    shiny$uiOutput(
      ns("calculate")
    )
  )
}

#' @export
sidebar_server <- function(id, fit_task) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create session-specific logger
    session_logger <- logging_utils$create_session_logger(session)
    session_logger$info("Demand sidebar module initialized", "module_init")

    file_input$server("upload_demand", type = "demand")

    output$num_free <- shiny$renderUI({
      if (input$check_free) {
        shiny$numericInput(
          inputId = ns("q0_val"),
          label = bslib$tooltip(
            trigger = list(
              "What number would you like to constrain Q0 to?",
              bsicons$bs_icon("info-circle")
            ),
            "Currently an experimental feature. Selecting Yes will only work
        with a fixed k value."
          ),
          value = 0,
          min = 0,
          max = 100
        )
      }
    })

    output$k_value <- shiny$renderUI({
      k_choices <- if (input$check_free) {
        validate$k_values
      } else {
        ks <- c(validate$k_values, "ind", "fit", "range")
        names(ks) <- c(
          as.character(validate$k_values),
          "Individual k",
          "Fitted k",
          "Empirical range (+.5)"
        )
        ks
      }

      if (
        input$equation %in%
          c(
            "Exponentiated (with k)",
            "Exponential (with k)"
          )
      ) {
        if (input$analysis_type %in% c("Pooled", "Mean", "Ind")) {
          shiny$selectInput(
            inputId = ns("k"),
            label = bslib$tooltip(
              trigger = list(
                "Select k value ",
                bsicons$bs_icon("info-circle")
              ),
              "Please refer at the documentation on the welcome page
              for insight on selecting a k value."
            ),
            choices = k_choices,
            selected = input$k
          )
        }
      }
    })

    # The fit runs on a mirai daemon (app/logic/async), so the button reports
    # busy from the task itself rather than from a blocked session.
    output$calculate <- shiny$renderUI({
      shiny$req(session$userData$data$demand)
      shiny$tagList(
        bslib$input_task_button(
          ns("calculate_demand"),
          "Run Fixed Effects Model",
          icon = shiny$icon("cogs"),
          label_busy = "Fitting curves...",
          class = "w-100"
        ),
        shiny$uiOutput(ns("cancel_demand_ui")),
        shiny$div(
          class = "text-muted small mt-1",
          shiny$textOutput(ns("fit_status_text"))
        )
      )
    })

    # bind_task_button reaches an input only once it is bound in the DOM. The
    # button is rendered on upload, before any click can start a fit, so it is
    # always present when the busy state is sent; a button inserted *during* a
    # running task would miss that state.
    bslib$bind_task_button(fit_task$task, "calculate_demand", session = session)

    output$cancel_demand_ui <- shiny$renderUI({
      if (!identical(fit_task$status(), "running")) {
        return(NULL)
      }
      shiny$actionButton(
        ns("cancel_demand"),
        "Cancel",
        class = "btn-outline-secondary w-100 mt-1"
      )
    })

    shiny$observeEvent(input$cancel_demand, fit_task$cancel())

    output$fit_status_text <- shiny$renderText({
      if (!identical(fit_task$status(), "running")) {
        return("")
      }
      shiny$invalidateLater(1000)
      secs <- round(
        as.numeric(difftime(Sys.time(), fit_task$started_at(), units = "secs"))
      )
      sprintf("Fitting... %ds elapsed. The rest of the app stays usable.", secs)
    })

    # Log when user initiates calculation
    shiny$observeEvent(input$calculate_demand, {
      session_logger$user_activity(
        action = "Demand model calculation initiated",
        input_id = "calculate_demand",
        input_value = input$analysis_type,
        module = "demand"
      )
    })
  })
}

#' @export
navpanel_ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(
    shiny$uiOutput(ns("demand_content"))
  )
}

#' @export
navpanel_server <- function(id, fit_task) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create session-specific logger
    session_logger <- logging_utils$create_session_logger(session)

    data_r <- shiny$reactiveValues(data_d = NULL)

    shiny$observe({
      shiny$req(session$userData$data$demand)
      data_r$data_d <- validate$rename_cols(session$userData$data$demand) |>
        validate$reshape_data(dat = _) |>
        validate$retype_data(dat = _)
    })

    # Show empty state or data content
    output$demand_content <- shiny$renderUI({
      if (is.null(session$userData$data$demand)) {
        bslib$card(
          class = "text-center border-dashed",
          style = "border: 2px dashed var(--bs-border-color); padding: 3rem 2rem;",
          shiny$div(
            bsicons$bs_icon("cloud-arrow-up", size = "3rem", class = "text-muted mb-3"),
            shiny$h4("No data uploaded", class = "text-muted"),
            shiny$p(
              class = "text-muted mb-3",
              "Upload a CSV or TSV file using the sidebar to get started."
            ),
            shiny$p(
              class = "text-muted small",
              "Expected format: columns for ",
              shiny$tags$code("id"), ", ",
              shiny$tags$code("x"), " (price), and ",
              shiny$tags$code("y"), " (consumption)."
            )
          )
        )
      } else {
        shiny$tagList(
          demand_data_table$ui(ns("data_table_demand")),
          demand_results_table$ui(ns("results_table_demand"))
        )
      }
    })

    demand_data_table$server(
      "data_table_demand",
      isgroup = shiny$reactive(input$group),
      data_r = data_r
    )

    demand_results_table$server(
      "results_table_demand",
      data_r = data_r,
      eq = shiny$reactive(input$equation),
      agg = shiny$reactive(input$analysis_type),
      fix_q0 = shiny$reactive(input$check_free),
      q0_val = shiny$reactive(input$q0_val),
      kval = shiny$reactive(input$k),
      calculate_btn = shiny$reactive(input$calculate_demand),
      groupcol = shiny$reactive(input$group),
      fit_task = fit_task
    )
  })
}
