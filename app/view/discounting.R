box::use(
  bsicons,
  bslib,
  shiny,
)

box::use(
  app/logic/validate,
  app/view/discounting_data_table,
  app/view/discounting_results_table,
  app/view/file_input,
)

#' @export
sidebar_ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(
    file_input$ui(ns("discounting")),

    shiny$selectInput(
      inputId = ns("calc_discounting"),
      label = "What are you scoring?",
      choices = c(
        "Indifference Point Regression",
        "27-Item MCQ",
        "5.5 Trial Delay Discounting",
        "5.5 Trial Probability Discounting"
      )
    ),
    shiny$uiOutput(
     outputId = ns("dd_eq")
    ),
    shiny$uiOutput(
      outputId = ns("dd_method")
    ),
    shiny$uiOutput(
      outputId = ns("mcq_imputation")
    ),
    shiny$uiOutput(
      outputId = ns("mcq_trans")
    ),
    shiny$uiOutput(
      ns("calculate")
    )
  )
}

#' @export
sidebar_server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    file_input$server(
      "discounting",
      type = "discounting"
    )

    shiny$observe({
      shiny$req(session$userData$data$discounting)

      output$mcq_imputation <- shiny$renderUI({
        if (input$calc_discounting == "27-Item MCQ") {
          shiny$tagList(
            shiny$selectInput(
              inputId = ns("imputation"),
              label = bslib$tooltip(
                trigger = list(
                  "Impute missing values with:",
                  bsicons$bs_icon("info-circle")
                ),
                "GGM = Group Geometric Mean imputation; INN = Item Nearest
                Neighbor imputation. Please
                refer to the documentation for more information."
              ),
              choices = c(
                "None" = "none",
                "GGM" = "GGM",
                "INN (no random)" = "INN",
                "INN (random)" = "INN_random"
              ),
              selected = "None"
            )
          )
        } else {
          shiny$div()
        }
      })

      output$mcq_trans <- shiny$renderUI({
        if (input$calc_discounting == "27-Item MCQ") {
          shiny$tagList(
            shiny$selectInput(
              inputId = ns("trans"),
              label = bslib$tooltip(
                trigger = list(
                  "Transform k values using:",
                  bsicons$bs_icon("info-circle")
                ),
                "None = No transformation;
                Log = Log transformation (base 10);
                Ln = Natural log (base e)."
              ),
              choices = c(
                "None" = "none",
                "Log" = "log",
                "Ln" = "ln"
              ),
              selected = "None"
            )
          )
        } else {
          shiny$div()
        }
      })

      output$dd_eq <- shiny$renderUI({
        if (input$calc_discounting == "Indifference Point Regression") {
          shiny$tagList(
            shiny$selectInput(
              inputId = ns("equation"),
              label = "Select equation:",
            choices = list(
              "Mazur Hyperbolic" = "Mazur",
              "Exponential" = "Exponential"
              )
            )
        )
    } else {
          shiny$div()
    }
  })

      output$dd_method <- shiny$renderUI({
        if (input$calc_discounting == "Indifference Point Regression") {
          shiny$radioButtons(
            inputId = ns("analysis_type"),
            label = "Analysis Type",
            choices = list(
              "Fit to Group (pooled)" = "Pooled",
              "Fit to Group (mean)" = "Mean",
              "Two Stage" = "Two Stage"
              ),
            selected = "Pooled"
          )
        }
      })

    output$calculate <- shiny$renderUI({
      shiny$req(session$userData$data$discounting)
      shiny$actionButton(
        inputId = ns("calculate_discounting"),
        label = "Calculate"
      )
    })

    })
})
}

#' @export
navpanel_ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(
    shiny$div(
      style = "min-height:500px; max-height:100vh; overflow:auto;",
      discounting_data_table$ui(
        ns("data_table_discounting")
      ),
    ),
    shiny$div(
      style = "min-height:600px; max-height:100vh; overflow:auto;",
      discounting_results_table$ui(
        ns("results_table_discounting")
      )
    )
  )
}

#' @export
navpanel_server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_r <- shiny$reactiveValues(data_d = NULL)

    shiny$observe({
      shiny$req(session$userData$data$discounting)
      if (ncol(session$userData$data$discounting) == 28) {
        data_r$data_d <- validate$reshape_data(
          session$userData$data$discounting,
          type = "discounting"
        )
      } else if (colnames(session$userData$data$discounting)[1] == "id" & ncol(session$userData$data$discounting) > 3) {
        data_r$data_d <- validate$reshape_data(
          session$userData$data$discounting,
          type = "discounting"
        ) |>
          validate$retype_data(dat = _)
      } else {
        data_r$data_d <- session$userData$data$discounting
      }

    })

    discounting_data_table$server(
      "data_table_discounting",
      data_r = data_r,
      type = shiny$reactive(input$calc_discounting)
    )

    discounting_results_table$server(
      "results_table_discounting",
      data_r = data_r,
      eq = shiny$reactive(input$equation),
      agg = shiny$reactive(input$analysis_type),
      type = shiny$reactive(input$calc_discounting),
      imputation = shiny$reactive(input$imputation),
      trans = shiny$reactive(input$trans),
      calculate_btn = shiny$reactive(input$calculate_discounting)
    )
  })
}
