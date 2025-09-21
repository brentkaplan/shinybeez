box::use(
  beezdemand,
  beezdiscounting[...],
  bslib,
  dplyr,
  DT,
  esquisse,
  htmltools,
  ggplot2,
  rhino,
  shiny,
  stats,
  tidyr,
  utils,
)

box::use(
  app / logic / utils,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$uiOutput(
    ns("results_box")
  )
}

#' @export
server <- function(
    id, data_r, eq, agg,
    type,
    imputation = "none",
    trans = "none",
    calculate_btn) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Dynamic UI for results
    output$results_box <- shiny$renderUI({
      shiny$req(type())
      if (type() == "27-Item MCQ" && !("I16" %in% names(data_r$data_d))) {
        bslib$navset_card_tab(
          id = ns("tabs"),
          title = "Results Table",
          bslib$nav_panel("Results", DT$DTOutput(ns("results_table"))),
          bslib$nav_panel("Summary Statistics", DT$DTOutput(ns("summary_table"))),
          bslib$nav_panel("Correlations", DT$DTOutput(ns("correlation_table"))),
          bslib$nav_panel("Imputed Data", DT$DTOutput(ns("imputed_data_table"))),
          bslib$nav_panel("Prop SIR/SS", esquisse$ggplot_output(ns("prop_plot"))),
          bslib$nav_panel("Boxplot", esquisse$ggplot_output(ns("boxplot_plot")))
        )
      } else {
        bslib$navset_card_tab(
          id = ns("tabs"),
          title = "Results Table",
          bslib$nav_panel("Results", DT$DTOutput(ns("results_table"))),
          bslib$nav_panel(
            "Plots",
            bslib$layout_sidebar(
              fillable = TRUE,
              sidebar = bslib$sidebar(
                title = "Plot Settings",
                open = FALSE,
                shiny$textInput(ns("title"), "Title Text", "title"),
                shiny$textInput(ns("xtext"), "X-Axis Text", "x"),
                shiny$textInput(ns("ytext"), "Y-Axis Text", "y"),
                shiny$checkboxInput(ns("xlog"), "Log X-Axis"),
                shiny$actionButton(ns("update_plot_btn"), "Update Plot")
              ),
              esquisse$ggplot_output(ns("regression_plot"))
            )
          )
        )
      }
    })

    # Reactive calculation engine
    main_calc_reactive <- shiny$eventReactive(calculate_btn(), {
      shiny$req(data_r$data_d, type())
      rhino$log$info(paste("Calculating for:", type()))

      res <- list()
      if (type() == "27-Item MCQ" && !("I16" %in% names(data_r$data_d))) {
        rhino$log$debug(paste("Imputation method:", imputation(), "; Transformation:", trans()))
        impute_method <- if (imputation() == "none") NULL else imputation()
        random_impute <- grepl("random", imputation(), ignore.case = TRUE)
        
        calc_results <- score_mcq27(
          data_r$data_d,
          impute_method = impute_method,
          random = random_impute,
          return_data = TRUE,
          trans = trans()
        )
        res$results <- calc_results$results |>
          dplyr$mutate_at(dplyr$vars(dplyr$contains("_k")), ~ round(., 6)) |>
          dplyr$mutate_at(dplyr$vars(dplyr$contains("_prop")), ~ round(., 3)) |>
          dplyr$mutate_at(dplyr$vars(dplyr$contains("_cons")), ~ round(., 3))
        res$data <- calc_results$data
        res$summary <- summarize_mcq(res$results)
        
        plot_data <- if (any(names(res$data) %in% "newresponse")) res$data |> dplyr$mutate(response = newresponse) else res$data
        res$propplot <- plot_data |> prop_ss() |> plot() + beezdemand$theme_apa() + utils$add_shiny_logo(utils$watermark_tr)
        res$boxplot <- res$results |> plot() + beezdemand$theme_apa() + ggplot2$scale_x_discrete(labels = c("Small k", "Medium k", "Large k", "Geomean k", "Overall k")) + utils$add_shiny_logo(utils$watermark_tr)

      } else if (type() == "5.5 Trial Delay Discounting") {
        rhino$log$info("Calculating 5.5 Trial DD")
        res$results <- calc_dd(data_r$data_d)
      } else if (type() == "5.5 Trial Probability Discounting") {
        rhino$log$info("Calculating 5.5 Trial PD")
        res$results <- calc_pd(data_r$data_d)
      } else if (type() == "Indifference Point Regression") {
        rhino$log$debug(paste("Equation:", eq(), "; Aggregation:", agg()))
        rhino$log$info("Calculating Regression")
        res$dd_fit <- fit_dd(data_r$data_d, equation = eq(), method = agg())
        res$results <- results_dd(res$dd_fit) |>
          dplyr$mutate(dplyr$across(where(is.numeric), \(x) round(x, 4))) |>
          dplyr$mutate(id = factor(id, levels = unique(data_r$data_d$id))) |>
          dplyr$arrange(id)
      }
      return(res)
    })

    # Render outputs
    output$results_table <- DT$renderDT({
      shiny$req(main_calc_reactive()$results)
      # ... (DT datatable options as before)
    })

    output$summary_table <- DT$renderDT({
      shiny$req(main_calc_reactive()$summary)
      # ... (DT datatable options as before)
    })

    output$correlation_table <- DT$renderDT({
      shiny$req(main_calc_reactive()$results)
      # ... (correlation logic and DT datatable options as before)
    })

    output$imputed_data_table <- DT$renderDT({
      shiny$req(main_calc_reactive()$data)
      # ... (DT datatable options as before)
    })

    esquisse$render_ggplot(id = "prop_plot", expr = main_calc_reactive()$propplot)
    esquisse$render_ggplot(id = "boxplot_plot", expr = main_calc_reactive()$boxplot)

    plot_object_reactive <- shiny$eventReactive(c(calculate_btn(), input$update_plot_btn), {
        shiny$req(main_calc_reactive()$dd_fit)
        plot_dd(
          main_calc_reactive()$dd_fit,
          xlabel = input$xtext,
          ylabel = input$ytext,
          logx = input$xlog
        ) +
        ggplot2$ggtitle(input$title) +
        utils$add_shiny_logo(utils$watermark_tr)
    })

    esquisse$render_ggplot(id = "regression_plot", expr = plot_object_reactive())

  })
}
