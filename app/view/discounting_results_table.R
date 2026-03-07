box::use(
  beezdemand,
  beezdiscounting[plot_dd, prop_ss],
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
  app / logic / discounting / five_trial,
  app / logic / discounting / regression,
  app / logic / discounting / scoring,
  app / logic / logging_utils,
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
    session_logger <- logging_utils$create_session_logger(session)

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
        mcq_out <- session_logger$with_performance("mcq_scoring", function() {
          scoring$score_and_format_mcq(
            data_r$data_d,
            imputation = imputation(),
            trans = trans()
          )
        })
        res$results <- mcq_out$results
        res$data <- mcq_out$data
        res$summary <- mcq_out$summary

        plot_data <- if (any(names(res$data) %in% "newresponse")) {
          res$data |> dplyr$mutate(response = newresponse)
        } else {
          res$data
        }
        res$propplot <- plot_data |>
          prop_ss() |>
          plot() +
          beezdemand$theme_apa() +
          utils$add_shiny_logo(utils$watermark_tr)
        res$boxplot <- res$results |>
          plot() +
          beezdemand$theme_apa() +
          ggplot2$scale_x_discrete(
            labels = c("Small k", "Medium k", "Large k", "Geomean k", "Overall k")
          ) +
          utils$add_shiny_logo(utils$watermark_tr)

      } else if (type() == "5.5 Trial Delay Discounting") {
        rhino$log$info("Calculating 5.5 Trial DD")
        res$results <- session_logger$with_performance(
          "five_trial_dd", function() {
            five_trial$compute_five_trial_dd(data_r$data_d)
          }
        )
      } else if (type() == "5.5 Trial Probability Discounting") {
        rhino$log$info("Calculating 5.5 Trial PD")
        res$results <- session_logger$with_performance(
          "five_trial_pd", function() {
            five_trial$compute_five_trial_pd(data_r$data_d)
          }
        )
      } else if (type() == "Indifference Point Regression") {
        rhino$log$debug(paste("Equation:", eq(), "; Aggregation:", agg()))
        rhino$log$info("Calculating Regression")
        reg_out <- session_logger$with_performance(
          "discounting_regression_fit", function() {
            regression$fit_and_format_regression(
              data_r$data_d,
              equation = eq(),
              method = agg()
            )
          }
        )
        res$dd_fit <- reg_out$dd_fit
        res$results <- reg_out$results
      }
      return(res)
    })

    # Render outputs
    output$results_table <- DT$renderDT(server = FALSE, {
      shiny$req(main_calc_reactive()$results)
      DT$datatable(
        main_calc_reactive()$results,
        rownames = FALSE,
        extensions = c("Buttons", "Scroller"),
        fillContainer = FALSE,
        options = list(
          pageLength = 20,
          autoWidth = TRUE,
          ordering = TRUE,
          dom = "Btipl",
          buttons = list(
            list(extend = "copy"),
            list(extend = "print"),
            list(
              extend = "csv",
              filename = "shinybeez_Discounting_Results",
              title = NULL
            ),
            list(
              extend = "excel",
              filename = "shinybeez_Discounting_Results",
              title = NULL
            ),
            list(
              extend = "pdf",
              filename = "shinybeez_Discounting_Results",
              title = NULL
            )
          ),
          deferRender = TRUE,
          scrollY = 400,
          scroller = TRUE
        )
      )
    })

    output$summary_table <- DT$renderDT(server = FALSE, {
      shiny$req(main_calc_reactive()$summary)
      DT$datatable(
        main_calc_reactive()$summary,
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
              filename = "shinybeez_Discounting_Summary",
              title = NULL
            ),
            list(
              extend = "excel",
              filename = "shinybeez_Discounting_Summary",
              title = NULL
            ),
            list(
              extend = "pdf",
              filename = "shinybeez_Discounting_Summary",
              title = NULL
            )
          ),
          deferRender = TRUE,
          scrollY = 300,
          scroller = TRUE
        )
      )
    })

    output$correlation_table <- DT$renderDT(server = FALSE, {
      shiny$req(main_calc_reactive()$results)
      k_cols <- grep("_k$", names(main_calc_reactive()$results), value = TRUE)
      shiny$req(length(k_cols) > 0)
      cor_data <- main_calc_reactive()$results[, k_cols, drop = FALSE]
      cor_matrix <- round(stats$cor(cor_data, use = "pairwise.complete.obs"), 3)
      cor_df <- as.data.frame(cor_matrix)
      cor_df <- cbind(Variable = rownames(cor_df), cor_df)
      DT$datatable(
        cor_df,
        rownames = FALSE,
        extensions = c("Buttons"),
        fillContainer = FALSE,
        options = list(
          autoWidth = TRUE,
          ordering = FALSE,
          dom = "Bt",
          buttons = list(
            list(extend = "copy"),
            list(extend = "print"),
            list(
              extend = "csv",
              filename = "shinybeez_Discounting_Correlations",
              title = NULL
            ),
            list(
              extend = "excel",
              filename = "shinybeez_Discounting_Correlations",
              title = NULL
            )
          )
        )
      )
    })

    output$imputed_data_table <- DT$renderDT(server = FALSE, {
      shiny$req(main_calc_reactive()$data)
      DT$datatable(
        main_calc_reactive()$data,
        rownames = FALSE,
        extensions = c("Buttons", "Scroller"),
        fillContainer = FALSE,
        options = list(
          pageLength = 20,
          autoWidth = TRUE,
          ordering = TRUE,
          dom = "Btipl",
          buttons = list(
            list(extend = "copy"),
            list(extend = "print"),
            list(
              extend = "csv",
              filename = "shinybeez_Discounting_ImputedData",
              title = NULL
            ),
            list(
              extend = "excel",
              filename = "shinybeez_Discounting_ImputedData",
              title = NULL
            ),
            list(
              extend = "pdf",
              filename = "shinybeez_Discounting_ImputedData",
              title = NULL
            )
          ),
          deferRender = TRUE,
          scrollY = 400,
          scroller = TRUE
        )
      )
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
