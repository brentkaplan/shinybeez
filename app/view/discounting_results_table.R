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
  app / logic / telemetry_utils,
  app / logic / utils,
  app / view / shared / data_table[build_datatable],
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
        telemetry_utils$track_configuration(
          "discounting",
          config = list(type = "MCQ", imputation = imputation(), transformation = trans()),
          session = session
        )
        telemetry_utils$track_model_fitting(
          "discounting",
          parameters = list(type = "MCQ", imputation = imputation(), transformation = trans()),
          status = "started",
          session = session
        )
        mcq_out <- tryCatch(
          session_logger$with_performance("mcq_scoring", function() {
            scoring$score_and_format_mcq(
              data_r$data_d,
              imputation = imputation(),
              trans = trans()
            )
          }, always_log = TRUE),
          error = function(e) {
            friendly_msg <- scoring$friendly_discounting_error(e$message)
            shiny$showNotification(
              friendly_msg,
              type = "error",
              duration = 10
            )
            session_logger$error_enhanced(
              paste("MCQ scoring failed:", e$message),
              error_object = e,
              context = "mcq_scoring"
            )
            telemetry_utils$track_model_fitting(
              "discounting",
              parameters = list(type = "MCQ"),
              status = "failed",
              session = session
            )
            return(NULL)
          }
        )
        if (is.null(mcq_out)) return(list())
        telemetry_utils$track_model_fitting(
          "discounting",
          parameters = list(type = "MCQ"),
          status = "completed",
          session = session
        )
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
        # Gate on the format BEFORE scoring. compute_five_trial_dd() also guards,
        # but an abort there would be routed through friendly_discounting_error()
        # and prefixed with the generic "An error occurred during scoring:"
        # fallback, burying a message that is already specific.
        fmt_chk <- five_trial$validate_five_trial(data_r$data_d)
        if (is.character(fmt_chk)) {
          telemetry_utils$track_validation(
            "discounting", "failure", "five_trial_format", fmt_chk, session
          )
          shiny$showNotification(fmt_chk, type = "error", duration = NULL)
          return(list())
        }
        telemetry_utils$track_configuration(
          "discounting",
          config = list(type = "5.5_DD"),
          session = session
        )
        telemetry_utils$track_model_fitting(
          "discounting",
          parameters = list(type = "5.5_DD"),
          status = "started",
          session = session
        )
        dd_out <- tryCatch(
          session_logger$with_performance(
            "five_trial_dd", function() {
              five_trial$compute_five_trial_dd(data_r$data_d)
            },
            always_log = TRUE
          ),
          error = function(e) {
            friendly_msg <- scoring$friendly_discounting_error(e$message)
            shiny$showNotification(friendly_msg, type = "error", duration = 10)
            session_logger$error_enhanced(
              paste("5.5 Trial DD failed:", e$message),
              error_object = e,
              context = "five_trial_dd"
            )
            telemetry_utils$track_model_fitting(
              "discounting",
              parameters = list(type = "5.5_DD"),
              status = "failed",
              session = session
            )
            return(NULL)
          }
        )
        if (is.null(dd_out)) return(list())
        telemetry_utils$track_model_fitting(
          "discounting",
          parameters = list(type = "5.5_DD"),
          status = "completed",
          session = session
        )
        res$results <- dd_out
      } else if (type() == "5.5 Trial Probability Discounting") {
        rhino$log$info("Calculating 5.5 Trial PD")
        fmt_chk <- five_trial$validate_five_trial(data_r$data_d)
        if (is.character(fmt_chk)) {
          telemetry_utils$track_validation(
            "discounting", "failure", "five_trial_format", fmt_chk, session
          )
          shiny$showNotification(fmt_chk, type = "error", duration = NULL)
          return(list())
        }
        telemetry_utils$track_configuration(
          "discounting",
          config = list(type = "5.5_PD"),
          session = session
        )
        telemetry_utils$track_model_fitting(
          "discounting",
          parameters = list(type = "5.5_PD"),
          status = "started",
          session = session
        )
        pd_out <- tryCatch(
          session_logger$with_performance(
            "five_trial_pd", function() {
              five_trial$compute_five_trial_pd(data_r$data_d)
            },
            always_log = TRUE
          ),
          error = function(e) {
            friendly_msg <- scoring$friendly_discounting_error(e$message)
            shiny$showNotification(friendly_msg, type = "error", duration = 10)
            session_logger$error_enhanced(
              paste("5.5 Trial PD failed:", e$message),
              error_object = e,
              context = "five_trial_pd"
            )
            telemetry_utils$track_model_fitting(
              "discounting",
              parameters = list(type = "5.5_PD"),
              status = "failed",
              session = session
            )
            return(NULL)
          }
        )
        if (is.null(pd_out)) return(list())
        telemetry_utils$track_model_fitting(
          "discounting",
          parameters = list(type = "5.5_PD"),
          status = "completed",
          session = session
        )
        res$results <- pd_out
      } else if (type() == "Indifference Point Regression") {
        rhino$log$debug(paste("Equation:", eq(), "; Aggregation:", agg()))
        rhino$log$info("Calculating Regression")
        telemetry_utils$track_configuration(
          "discounting",
          config = list(type = "IDP", equation = eq(), method = agg()),
          session = session
        )
        telemetry_utils$track_model_fitting(
          "discounting",
          parameters = list(type = "IDP", equation = eq(), method = agg()),
          status = "started",
          session = session
        )
        reg_out <- tryCatch(
          session_logger$with_performance(
            "discounting_regression_fit", function() {
              regression$fit_and_format_regression(
                data_r$data_d,
                equation = eq(),
                method = agg()
              )
            },
            always_log = TRUE
          ),
          error = function(e) {
            friendly_msg <- scoring$friendly_discounting_error(e$message)
            shiny$showNotification(friendly_msg, type = "error", duration = 10)
            session_logger$error_enhanced(
              paste("Regression fitting failed:", e$message),
              error_object = e,
              context = "discounting_regression"
            )
            telemetry_utils$track_model_fitting(
              "discounting",
              parameters = list(type = "IDP"),
              status = "failed",
              session = session
            )
            return(NULL)
          }
        )
        if (is.null(reg_out)) return(list())
        telemetry_utils$track_model_fitting(
          "discounting",
          parameters = list(type = "IDP"),
          status = "completed",
          session = session
        )
        res$dd_fit <- reg_out$dd_fit
        res$results <- reg_out$results
      }
      return(res)
    })

    # Render outputs
    output$results_table <- DT$renderDT(server = FALSE, {
      shiny$req(main_calc_reactive()$results)
      build_datatable(
        main_calc_reactive()$results,
        filename_prefix = "shinybeez_Discounting_Results",
        scroll_y = 400,
        page_length = 20
      )
    })

    output$summary_table <- DT$renderDT(server = FALSE, {
      shiny$req(main_calc_reactive()$summary)
      build_datatable(
        main_calc_reactive()$summary,
        filename_prefix = "shinybeez_Discounting_Summary",
        scroll_y = 300
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
      # Correlation table is a special case: no scroller, no PDF, no ordering
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
      build_datatable(
        main_calc_reactive()$data,
        filename_prefix = "shinybeez_Discounting_ImputedData",
        scroll_y = 400,
        page_length = 20
      )
    })

    esquisse$render_ggplot(
      id = "prop_plot",
      expr = utils$apply_dark_mode_theme(
        main_calc_reactive()$propplot,
        session$rootScope()$input$dark_mode
      )
    )
    esquisse$render_ggplot(
      id = "boxplot_plot",
      expr = utils$apply_dark_mode_theme(
        main_calc_reactive()$boxplot,
        session$rootScope()$input$dark_mode
      )
    )

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

    esquisse$render_ggplot(
      id = "regression_plot",
      expr = utils$apply_dark_mode_theme(
        plot_object_reactive(),
        session$rootScope()$input$dark_mode
      )
    )

  })
}
