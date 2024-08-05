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
)

box::use(
  app/logic/utils,
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
    id, data_r, type,
    imputation = "none", trans = "none",
    calculate_btn
    ) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    res <- shiny$reactiveValues(
      data = NULL,
      results = NULL,
      summary = NULL,
      propplot = NULL,
      boxplot = NULL
      )

    # populate the Results Table and tabs based on data and type
    shiny$observe({
      shiny$req(data_r$data_d)
      shiny$req(type())
      output$results_box <- shiny$renderUI(
        if (type() == "27-Item MCQ" & !("I16" %in% names(data_r$data_d))) {
          bslib$navset_card_tab(
            id = ns("tabs"),
            title = "Results Table",
            bslib$nav_panel(
              id = ns("results"),
              "Results",
              DT$DTOutput(ns("results_table"))
            ),
            bslib$nav_panel(
              id = ns("summary"),
              "Summary Statistics",
              DT$DTOutput(ns("summary_table"))
            ),
            bslib$nav_panel(
              id = ns("correlation"),
              "Correlations",
              DT$DTOutput(ns("correlation_table"))
            ),
            bslib$nav_panel(
              id = ns("imputed_data"),
              "Imputed Data",
              DT$DTOutput(ns("imputed_data_table"))
            ),
            bslib$nav_panel(
              id = ns("prop_sir"),
              "Prop SIR/SS",
              esquisse$ggplot_output(
                ns("prop_plot"),
                downloads = esquisse$downloads_labels(
                  label = esquisse$ph("download-simple"),
                  png = htmltools$tagList(esquisse$ph("image"), "PNG"),
                  pdf = NULL,
                  svg = htmltools$tagList(esquisse$ph("browsers"), "SVG"),
                  jpeg = htmltools$tagList(esquisse$ph("image"), "JPEG"),
                  pptx = NULL,
                  more = htmltools$tagList(
                    esquisse$ph("gear"),
                    esquisse$i18n("More options")
                  )
                )
              ),
            ),
            bslib$nav_panel(
              id = ns("boxplots"),
              "Boxplot",
              esquisse$ggplot_output(
                ns("boxplot_plot"),
                downloads = esquisse$downloads_labels(
                  label = esquisse$ph("download-simple"),
                  png = htmltools$tagList(esquisse$ph("image"), "PNG"),
                  pdf = NULL,
                  svg = htmltools$tagList(esquisse$ph("browsers"), "SVG"),
                  jpeg = htmltools$tagList(esquisse$ph("image"), "JPEG"),
                  pptx = NULL,
                  more = htmltools$tagList(
                    esquisse$ph("gear"),
                    esquisse$i18n("More options")
                  )
                )
              ),
            )
          )
        } else {
          bslib$navset_card_tab(
            id = ns("tabs"),
            title = "Results Table",
            bslib$nav_panel(
              id = ns("results"),
              "Results",
              DT$DTOutput(ns("results_table"))
            )
          )
        }
      )
    })

    shiny$observe({
      shiny$req(data_r$data_d)
      # null out any objects left over
      res$data <- NULL
      res$results <- NULL
      res$summary <- NULL
      res$propplot <- NULL
      res$boxplot <- NULL
      trans <- trans()
      if (type() == "27-Item MCQ" & !("I16" %in% names(data_r$data_d))) {
        if (imputation() == "none" || is.null(imputation())) {
          rhino$log$info("Calculating MCQ27 without imputation")
          res$results <- score_mcq27(data_r$data_d, trans = trans)
          res$data <- data_r$data_d
        } else if (imputation() == "GGM") {
          rhino$log$info("Calculating MCQ27 with GGM imputation")
          res$results <- score_mcq27(
            data_r$data_d,
            impute_method = "GGM",
            trans = trans
          )
          res$data <- data_r$data_d
        } else if (imputation() == "INN") {
          rhino$log$info("Calculating MCQ27 with INN imputation")
          result <- score_mcq27(data_r$data_d,
            impute_method = "INN",
            return_data = TRUE,
            trans = trans
          )
          res$results <- result$results
          res$data <- result$data
        } else if (imputation() == "INN_random") {
          rhino$log$info("Calculating MCQ27 with INN random imputation")
          result <- score_mcq27(
            data_r$data_d,
            impute_method = "INN",
            random = TRUE,
            return_data = TRUE,
            trans = trans
          )
          res$results <- result$results
          res$data <- result$data
        }
        # round results
        res$results <- res$results |>
          dplyr$mutate_at(dplyr$vars(dplyr$contains("_k")), ~ round(., 6)) |>
          dplyr$mutate_at(dplyr$vars(dplyr$contains("_prop")), ~ round(., 3)) |>
          dplyr$mutate_at(dplyr$vars(dplyr$contains("_cons")), ~ round(., 3))
        # make summary table
        res$summary <- summarize_mcq(res$results)
        # make sir prop plot
        if (any(names(res$data) %in% "newresponse")) {
          res$propplot <- res$data |>
            dplyr$mutate(response = newresponse) |>
            prop_ss() |>
            plot() +
            beezdemand$theme_apa() +
            utils$add_shiny_logo(utils$watermark_tr)
        } else {
          res$propplot <- res$data |>
            prop_ss() |>
            plot() +
            beezdemand$theme_apa() +
            utils$add_shiny_logo(utils$watermark_tr)
        }
        # make boxplot
        res$boxplot <- res$results |>
          plot() +
          beezdemand$theme_apa() +
          ggplot2$scale_x_discrete(
            labels = c(
              "Small k", "Medium k",
              "Large k", "Geomean k",
              "Overall k"
            )
          ) +
          utils$add_shiny_logo(utils$watermark_tr)
      } else if (type() == "5.5 Trial Delay Discounting") {
        rhino$log$info("Calculating 5.5 Trial DD")
        res$results <- calc_dd(data_r$data_d)
      } else if (type() == "5.5 Trial Probability Discounting") {
        rhino$log$info("Calculating 5.5 Trial PD")
        res$results <- calc_pd(data_r$data_d)
      }
    })  |>
      shiny$bindEvent(calculate_btn())

    output$results_table <- DT$renderDT(server = FALSE, {
      shiny$req(res$results)
      if (type() == "27-Item MCQ") {
        DT$datatable(
          res$results,
          rownames = FALSE,
          extensions = c('Buttons', "Scroller", "FixedColumns"),
          fillContainer = TRUE,
          options = list(
            autoWidth = TRUE,
            ordering = TRUE,
            dom = 'Bti',
            buttons = list(
              list(extend = 'copy'),
              list(extend = 'print'),
              list(
                extend = 'csv',
                filename = "ShinyBeez_Discounting_Results",
                title = NULL
              ),
              list(
                extend = 'excel',
                filename = "ShinyBeez_Discounting_Results",
                title = NULL
              ),
              list(
                extend = 'pdf',
                filename = "ShinyBeez_Discounting_Results",
                title = NULL
              )
            ),
            deferRender = TRUE,
            scrollY = 300,
            scroller = TRUE,
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 1)
          )
        ) |>
          DT$formatStyle(
            c(
              'overall_consistency',
              'small_consistency',
              'medium_consistency',
              'large_consistency',
              'composite_consistency'
            ),
            backgroundColor = DT$styleInterval(.75, c('#FFAEB9', ''))
          )
      } else {
        DT$datatable(
          res$results,
          rownames = FALSE,
          extensions = c('Buttons', "Scroller", "FixedColumns"),
          fillContainer = TRUE,
          options = list(
            autoWidth = TRUE,
            ordering = TRUE,
            dom = 'Bti',
            buttons = list(
              list(extend = 'copy'),
              list(extend = 'print'),
              list(
                extend = 'csv',
                filename = "ShinyBeez_Discounting_Results",
                title = NULL
              ),
              list(
                extend = 'excel',
                filename = "ShinyBeez_Discounting_Results",
                title = NULL
              ),
              list(
                extend = 'pdf',
                filename = "ShinyBeez_Discounting_Results",
                title = NULL
              )
            ),
            deferRender = FALSE,
            scrollY = 300,
            scroller = TRUE,
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 1)
          )
        )
      }
    }) |>
      shiny$bindEvent(calculate_btn())

    output$summary_table <- DT$renderDT(server = FALSE, {
      shiny$req(res$summary)
      DT$datatable(
        res$summary,
        rownames = FALSE,
        extensions = c('Buttons', "Scroller"),
        fillContainer = FALSE,
        options = list(
          autoWidth = TRUE,
          ordering = TRUE,
          dom = 'Bti',
          columnDefs = list(
            list(className = 'dt-center', targets = 1:3),
            list(className = 'dt-right', targets = 0)
            ),
          buttons = list(
            list(extend = 'copy'),
            list(extend = 'print'),
            list(
              extend = 'csv',
              filename = "ShinyBeez_Discounting_Summary",
              title = NULL
            ),
            list(
              extend = 'excel',
              filename = "ShinyBeez_Discounting_Summary",
              title = NULL
            ),
            list(
              extend = 'pdf',
              filename = "ShinyBeez_Discounting_Summary",
              title = NULL
            )
          ),
          deferRender = TRUE,
          scrollY = 250,
          scroller = TRUE
        )
      ) |>
        DT$formatRound(columns = c("Mean", "SD", "SEM"), digits = 4) |>
        DT$formatStyle(
          columns = "Metric",
          textAlign = 'right'
        ) |>
        DT$formatStyle(
          columns = c("Mean", "SD", "SEM"),
          textAlign = 'center'
        )
    }) |>
      shiny$bindEvent(calculate_btn())

    output$correlation_table <- DT$renderDT(server = FALSE, {
      shiny$req(res$results)
      correlations <- NULL
      correlations <- res$results |>
        dplyr$select(contains(c("small_k", "medium_k", "large_k"))) |>
        stats$cor(use = "pairwise.complete.obs") |>
        round(2)
      DT$datatable(
        data = correlations,
        rownames = TRUE,
        extensions = c('Buttons', "Scroller"),
        fillContainer = FALSE,
        autoHideNavigation = TRUE,
        options = list(
          pageLength = 3,
          autoWidth = TRUE,
          ordering = TRUE,
          dom = 'Bt',
          buttons = list(
            list(extend = 'copy'),
            list(extend = 'print'),
            list(
              extend = 'csv',
              filename = "ShinyBeez_Discounting_Summary",
              title = NULL
            ),
            list(
              extend = 'excel',
              filename = "ShinyBeez_Discounting_Summary",
              title = NULL
            ),
            list(
              extend = 'pdf',
              filename = "ShinyBeez_Discounting_Summary",
              title = NULL
            )
          ),
          deferRender = TRUE,
          scrollY = 200,
          scroller = TRUE
        )
      )
    }) |>
      shiny$bindEvent(calculate_btn())

    shiny$observe({
      if (imputation() %in% c("INN", "INN_random")) {
        output$imputed_data_table <- DT$renderDT(server = FALSE, {
          shiny$req(res$data)
          DT$datatable(
            res$data,
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
                list(
                  extend = 'csv',
                  filename = "ShinyBeez_Discounting_Imputed_Data",
                  title = NULL
                ),
                list(
                  extend = 'excel',
                  filename = "ShinyBeez_Discounting_Imputed_Data",
                  title = NULL
                ),
                list(
                  extend = 'pdf',
                  filename = "ShinyBeez_Discounting_Imputed_Data",
                  title = NULL
                )
              ),
              deferRender = TRUE,
              scrollY = 250,
              scroller = TRUE
            )
          )
        })
      }
    }) |>
      shiny$bindEvent(calculate_btn())

    shiny$observe({
      shiny$req(res$propplot)
      esquisse$render_ggplot(
        id = "prop_plot",
        expr = res$propplot,
        filename = "shinybeez-discounting-prop-sir")
    }) |>
      shiny$bindEvent(calculate_btn())

    shiny$observe({
      shiny$req(res$boxplot)
      esquisse$render_ggplot(
        id = "boxplot_plot",
        expr = res$boxplot,
        filename = "shinybeez-discounting-k-boxplots")
    }) |>
      shiny$bindEvent(calculate_btn())

  })
}
