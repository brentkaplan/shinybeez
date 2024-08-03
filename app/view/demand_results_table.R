box::use(
  beezdemand[FitCurves, theme_apa],
  bslib,
  dplyr,
  DT[datatable, DTOutput, renderDT],
  esquisse,
  ggplot2,
  htmltools[tagList],
  rhino,
  shiny,
  stats[aggregate, coef],
  tidyr[pivot_longer],
)

box::use(
  app/logic/utils,
  app/logic/validate,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  bslib$navset_card_tab(
    title = "Results Table",
    bslib$nav_panel(
      id = "fitted_results",
      "Model Results",
      DTOutput(ns("model_results_table"))
    ),
    bslib$nav_panel(
      id = "plot_results",
      "Plots",
      bslib$card(
        style = "border: none; ",
        bslib$layout_sidebar(
          fillable = TRUE,
          sidebar = bslib$sidebar(
            title = "Plot Settings",
            open = FALSE,
            shiny$textInput(
              inputId = ns("title"),
              label = "Title Text",
              value = "title"
            ),
            shiny$uiOutput(ns("group_name")),
            shiny$textInput(
              inputId = ns("xtext"),
              label = "X-Axis Text",
              value = "x"
            ),
            shiny$textInput(
              inputId = ns("ytext"),
              label = "Y-Axis Text",
              value = "y"
            ),
            shiny$checkboxInput(
              inputId = ns("xlog"),
              label = "Log X-Axis"
            ),
            shiny$checkboxInput(
              inputId = ns("ylog"),
              label = "Log Y-Axis"
            ),
            shiny$actionButton(
              inputId = ns("update_plot_btn"),
              label = "Update Plot"
            )
          ),
          esquisse$ggplot_output(
            ns("plot"),
            downloads = esquisse$downloads_labels(
              label = esquisse$ph("download-simple"),
              png = tagList(esquisse$ph("image"), "PNG"),
              pdf = NULL,
              svg = tagList(esquisse$ph("browsers"), "SVG"),
              jpeg = tagList(esquisse$ph("image"), "JPEG"),
              pptx = NULL,
              more = tagList(esquisse$ph("gear"), esquisse$i18n("More options"))
            )
          )
        )
      )
    )
  )
}

#' @export
server <- function(
    id, data_r, eq = NULL, agg = NULL,
    fix_q0 = FALSE, q0_val = NULL, groupcol = NULL,
    mem = FALSE, kval, calculate_btn
    ) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    res <- shiny$reactiveValues(
      data = NULL,
      output = NULL,
      results = NULL,
      plot = NULL
    )

    shiny$observe({
      if (kval() %in% validate$k_values) {
        k <- as.numeric(kval())
      } else {
        k <- kval()
      }
      analysis_type <- agg()
      eq <- if (eq() %in% "Exponentiated (with k)") {
        "koff"
      } else if (eq() %in% "Exponential (with k)") {
        "hs"
      }
      agg <- if (is.null(agg()) | agg() %in% "Ind") NULL else agg()
      constrainq0 <- if (is.null(q0_val()) | !fix_q0()) NULL else q0_val()

      rhino$log$info(paste(
        "Calculating demand with options: agg =", agg, "; k =", k,
        "; eq =", eq, "; constrainq0 =", constrainq0
        ))
      if ((is.null(groupcol()) | !groupcol()) | analysis_type %in% "Ind") {
        res$output <- data_r$data_d |>
          FitCurves(
            dat = _,
            eq = eq,
            agg = agg,
            k = k,
            constrainq0 = constrainq0,
            detailed = TRUE
            )
        res$results <- res$output[[1]] |>
          dplyr$select(!(Intensity:Pmaxe)) |>
          dplyr$mutate(dplyr$across(dplyr$all_of(c("Q0d", "K", "R2", "Q0se", "AbsSS", "SdRes",
                                 "Q0Low", "Q0High", "EV", "Omaxd", "Pmaxd",
                                 "Omaxa", "Pmaxa")), \(x) round(x, 2)),
                 dplyr$across(dplyr$all_of(c("Alpha", "Alphase",
                                 "AlphaLow", "AlphaHigh")), \(x) round(x, 4)))
      } else {
        if (!"group" %in% colnames(data_r$data_d)) {
          shiny$showNotification(
            "You have selected to group the data but there is no
            'group' column in the data.",
            type = "error",
            duration = 10
          )
          return()
        }

        res$output <- vector("list", length = 3)
        tmp <- data_r$data_d |>
          dplyr$group_by(group) |>
          dplyr$group_map(~{
            fit_result <- FitCurves(
              dat = .x,
              eq = eq,
              agg = agg,
              k = k,
              constrainq0 = constrainq0,
              detailed = TRUE
            )
            list(
              group = dplyr$first(.x$group),
              fit_result_1 = fit_result[[1]],
              fit_result_3 = fit_result[[3]]
            )
          }, .keep = TRUE)
        res$output[[1]] <- dplyr$bind_rows(lapply(tmp, function(x) {
          cbind(group = x$group, x$fit_result_1)}))
        res$output[[3]] <- dplyr$bind_rows(lapply(tmp, function(x) {
          cbind(group = x$group, x$fit_result_3[[1]])}))
        res$results <- res$output[[1]] |>
          dplyr$select(!(Intensity:Pmaxe)) |>
          dplyr$mutate(dplyr$across(dplyr$all_of(c("Q0d", "K", "R2", "Q0se", "AbsSS", "SdRes",
                                 "Q0Low", "Q0High", "EV", "Omaxd", "Pmaxd",
                                 "Omaxa", "Pmaxa")), \(x) round(x, 2)),
                 dplyr$across(dplyr$all_of(c("Alpha", "Alphase",
                                 "AlphaLow", "AlphaHigh")), \(x) round(x, 4)))

      }
    }) |>
      shiny$bindEvent(calculate_btn())

    output$model_results_table <- renderDT(server = FALSE, {
      shiny$req(res$results)
      datatable(
        res$results,
        rownames = FALSE,
        extensions = c('Buttons', "FixedColumns"),
        fillContainer = TRUE,
        options = list(
          pageLength = 20,
          autoWidth = TRUE,
          ordering = TRUE,
          dom = 'Btipl',
          buttons = list(
            list(extend = 'copy'),
            list(extend = 'print'),
            list(extend = 'csv', filename = "ShinyBeez_Demand_ModelResults", title = NULL),
            list(extend = 'excel', filename = "ShinyBeez_Demand_ModelResults", title = NULL),
            list(extend = 'pdf', filename = "ShinyBeez_Demand_ModelResults", title = NULL)
          ),
          fixedColumns = list(leftColumns = 1)
        )
      )
    })

    shiny$observe({
      if (groupcol()) {
        if (!"group" %in% colnames(data_r$data_d)) {
          shiny$showNotification(
            "You have selected to group the data but there is no
            'group' column in the data.",
            type = "error",
            duration = 10
          )
          return()
        }
        output$group_name <- shiny$renderUI({
          shiny$textInput(
            inputId = ns("legend_title"),
            label = "Legend Title",
            value = "group"
          )
        })
      } else {
        output$group_name <- shiny$renderUI({
          shiny$div()
        })
      }
      analysis_type <- agg()
      res$plot <- NULL
      pt_shape <- 21
      pt_fill <- "white"
      pt_size <- 3
      if (analysis_type %in% c("Mean")) {
        if (!groupcol()) {
          data_g <- aggregate(y ~ x, data_r$data_d, mean, na.rm = TRUE)
          res$plot <- data_g |>
            ggplot2$ggplot(ggplot2$aes(x = x, y = y)) +
            ggplot2$geom_line(
              ggplot2$aes(x = x, y = y),
              data = res$output[[3]][[1]]
            ) +
            ggplot2$geom_point(shape = pt_shape, fill = pt_fill, size = pt_size) +
            theme_apa()
        } else {
          data_g <- aggregate(y ~ x + group, data_r$data_d, mean, na.rm = TRUE)
          res$plot <- data_g |>
            ggplot2$ggplot(ggplot2$aes(x = x, y = y, group = group)) +
            ggplot2$geom_line(
              ggplot2$aes(x = x, y = y, color = group),
              data = res$output[[3]]
            ) +
            ggplot2$geom_point(
              ggplot2$aes(color = group),
              shape = pt_shape,
              fill = pt_fill,
              size = pt_size) +
            theme_apa()
        }
      } else if (analysis_type %in% "Ind") {
        res$plot <- data_r$data_d |>
          ggplot2$ggplot(ggplot2$aes(x = x, y = y, group = id)) +
          ggplot2$geom_line(
            ggplot2$aes(x = x, y = y, group = id),
            data =  dplyr$bind_rows(res$output[[3]]),
            alpha = 0.33
          ) +
          theme_apa()
        if (length(unique(data_r$data_d$id)) < 51) {
          res$plot <- data_r$data_d |>
            ggplot2$ggplot(ggplot2$aes(x = x, y = y)) +
            ggplot2$geom_line(
              ggplot2$aes(x = x, y = y),
              data =  dplyr$bind_rows(res$output[[3]])
            ) +
            ggplot2$geom_point(shape = pt_shape, fill = pt_fill, size = pt_size) +
            theme_apa() +
            ggplot2$facet_wrap(~ id)
        }
      } else {
        if (!groupcol()) {
          res$plot <- data_r$data_d |>
            ggplot2$ggplot(ggplot2$aes(x = x, y = y)) +
            ggplot2$geom_line(
              ggplot2$aes(x = x, y = y),
              data =  res$output[[3]][[1]]
            ) +
            ggplot2$geom_point(shape = pt_shape, fill = pt_fill, size = pt_size) +
            theme_apa()
        } else {
          res$plot <- data_r$data_d |>
            ggplot2$ggplot(ggplot2$aes(x = x, y = y, group = group)) +
            ggplot2$geom_line(
              ggplot2$aes(x = x, y = y, color = group),
              data =  res$output[[3]]
            ) +
            ggplot2$geom_point(
              ggplot2$aes(color = group),
              shape = pt_shape,
              fill = pt_fill,
              size = pt_size) +
            theme_apa()
        }
      }
    }) |>
      shiny$bindEvent(calculate_btn())

    shiny$observe({
      if (is.null(res$plot)) return()
      res$plot <- res$plot +
        ggplot2$xlab(input$xtext) +
        ggplot2$ylab(input$ytext) +
        ggplot2$ggtitle(input$title)

      if (input$xlog) {
        res$plot <- res$plot +
          ggplot2$scale_x_continuous(
            transform = "pseudo_log"
          )
      } else {
        res$plot <- res$plot +
          ggplot2$scale_x_continuous()
      }

      if (input$ylog) {
        res$plot <- res$plot +
          ggplot2$scale_y_continuous(
            transform = "pseudo_log"
          )
      } else {
        res$plot <- res$plot +
          ggplot2$scale_y_continuous()
      }

      if (groupcol()) {
        res$plot <- res$plot +
          ggplot2$guides(color = ggplot2$guide_legend(title = input$legend_title))
      }

      if (agg() != "Ind" | length(unique(data_r$data_d$id)) > 51) {
        res$plot <- res$plot +
          utils$add_shiny_logo(utils$watermark_tr)
      }

      esquisse$render_ggplot(
        id = "plot",
        expr = res$plot,
        filename = "shinybeez-demand"
      )
    }) |>
      shiny$bindEvent(c(calculate_btn(), input$update_plot_btn))
  })
}
