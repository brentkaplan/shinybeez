box::use(
  beezdemand[theme_apa],
  bslib,
  dplyr,
  DT[DTOutput, renderDT],
  esquisse,
  ggplot2,
  htmltools[tagList],
  rhino,
  shiny,
  stats[aggregate],
)

box::use(
  app / logic / demand / fitting,
  app / logic / utils,
  app / logic / validate,
  app / logic / logging_utils,
  app / view / shared / data_table[build_datatable],
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
            shiny$selectInput(
              inputId = ns("palette"),
              label = "Color Palette",
              choices = c("Okabe-Ito", "HCL Light", "HCL Dark"),
              selected = "Okabe-Ito"
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
  id,
  data_r,
  eq = NULL,
  agg = NULL,
  fix_q0 = FALSE,
  q0_val = NULL,
  groupcol = NULL,
  mem = FALSE,
  kval,
  calculate_btn
) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create session-specific logger
    session_logger <- logging_utils$create_session_logger(session)
    session_logger$info(
      "Demand results table module initialized",
      "module_init"
    )

    res <- shiny$reactiveValues(
      data = NULL,
      output = NULL,
      results = NULL,
      plot = NULL
    )

    shiny$observe({
      eq_code <- fitting$resolve_equation(eq())
      k <- fitting$resolve_k_value(kval(), validate$k_values)
      agg_val <- fitting$resolve_aggregation(agg())
      constrainq0 <- fitting$resolve_q0_constraint(q0_val(), fix_q0())
      analysis_type <- agg()
      is_grouped <- !is.null(groupcol()) && groupcol() && analysis_type != "Ind"

      session_logger$info(
        paste("Fitting demand: eq =", eq_code, "; k =", k,
              "; agg =", agg_val, "; grouped =", is_grouped),
        "model_fitting"
      )

      shiny$withProgress(message = "Fitting demand curves...", {
        fit_result <- tryCatch(
          session_logger$with_performance("demand_curve_fitting", function() {
            if (is_grouped) {
              fitting$fit_demand_grouped(
                data_r$data_d, eq = eq_code, agg = agg_val,
                k = k, constrainq0 = constrainq0
              )
            } else {
              fitting$fit_demand_ungrouped(
                data_r$data_d, eq = eq_code, agg = agg_val,
                k = k, constrainq0 = constrainq0
              )
            }
          }),
          error = function(e) {
            rhino$log$error(paste("Error in FitCurves:", e$message))
            shiny$showNotification(
              paste("Error fitting demand curves:", e$message),
              type = "error", duration = NULL
            )
            NULL
          }
        )
      })

      if (!is.null(fit_result)) {
        res$output <- fit_result$output
        res$results <- fit_result$results
        if (length(fit_result$failed_groups) > 0) {
          shiny$showNotification(
            paste(
              "Fitting failed for groups:",
              paste(fit_result$failed_groups, collapse = ", ")
            ),
            type = "warning",
            duration = NULL
          )
        }
        shiny$showNotification(
          "Model fitting complete. See Model Results tab.",
          type = "message",
          duration = 5
        )
      } else {
        res$output <- NULL
        res$results <- NULL
      }
    }) |>
      shiny$bindEvent(calculate_btn())

    output$model_results_table <- renderDT(server = FALSE, {
      shiny$req(res$results)
      build_datatable(
        res$results,
        filename_prefix = "shinybeez_Demand_ModelResults",
        fixed_columns = 1L,
        page_length = 20,
        fill_container = TRUE
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

      # Only create plots if we have valid output from FitCurves
      if (is.null(res$output)) {
        return()
      }

      if (analysis_type %in% c("Mean")) {
        if (!groupcol()) {
          data_g <- aggregate(y ~ x, data_r$data_d, mean, na.rm = TRUE)
          res$plot <- data_g |>
            ggplot2$ggplot(ggplot2$aes(x = x, y = y)) +
            ggplot2$geom_line(
              ggplot2$aes(x = x, y = y),
              data = res$output$predictions[[1]]
            ) +
            ggplot2$geom_point(
              shape = pt_shape,
              fill = pt_fill,
              size = pt_size
            ) +
            theme_apa()
        } else {
          data_g <- aggregate(y ~ x + group, data_r$data_d, mean, na.rm = TRUE)
          res$plot <- data_g |>
            ggplot2$ggplot(ggplot2$aes(x = x, y = y, group = group)) +
            ggplot2$geom_line(
              ggplot2$aes(x = x, y = y, color = group),
              data = res$output$predictions
            ) +
            ggplot2$geom_point(
              ggplot2$aes(color = group),
              shape = pt_shape,
              fill = pt_fill,
              size = pt_size
            ) +
            theme_apa()
        }
      } else if (analysis_type %in% "Ind") {
        res$plot <- data_r$data_d |>
          ggplot2$ggplot(ggplot2$aes(x = x, y = y, group = id)) +
          ggplot2$geom_line(
            ggplot2$aes(x = x, y = y, group = id),
            data = dplyr$bind_rows(res$output$predictions),
            alpha = 0.33
          ) +
          theme_apa()
        if (length(unique(data_r$data_d$id)) < 51) {
          res$plot <- data_r$data_d |>
            ggplot2$ggplot(ggplot2$aes(x = x, y = y)) +
            ggplot2$geom_line(
              ggplot2$aes(x = x, y = y),
              data = dplyr$bind_rows(res$output$predictions)
            ) +
            ggplot2$geom_point(
              shape = pt_shape,
              fill = pt_fill,
              size = pt_size
            ) +
            theme_apa() +
            ggplot2$facet_wrap(~id)
        }
      } else {
        if (!groupcol()) {
          res$plot <- data_r$data_d |>
            ggplot2$ggplot(ggplot2$aes(x = x, y = y)) +
            ggplot2$geom_line(
              ggplot2$aes(x = x, y = y),
              data = res$output$predictions[[1]]
            ) +
            ggplot2$geom_point(
              shape = pt_shape,
              fill = pt_fill,
              size = pt_size
            ) +
            theme_apa()
        } else {
          res$plot <- data_r$data_d |>
            ggplot2$ggplot(ggplot2$aes(x = x, y = y, group = group)) +
            ggplot2$geom_line(
              ggplot2$aes(x = x, y = y, color = group),
              data = res$output$predictions
            ) +
            ggplot2$geom_point(
              ggplot2$aes(color = group),
              shape = pt_shape,
              fill = pt_fill,
              size = pt_size
            ) +
            theme_apa()
        }
      }
    }) |>
      shiny$bindEvent(calculate_btn())

    shiny$observe({
      if (is.null(res$plot)) {
        return()
      }
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
          ggplot2$guides(
            color = ggplot2$guide_legend(title = input$legend_title)
          )
        # Apply discrete palette for groups
        n_groups <- length(unique(data_r$data_d$group))
        res$plot <- res$plot +
          ggplot2$scale_color_manual(
            values = utils$get_palette_colors(input$palette, n_groups)
          )
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
