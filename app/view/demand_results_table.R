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
  app / logic / utils,
  app / logic / validate,
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
    id, data_r, eq = NULL, agg = NULL,
    fix_q0 = FALSE, q0_val = NULL, groupcol = NULL,
    mem = FALSE, kval, calculate_btn
) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    main_calc_reactive <- shiny$eventReactive(calculate_btn(), {
      shiny$req(data_r$data_d)
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
      agg <- if (is.null(agg()) || agg() %in% "Ind") NULL else agg()
      constrainq0 <- if (is.null(q0_val()) || !fix_q0()) NULL else q0_val()

      rhino$log$info(paste(
        "Calculating demand with options: agg =", agg, "; k =", k,
        "; eq =", eq, "; constrainq0 =", constrainq0
      ))

      if ((is.null(groupcol()) || !groupcol()) || analysis_type %in% "Ind") {
        output_val <- data_r$data_d |>
          FitCurves(
            dat = _,
            eq = eq,
            agg = agg,
            k = k,
            constrainq0 = constrainq0,
            detailed = TRUE
          )
        results_val <- output_val[[1]] |>
          dplyr$select(!(Intensity:Pmaxe)) |>
          dplyr$mutate(
            dplyr$across(
              dplyr$all_of(c(
                "Q0d", "K", "R2", "Q0se", "AbsSS", "SdRes",
                "Q0Low", "Q0High", "EV", "Omaxd", "Pmaxd",
                "Omaxa", "Pmaxa"
              )),
              \(x) round(x, 2)
            ),
            dplyr$across(
              dplyr$all_of(c(
                "Alpha", "Alphase",
                "AlphaLow", "AlphaHigh"
              )),
              \(x) round(x, 4)
            )
          )
      } else {
        shiny$validate(
          shiny$need("group" %in% colnames(data_r$data_d), "You have selected to group the data but there is no 'group' column in the data.")
        )

        output_val <- vector("list", length = 3)
        tmp <- data_r$data_d |>
          dplyr$group_by(group) |>
          dplyr$group_map(
            ~ {
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
            },
            .keep = TRUE
          )
        output_val[[1]] <- dplyr$bind_rows(lapply(tmp, function(x) {
          cbind(group = x$group, x$fit_result_1)
        }))
        output_val[[3]] <- dplyr$bind_rows(lapply(tmp, function(x) {
          cbind(group = x$group, x$fit_result_3[[1]])
        }))
        results_val <- output_val[[1]] |>
          dplyr$select(!(Intensity:Pmaxe)) |>
          dplyr$mutate(
            dplyr$across(
              dplyr$all_of(c(
                "Q0d", "K", "R2", "Q0se", "AbsSS", "SdRes",
                "Q0Low", "Q0High", "EV", "Omaxd", "Pmaxd",
                "Omaxa", "Pmaxa"
              )),
              \(x) round(x, 2)
            ),
            dplyr$across(
              dplyr$all_of(c(
                "Alpha", "Alphase",
                "AlphaLow", "AlphaHigh"
              )),
              \(x) round(x, 4)
            )
          )
      }
      list(results = results_val, output = output_val)
    })

    output$model_results_table <- renderDT(server = FALSE, {
      main_calc <- main_calc_reactive()
      shiny$req(main_calc$results)
      datatable(
        main_calc$results,
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

    plot_reactive <- shiny$eventReactive(c(calculate_btn(), input$update_plot_btn), {
      main_calc <- main_calc_reactive()
      shiny$req(main_calc)

      if (groupcol()) {
        shiny$validate(
          shiny$need("group" %in% colnames(data_r$data_d), "You have selected to group the data but there is no 'group' column in the data.")
        )
      }

      analysis_type <- agg()
      pt_shape <- 21
      pt_fill <- "white"
      pt_size <- 3

      p <- if (analysis_type %in% c("Mean")) {
        if (!groupcol()) {
          data_g <- aggregate(y ~ x, data_r$data_d, mean, na.rm = TRUE)
          data_g |>
            ggplot2$ggplot(ggplot2$aes(x = x, y = y)) +
            ggplot2$geom_line(
              ggplot2$aes(x = x, y = y),
              data = main_calc$output[[3]][[1]]
            ) +
            ggplot2$geom_point(shape = pt_shape, fill = pt_fill, size = pt_size) +
            theme_apa()
        } else {
          data_g <- aggregate(y ~ x + group, data_r$data_d, mean, na.rm = TRUE)
          data_g |>
            ggplot2$ggplot(ggplot2$aes(x = x, y = y, group = group)) +
            ggplot2$geom_line(
              ggplot2$aes(x = x, y = y, color = group),
              data = main_calc$output[[3]]
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
        p_base <- data_r$data_d |>
          ggplot2$ggplot(ggplot2$aes(x = x, y = y, group = id)) +
          ggplot2$geom_line(
            ggplot2$aes(x = x, y = y, group = id),
            data = dplyr$bind_rows(main_calc$output[[3]]),
            alpha = 0.33
          ) +
          theme_apa()
        if (length(unique(data_r$data_d$id)) < 51) {
          p_base +
            ggplot2$geom_point(shape = pt_shape, fill = pt_fill, size = pt_size) +
            ggplot2$facet_wrap(~id)
        } else {
          p_base
        }
      } else {
        if (!groupcol()) {
          data_r$data_d |>
            ggplot2$ggplot(ggplot2$aes(x = x, y = y)) +
            ggplot2$geom_line(
              ggplot2$aes(x = x, y = y),
              data = main_calc$output[[3]][[1]]
            ) +
            ggplot2$geom_point(shape = pt_shape, fill = pt_fill, size = pt_size) +
            theme_apa()
        } else {
          data_r$data_d |>
            ggplot2$ggplot(ggplot2$aes(x = x, y = y, group = group)) +
            ggplot2$geom_line(
              ggplot2$aes(x = x, y = y, color = group),
              data = main_calc$output[[3]]
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

      p <- p +
        ggplot2$xlab(input$xtext) +
        ggplot2$ylab(input$ytext) +
        ggplot2$ggtitle(input$title)

      if (input$xlog) {
        p <- p + ggplot2$scale_x_continuous(transform = "pseudo_log")
      } else {
        p <- p + ggplot2$scale_x_continuous()
      }

      if (input$ylog) {
        p <- p + ggplot2$scale_y_continuous(transform = "pseudo_log")
      } else {
        p <- p + ggplot2$scale_y_continuous()
      }

      if (groupcol()) {
        p <- p +
          ggplot2$guides(color = ggplot2$guide_legend(title = input$legend_title))
        n_groups <- length(unique(data_r$data_d$group))
        p <- p +
          ggplot2$scale_color_manual(
            values = utils$get_palette_colors(input$palette, n_groups)
          )
      }

      if (agg() != "Ind" || length(unique(data_r$data_d$id)) > 51) {
        p <- p + utils$add_shiny_logo(utils$watermark_tr)
      }

      p
    })

    output$group_name <- shiny$renderUI({
      if (groupcol()) {
        shiny$textInput(
          inputId = ns("legend_title"),
          label = "Legend Title",
          value = "group"
        )
      }
    })

    esquisse$render_ggplot(
      id = "plot",
      expr = plot_reactive(),
      filename = "shinybeez-demand"
    )
  })
}
