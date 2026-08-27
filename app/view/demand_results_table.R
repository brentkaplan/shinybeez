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
  app / logic / telemetry_utils,
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
              choices = c("Codedbx", "Okabe-Ito", "HCL Light", "HCL Dark"),
              selected = "Codedbx"
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
  kval,
  calculate_btn,
  fit_task
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
      base_plot = NULL,
      plot = NULL,
      # The group levels the base plot was actually built with (NULL when it has no colour
      # aesthetic). The colour scale is derived from THIS, never from the live upload —
      # see resolve_group_scale() and production error bce0bb1d.
      plot_group_levels = NULL
    )

    # Metadata for the fit currently on the task; NULL when nothing is pending.
    pending_fit <- shiny$reactiveVal(NULL)

    # Bumped once per COMPLETED fit (success or failure). The plot observers key
    # off this rather than off the Calculate button: the fit is now asynchronous,
    # so at click time res$output still holds the previous run's fit. They bind it
    # with ignoreInit = TRUE -- unlike an actionButton's 0, this counter's initial
    # 0L is not treated as a null event, so without it they would fire once at
    # module init, which the old calculate_btn() binding never did.
    fit_generation <- shiny$reactiveVal(0L)

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

      telemetry_utils$track_configuration(
        "demand",
        config = list(
          equation = eq_code, k = k, aggregation = agg_val,
          grouped = is_grouped, constrainq0 = constrainq0
        ),
        session = session
      )
      telemetry_utils$track_model_fitting(
        "demand_fixed",
        parameters = list(equation = eq_code, k = k, aggregation = agg_val),
        status = "started",
        session = session
      )

      if (identical(fit_task$status(), "running")) {
        shiny$showNotification(
          "Demand curve fitting is already running - wait for it or press Cancel.",
          type = "warning",
          duration = 5
        )
        return(NULL)
      }

      spec <- list(
        is_grouped = is_grouped, eq = eq_code, agg = agg_val,
        k = k, constrainq0 = constrainq0
      )
      pending_fit(list(eq_code = eq_code, k = k, agg_val = agg_val))
      fit_task$invoke(spec, data_r$data_d)
    }) |>
      shiny$bindEvent(calculate_btn())

    # Task outcome -> results, notifications, logging, telemetry.
    shiny$observeEvent(fit_task$status(), {
      st <- fit_task$status()
      p <- pending_fit()
      if (is.null(p) || st %in% c("initial", "running")) {
        return()
      }
      on.exit(pending_fit(NULL), add = TRUE)
      on.exit(fit_generation(fit_generation() + 1L), add = TRUE)
      params <- list(equation = p$eq_code, k = p$k, aggregation = p$agg_val)

      if (identical(st, "success")) {
        r <- fit_task$result()
        session_logger$performance(
          "demand_curve_fitting",
          duration_ms = r$duration_ms,
          additional_metrics = list(status = "success"),
          always_log = TRUE
        )
        fit_result <- r$fit
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
          telemetry_utils$track_model_fitting(
            "demand_fixed",
            parameters = c(
              params,
              list(
                failed_groups = paste(fit_result$failed_groups, collapse = ",")
              )
            ),
            status = "partial",
            session = session
          )
        }
        telemetry_utils$track_model_fitting(
          "demand_fixed",
          parameters = params,
          status = "completed",
          session = session
        )
        shiny$showNotification(
          "Model fitting complete. See Model Results tab.",
          type = "message",
          duration = 5
        )
        return()
      }

      res$output <- NULL
      res$results <- NULL
      outcome <- fit_task$outcome()

      if (identical(outcome, "cancelled")) {
        shiny$showNotification(
          "Demand curve fitting cancelled.",
          type = "warning",
          duration = 5
        )
        telemetry_utils$track_model_fitting(
          "demand_fixed",
          parameters = params,
          status = "cancelled",
          session = session
        )
        return()
      }

      if (identical(outcome, "timeout")) {
        shiny$showNotification(
          "Demand curve fitting timed out.",
          type = "error",
          duration = NULL
        )
        telemetry_utils$track_model_fitting(
          "demand_fixed",
          parameters = params,
          status = "timeout",
          session = session
        )
        return()
      }

      msg <- fit_task$error_message()
      session_logger$error_enhanced(
        paste("Error in FitCurves:", msg), simpleError(msg),
        context = "demand_curve_fitting",
        user_action = "demand model calculation"
      )
      telemetry_utils$track_model_fitting(
        "demand_fixed",
        parameters = params,
        status = "failed",
        session = session
      )
      shiny$showNotification(
        paste("Error fitting demand curves:", msg),
        type = "error", duration = NULL
      )
    })

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

    # Drop every plot artefact. Must run before ANY early return from the plot observer:
    # a stale base_plot left behind by a bailed-out run still satisfies the decorator's
    # req(), which then colours it against the *new* data — that is bce0bb1d.
    clear_plot_state <- function() {
      res$base_plot <- NULL
      res$plot <- NULL
      res$plot_group_levels <- NULL
    }

    shiny$observe({
      if (groupcol()) {
        if (!"group" %in% colnames(data_r$data_d)) {
          clear_plot_state()
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
      clear_plot_state()
      pt_shape <- 21
      pt_fill <- "white"
      pt_size <- 3

      # Only create plots if we have valid output from FitCurves
      if (is.null(res$output)) {
        return()
      }

      # Every branch below that maps colour = group records the levels it used; the branches
      # that don't map colour leave this NULL (set by clear_plot_state above), so the
      # decorator adds no scale at all.
      is_coloured_by_group <- groupcol() && !analysis_type %in% "Ind"
      if (is_coloured_by_group) {
        res$plot_group_levels <- unique(data_r$data_d$group)
      }

      if (analysis_type %in% c("Mean")) {
        if (!groupcol()) {
          data_g <- aggregate(y ~ x, data_r$data_d, mean, na.rm = TRUE)
          res$base_plot <- data_g |>
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
          res$base_plot <- data_g |>
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
        res$base_plot <- data_r$data_d |>
          ggplot2$ggplot(ggplot2$aes(x = x, y = y, group = id)) +
          ggplot2$geom_line(
            ggplot2$aes(x = x, y = y, group = id),
            data = dplyr$bind_rows(res$output$predictions),
            alpha = 0.33
          ) +
          theme_apa()
        if (length(unique(data_r$data_d$id)) < 51) {
          res$base_plot <- data_r$data_d |>
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
          res$base_plot <- data_r$data_d |>
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
          res$base_plot <- data_r$data_d |>
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
      shiny$bindEvent(fit_generation(), ignoreInit = TRUE)

    shiny$observe({
      shiny$req(res$base_plot)
      res$plot <- res$base_plot +
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

      # Colour from the levels the base plot was BUILT with, not from the live upload. The
      # two can disagree: this observer also fires on Update Plot and the dark-mode toggle,
      # by which time the user may have uploaded different data without re-running Calculate.
      # resolve_group_scale() returns NULL when there is nothing to colour, and adding NULL
      # to a ggplot is a no-op — so this is safe to add unconditionally.
      if (!is.null(res$plot_group_levels)) {
        res$plot <- res$plot +
          ggplot2$guides(
            color = ggplot2$guide_legend(title = input$legend_title)
          ) +
          utils$resolve_group_scale(res$plot_group_levels, input$palette)
      }

      if (agg() != "Ind" || length(unique(data_r$data_d$id)) > 51) {
        res$plot <- res$plot +
          utils$add_shiny_logo(utils$watermark_tr)
      }

      # Match the plot to the active color mode (re-renders on toggle, below)
      res$plot <- utils$apply_dark_mode_theme(
        res$plot,
        session$rootScope()$input$dark_mode
      )

      esquisse$render_ggplot(
        id = "plot",
        expr = res$plot,
        filename = "shinybeez-demand"
      )
    }) |>
      shiny$bindEvent(
        c(fit_generation(), input$update_plot_btn),
        session$rootScope()$input$dark_mode,
        ignoreInit = TRUE
      )
  })
}
