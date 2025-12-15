#' Mixed Effects Demand - Navpanel Module
#'
#' Contains navpanel_ui and navpanel_server for the mixed effects demand tab.

box::use(
  beezdemand,
  bslib,
  dplyr,
  DT,
  esquisse,
  ggplot2,
  ggprism,
  htmltools,
  nlme,
  openxlsx,
  rlang,
  shiny,
  stats
)

box::use(
  app / logic / utils,
  app / logic / logging_utils,
  app / logic / mixed_effects_demand_utils,
  app / logic / mixed_effects / comparisons,
  app / logic / mixed_effects / emms_utils,
  app / logic / mixed_effects / export_utils,
  app / logic / mixed_effects / model_fitting,
  app / logic / mixed_effects / plotting,
  app / logic / mixed_effects / validation_utils
)

#' @export
navpanel_ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    bslib$navset_card_tab(
      id = ns("data_display_tabs"),
      title = "Data Overview",
      bslib$nav_panel(
        title = "Data",
        DT$DTOutput(ns("input_data_table"))
      ),
      bslib$nav_panel(
        title = "Descriptives",
        DT$DTOutput(ns("descriptives_ll4_table"))
      ),
      bslib$nav_panel(
        title = "Systematic Criteria",
        bslib$card(
          style = "border: none; ",
          bslib$layout_sidebar(
            fillable = TRUE,
            sidebar = bslib$sidebar(
              title = "Systematic Settings",
              open = FALSE,
              shiny$numericInput(
                inputId = ns("deltaq"),
                label = "Criteria #1: Relative change (DeltaQ)",
                value = .025,
                min = 0,
                max = 1,
                step = .005
              ),
              shiny$numericInput(
                inputId = ns("bounce"),
                label = "Criteria #2: Price to price increases (Bounce)",
                value = .10,
                min = 0,
                max = 1,
                step = .01
              ),
              shiny$numericInput(
                inputId = ns("reversals"),
                label = "Criteria #3: Number of reversals (Reversals)",
                value = 0,
                min = 0,
                max = 4,
                step = 1
              ),
              shiny$numericInput(
                inputId = ns("ncons0"),
                label = "Criteria #3a: Number of consecutive 0s used to flag reversals",
                value = 2,
                min = 0,
                max = 4,
                step = 1
              ),
              shiny$uiOutput(ns("systematic_group_ui"))
            ),
            DT$DTOutput(ns("systematic_table"))
          )
        )
      )
    ),
    bslib$navset_card_tab(
      id = ns("results_display_tabs"),
      title = "Mixed Model Results",
      bslib$nav_panel(
        title = "Model Summary",
        shiny$verbatimTextOutput(ns("model_summary_output"))
      ),
      bslib$nav_panel(
        title = "Fixed Effects",
        DT$DTOutput(ns("fixed_effects_table"))
      ),
      bslib$nav_panel(
        title = "Random Effects",
        shiny$div(
          shiny$uiOutput(ns("individual_random_effects_table")),
          shiny$hr(),
          shiny$h3("Random Effects (deviations from fixed effects):"),
          DT$DTOutput(ns("random_effects_table"))
        )
      ),
      bslib$nav_panel(
        title = "EMMs & EV",
        shiny$div(
          shiny$h4("Q0 Estimates"),
          DT$DTOutput(ns("emms_q0_table")),
          shiny$hr(),
          shiny$h4("Alpha Estimates"),
          DT$DTOutput(ns("emms_alpha_table")),
          shiny$hr(),
          shiny$h4("Essential Value (EV)"),
          DT$DTOutput(ns("emms_ev_table"))
        )
      ),
      bslib$nav_panel(
        title = "Pairwise Comparisons",
        shiny$div(
          shiny$uiOutput(ns("comparison_factor_selector_ui")),
          shiny$uiOutput(ns("contrast_by_selector_ui")),
          shiny$selectInput(
            ns("comparison_adjust_method"),
            "P-value Adjustment:",
            choices = c(
              "tukey",
              "scheffe",
              "sidak",
              "bonferroni",
              "dunnettx",
              "mvt",
              "holm",
              "hochberg",
              "hommel",
              "fdr",
              "none"
            ),
            selected = "fdr"
          ),
          shiny$radioButtons(
            ns("comparison_display_type"),
            label = "Display comparisons as:",
            choices = list(
              "Log10 Differences" = "log10",
              "Natural Scale Ratios" = "ratio"
            ),
            selected = "log10",
            inline = TRUE
          ),
        ),
        shiny$div(
          shiny$hr(),
          shiny$uiOutput(ns("comparisons_q0_ui")),
          shiny$hr(),
          shiny$uiOutput(ns("comparisons_alpha_ui"))
        ),
      ),
      bslib$nav_panel(
        title = "Plot",
        bslib$card(
          style = "border: none;",
          bslib$layout_sidebar(
            fillable = TRUE,
            sidebar = bslib$sidebar(
              title = "Plot Settings",
              open = FALSE,
              shiny$textInput(
                ns("plot_title"),
                "Plot Title",
                "Mixed Model Demand Plot"
              ),
              shiny$textInput(
                ns("plot_xlab"),
                "X-axis Label",
                "Price (Fixed Ratio)"
              ),
              shiny$textInput(
                ns("plot_ylab"),
                "Y-axis Label",
                "Consumption (Natural Scale)"
              ),
              shiny$selectInput(
                ns("plot_theme"),
                "Plot Theme",
                choices = c(
                  "Default (ggplot2)" = "default",
                  "GraphPad Prism" = "prism",
                  "Classic" = "classic",
                  "Minimal" = "minimal"
                ),
                selected = "default"
              ),
              shiny$numericInput(
                ns("plot_font_size"),
                "Base Font Size",
                value = 14,
                min = 10,
                max = 20,
                step = 1
              ),
              shiny$selectInput(
                ns("plot_legend_position"),
                "Legend Position",
                choices = c(
                  "Right" = "right",
                  "Bottom" = "bottom",
                  "None" = "none"
                ),
                selected = "right"
              ),
              shiny$selectInput(
                ns("plot_palette"),
                "Color Palette",
                choices = c("Okabe-Ito", "HCL Light", "HCL Dark"),
                selected = "Okabe-Ito"
              ),
              shiny$selectInput(
                ns("plot_color_by"),
                "Color lines/points by:",
                choices = NULL
              ),
              shiny$selectInput(
                ns("plot_linetype_by"),
                "Linetype by:",
                choices = NULL
              ),
              shiny$selectInput(
                ns("plot_facet_by"),
                "Facet by:",
                choices = NULL
              ),
              shiny$checkboxInput(
                ns("plot_x_trans_log"),
                "Log X-Axis",
                value = TRUE
              ),
              shiny$checkboxInput(
                ns("plot_y_trans_log"),
                "Pseudo-Log Y-Axis (Natural Scale)",
                value = TRUE
              ),
              shiny$checkboxInput(
                ns("show_population_lines"),
                "Show Population Lines",
                value = TRUE
              ),
              shiny$checkboxInput(
                ns("show_individual_lines"),
                "Show Individual Lines",
                value = FALSE
              ),
              shiny$checkboxInput(
                ns("show_observed_points_plot"),
                "Show Observed Points",
                value = TRUE
              ),
              shiny$checkboxInput(
                ns("show_watermark"),
                "Show shinybeez Watermark",
                value = TRUE
              ),
              shiny$actionButton(ns("update_plot_settings"), "Update Plot")
            ),
            esquisse$ggplot_output(
              ns("mixed_model_plot"),
              downloads = esquisse$downloads_labels(
                label = esquisse$ph("download-simple"),
                png = htmltools$tagList(esquisse$ph("image"), "PNG"),
                svg = htmltools$tagList(esquisse$ph("browsers"), "SVG"),
                jpeg = htmltools$tagList(esquisse$ph("image"), "JPEG"),
                pptx = NULL,
                more = htmltools$tagList(
                  esquisse$ph("gear"),
                  esquisse$i18n("More options")
                )
              )
            )
          )
        )
      ),
      bslib$nav_item(
        shiny$downloadButton(
          ns("export_all_xlsx"),
          "Export All",
          icon = shiny$icon("download"),
          class = "btn-link nav-link px-3"
        )
      )
    )
  )
}

#' @export
navpanel_server <- function(id, sidebar_reactives) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create session-specific logger for navpanel
    session_logger <- logging_utils$create_session_logger(session)

    # Helper: build transformed covariate column using extracted module
    # Uses covariate controls from sidebar_reactives
    build_covariate_modeling_info <- function(df_in) {
      covar <- sidebar_reactives$covariate()
      if (is.null(covar) || !nzchar(covar) || !(covar %in% names(df_in))) {
        return(list(df = df_in, model_covariate_name = NULL, at_list = NULL))
      }

      result <- model_fitting$process_covariate(
        df = df_in,
        covariate_col = covar,
        center = isTRUE(sidebar_reactives$cov_center()),
        scale = isTRUE(sidebar_reactives$cov_scale()),
        at_value = sidebar_reactives$cov_at_natural()
      )

      # Show notification if scale failed
      if (isTRUE(result$transform_info$scale_failed)) {
        shiny$showNotification(
          "Scale requested but SD is not positive; applying centering only.",
          type = "warning"
        )
      }

      result
    }

    data_to_analyze <- shiny$reactive({
      df <- sidebar_reactives$data_to_analyze_trigger()
      shiny$req(df)

      # Decide y_for_model once, based on the equation choice + user's Y selection
      y_var_col_name <- sidebar_reactives$y_var()
      equation_choice <- sidebar_reactives$equation_form()
      shiny$req(y_var_col_name, equation_choice)

      if (identical(equation_choice, "zben")) {
        # Prefer existing y_ll4; otherwise LL4-transform the selected Y
        if ("y_ll4" %in% names(df)) {
          df[["y_for_model"]] <- df[["y_ll4"]]
        } else {
          shiny$req(y_var_col_name %in% names(df))
          df[["y_for_model"]] <- beezdemand$ll4(df[[y_var_col_name]])
        }
        attr(df, "y_is_ll4") <- TRUE
      } else {
        # simplified: use the user's column as-is
        shiny$req(y_var_col_name %in% names(df))
        df[["y_for_model"]] <- df[[y_var_col_name]]
        attr(df, "y_is_ll4") <- FALSE
      }

      # Convert any nonfactors selected to factors
      factors_to_convert <- setdiff(
        sidebar_reactives$selected_factors(),
        "None"
      )

      # Only proceed if there are factors to convert and they exist in the data
      if (
        length(factors_to_convert) > 0 && all(factors_to_convert %in% names(df))
      ) {
        # find which are actually nonfactors (need to be converted)
        is_factor_status <- sapply(df[factors_to_convert], is.factor)
        nonfactor_columns <- names(is_factor_status)[!is_factor_status]

        # convert any nonfactors to factors
        if (length(nonfactor_columns) > 0) {
          df <- df |>
            dplyr$mutate(dplyr$across(
              dplyr$all_of(nonfactor_columns),
              as.factor
            ))
        }
      }
      # Add transformed covariate column if selected
      cov_info <- build_covariate_modeling_info(df)
      df <- cov_info$df

      return(df)
    }) |>
      shiny$bindCache(
        sidebar_reactives$data_to_analyze_trigger(),
        sidebar_reactives$equation_form(),
        sidebar_reactives$y_var(),
        sidebar_reactives$selected_factors(),
        sidebar_reactives$covariate(),
        sidebar_reactives$cov_center(),
        sidebar_reactives$cov_scale()
      )

    # Informational notification when equation or Y selection changes
    shiny$observeEvent(
      list(
        sidebar_reactives$equation_form(),
        sidebar_reactives$y_var()
      ),
      {
        eq <- sidebar_reactives$equation_form()
        yname <- sidebar_reactives$y_var()
        df_now <- sidebar_reactives$data_to_analyze_trigger()
        shiny$req(eq, yname, df_now)
        if (identical(eq, "zben")) {
          if ("y_ll4" %in% names(df_now)) {
            # Using existing y_ll4; no transform needed
            shiny$showNotification(
              "ZBEn: using existing y_ll4 column.",
              type = "message",
              duration = 3
            )
          } else {
            shiny$showNotification(
              "ZBEn: applying LL4 to selected Y.",
              type = "message",
              duration = 3
            )
          }
        } else {
          shiny$showNotification(
            paste("Simplified: using selected Y column '", yname, "'."),
            type = "message",
            duration = 3
          )
        }
      },
      ignoreInit = TRUE
    )

    output$input_data_table <- DT$renderDT({
      shiny$req(data_to_analyze())
      DT$datatable(
        data_to_analyze(),
        rownames = FALSE,
        extensions = c("Buttons", "Scroller"),
        options = list(
          scrollX = TRUE,
          pageLength = 5,
          autoWidth = TRUE,
          dom = "Bti",
          buttons = export_utils$build_dt_buttons(
            "shinybeez_MixedEffects_Input_Data"
          ),
          deferRender = TRUE,
          scroller = TRUE
        ),
        filter = "top",
        class = "compact hover"
      )
    })

    # Descriptive stats for y_ll4
    output$descriptives_ll4_table <- DT$renderDT({
      df_processed <- data_to_analyze()
      shiny$req(df_processed, "y_for_model" %in% names(df_processed))

      factors <- sidebar_reactives$selected_factors()
      x_var_sel <- sidebar_reactives$x_var()
      shiny$req(x_var_sel)

      grouping_vars <- c(factors, x_var_sel)
      grouping_vars_present <- intersect(grouping_vars, names(df_processed))

      if (length(grouping_vars_present) == 0) {
        desc_data <- df_processed |>
          dplyr$summarise(
            N = dplyr$n(),
            Mean_Y_Model = mean(y_for_model, na.rm = TRUE),
            SD_Y_Model = stats$sd(y_for_model, na.rm = TRUE),
            Median_Y_Model = stats$median(y_for_model, na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        desc_data <- df_processed |>
          dplyr$group_by(dplyr$across(dplyr$all_of(
            grouping_vars_present
          ))) |>
          dplyr$summarise(
            N = dplyr$n(),
            Mean_Y_Model = mean(y_for_model, na.rm = TRUE),
            SD_Y_Model = stats$sd(y_for_model, na.rm = TRUE),
            Median_Y_Model = stats$median(y_for_model, na.rm = TRUE),
            .groups = "drop"
          )
      }
      DT$datatable(
        dplyr$mutate(
          desc_data,
          dplyr$across(where(is.numeric), ~ round(., 3))
        ),
        caption = paste(
          "Descriptive Statistics for Y variable used in model (y_for_model)"
        ),
        rownames = FALSE,
        extensions = c("Buttons"),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          autoWidth = TRUE,
          dom = "Bti",
          buttons = export_utils$build_dt_buttons(
            "shinybeez_MixedEffects_Descriptives"
          )
        ),
        filter = "top",
        class = "compact hover"
      )
    })

    # Systematic Criteria: optional grouping selector (multi-select)
    output$systematic_group_ui <- shiny$renderUI({
      facs <- sidebar_reactives$selected_factors()
      if (is.null(facs) || length(facs) == 0) {
        return(NULL)
      }
      shiny$selectizeInput(
        ns("systematic_group_by"),
        "Group results by (optional):",
        choices = stats$setNames(facs, facs),
        selected = facs, # default to all selected factors; empty selection means no grouping
        multiple = TRUE,
        options = list(
          placeholder = "Select factors to group by (leave empty for none)"
        )
      )
    })

    # Systematic Criteria table
    shiny$observe({
      # Add dependency on selected factors so changes there retrigger this calculation
      facs_dep <- sidebar_reactives$selected_factors()
      df_raw <- sidebar_reactives$data_to_analyze_trigger()
      shiny$req(df_raw)
      id_col <- sidebar_reactives$id_var()
      x_col <- sidebar_reactives$x_var()
      y_col <- sidebar_reactives$y_var()
      shiny$req(id_col, x_col, y_col)

      # Guard against race: ensure selected columns exist in current data
      if (!all(c(id_col, x_col, y_col) %in% names(df_raw))) {
        # Wait for select inputs to update to the new dataset; skip this cycle
        return(invisible(NULL))
      }

      # Friendly note if using transformed Y
      if (identical(y_col, "y_ll4")) {
        shiny$showNotification(
          "Systematic Criteria using y_ll4 (transformed). Consider selecting raw y for conventional checks.",
          type = "message",
          duration = 5
        )
      }

      group_vars <- input$systematic_group_by
      systematic <- NULL

      # Compute systematic criteria, optionally by multiple grouping factors
      if (!is.null(group_vars) && length(group_vars) > 0) {
        # Validate selected group vars exist
        missing_groups <- setdiff(group_vars, names(df_raw))
        if (length(missing_groups) > 0) {
          shiny$showNotification(
            paste0(
              "Selected group-by variable(s) not found: ",
              paste(missing_groups, collapse = ", ")
            ),
            type = "error",
            duration = 10
          )
          return()
        }
        df_sys <- df_raw[, c(id_col, x_col, y_col, group_vars), drop = FALSE]
        names(df_sys)[1:3] <- c("id", "x", "y")
        suppressWarnings({
          df_sys$x <- as.numeric(df_sys$x)
          df_sys$y <- as.numeric(df_sys$y)
        })
        df_sys <- df_sys[!is.na(df_sys$y), , drop = FALSE]
        systematic <- df_sys |>
          dplyr$group_by(dplyr$across(dplyr$all_of(group_vars))) |>
          dplyr$group_modify(
            ~ beezdemand$CheckUnsystematic(
              dat = .x[, c("id", "x", "y")],
              deltaq = input$deltaq,
              bounce = input$bounce,
              reversals = input$reversals,
              ncons0 = input$ncons0
            )
          )
      } else {
        df_sys <- mixed_effects_demand_utils$prepare_systematic_input(
          df = df_raw,
          id_col = id_col,
          x_col = x_col,
          y_col = y_col
        )
        systematic <- beezdemand$CheckUnsystematic(
          dat = df_sys,
          deltaq = input$deltaq,
          bounce = input$bounce,
          reversals = input$reversals,
          ncons0 = input$ncons0
        )
      }

      output$systematic_table <- DT$renderDT(server = FALSE, {
        DT$datatable(
          systematic,
          rownames = FALSE,
          extensions = c("Buttons", "Scroller", "FixedColumns"),
          fillContainer = FALSE,
          options = list(
            autoWidth = FALSE,
            ordering = TRUE,
            dom = "Bti",
            buttons = export_utils$build_dt_buttons(
              "shinybeez_MixedEffects_Systematic_Criteria"
            ),
            deferRender = TRUE,
            scrollY = 250,
            scroller = TRUE,
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 1)
          )
        )
      })
    })

    # Reactive for the fitted model
    fitted_model_reactive <- shiny$eventReactive(
      sidebar_reactives$run_trigger(),
      {
        df <- data_to_analyze()
        shiny$req(df)

        # Get the user's selected parameters for random effects (e.g., c("alpha", "q0"))
        rand_eff_spec <- sidebar_reactives$random_effects_spec()

        # Map the checkbox values to the formal parameter names used in the model
        random_params_for_formula <- character(0)
        if ("q0" %in% rand_eff_spec) {
          random_params_for_formula <- c(random_params_for_formula, "Q0")
        }
        if ("alpha" %in% rand_eff_spec) {
          random_params_for_formula <- c(random_params_for_formula, "alpha")
        }

        if (length(random_params_for_formula) == 0) {
          shiny$showNotification(
            "No random effects selected for model parameters.
            \n\nTry using the demand tab at the top of the page.",
            type = "error"
          )
          return(NULL) # Stop if nothing is selected
        }

        # Construct the simple formula string (e.g., "Q0 + alpha")
        random_formula_str <- paste(random_params_for_formula, collapse = " + ")

        # Create the final formula object required by fit_demand_mixed (e.g., Q0 + alpha ~ 1)
        random_effects_formula_to_pass <- stats$as.formula(paste0(
          random_formula_str,
          " ~ 1"
        ))

        # Read the y-scale flag prepared in data_to_analyze()
        y_is_ll4 <- isTRUE(attr(df, "y_is_ll4"))

        # Ensure the authoritative column exists
        shiny$validate(
          shiny$need(
            "y_for_model" %in% names(df),
            "Internal error: 'y_for_model' not found."
          )
        )

        sel_factors <- setdiff(
          sidebar_reactives$selected_factors(),
          "None"
        )
        if (length(sel_factors) == 0) {
          sel_factors <- NULL
        }

        # Build nlmeControl from user-selected advanced fitting controls
        user_ctrl_vals <- sidebar_reactives$nlme_controls()
        user_nlme_control <- nlme$nlmeControl(
          maxIter = user_ctrl_vals$maxIter,
          pnlsMaxIter = user_ctrl_vals$pnlsMaxIter,
          msMaxIter = user_ctrl_vals$msMaxIter,
          tolerance = user_ctrl_vals$tolerance,
          pnlsTol = user_ctrl_vals$pnlsTol,
          minScale = user_ctrl_vals$minScale,
          niterEM = user_ctrl_vals$niterEM
        )

        current_collapse_levels <- sidebar_reactives$collapse_levels_reactive()

        # Check for invalid collapse definitions
        if (
          identical(current_collapse_levels, "ERROR_OVERLAP") ||
            identical(current_collapse_levels, "ERROR_SINGLE_LEVEL")
        ) {
          shiny$showNotification(
            "Cannot fit model due to an invalid collapse definition (overlap or single resulting level).",
            type = "error",
            duration = 7
          )
          return(NULL)
        }

        notif_id <- shiny$showNotification(
          "Fitting mixed-effects model... This may take a moment.",
          type = "message",
          duration = NULL
        )

        # Prepare covariate info for modeling (ensures transformed column exists)
        cov_info <- build_covariate_modeling_info(df)
        cont_covars_to_pass <- cov_info$model_covariate_name

        # Track model fitting start time for performance logging
        model_start_time <- Sys.time()

        model_fit <- tryCatch(
          {
            beezdemand$fit_demand_mixed(
              data = df,
              # y_var = y_var_actual,
              y_var = "y_for_model",
              x_var = sidebar_reactives$x_var(), # Assuming 'x' is the price/ratio column from ko
              id_var = sidebar_reactives$id_var(), # Assuming 'monkey' is the ID from ko
              factors = sel_factors,
              factor_interaction = sidebar_reactives$factor_interaction(),
              equation_form = sidebar_reactives$equation_form(),
              collapse_levels = current_collapse_levels,
              random_effects = random_effects_formula_to_pass,
              covariance_structure = sidebar_reactives$covariance_structure(),
              nlme_control = user_nlme_control,
              start_value_method = "pooled_nls", # Often more robust for complex models
              continuous_covariates = cont_covars_to_pass
            )
          },
          error = function(e) {
            shiny$removeNotification(notif_id)
            shiny$showNotification(
              paste("Model fitting error:", e$message),
              type = "error",
              duration = NULL
            )
            # Log model fitting failure
            session_logger$model_fitting(
              model_type = "mixed_effects_demand",
              parameters = list(
                equation = sidebar_reactives$equation_form(),
                factors = sel_factors
              ),
              status = "failed",
              metrics = list(error = e$message)
            )
            NULL
          }
        )

        if (is.null(model_fit) || is.null(model_fit$model)) {
          shiny$removeNotification(notif_id)
          shiny$showNotification(
            "Model fitting failed or did not converge.",
            type = "error"
          )
          # Log convergence failure
          session_logger$model_fitting(
            model_type = "mixed_effects_demand",
            parameters = list(
              equation = sidebar_reactives$equation_form(),
              factors = sel_factors
            ),
            status = "failed",
            metrics = list(error = "Model did not converge")
          )
          return(NULL)
        }

        # Persist the scale info for plotting
        model_fit$param_info$y_is_ll4 <- y_is_ll4

        # Calculate fitting duration and log success
        model_duration_ms <- as.numeric(difftime(
          Sys.time(),
          model_start_time,
          units = "secs"
        )) *
          1000
        session_logger$model_fitting(
          model_type = "mixed_effects_demand",
          parameters = list(
            equation = sidebar_reactives$equation_form(),
            factors = sel_factors,
            n_observations = nrow(df)
          ),
          status = "completed",
          metrics = list(fitting_time_ms = round(model_duration_ms))
        )

        # Log performance if it took a while
        session_logger$performance(
          operation_name = "mixed_effects_model_fit",
          duration_ms = round(model_duration_ms),
          additional_metrics = list(
            n_observations = nrow(df),
            n_factors = length(sel_factors)
          )
        )

        shiny$removeNotification(notif_id)
        shiny$showNotification("Model fitting complete.", type = "message")
        return(model_fit)
      }
    )

    output$model_summary_output <- shiny$renderPrint({
      model_fit <- fitted_model_reactive()
      shiny$req(model_fit, model_fit$model)
      print(model_fit)
    })

    output$fixed_effects_table <- DT$renderDT({
      model_fit <- fitted_model_reactive()
      shiny$req(model_fit, model_fit$model)
      fe <- nlme$fixef(model_fit)
      DT$datatable(
        data.frame(Parameter = names(fe), Value = round(fe, 4)),
        rownames = FALSE,
        extensions = c("Buttons"),
        options = list(
          dom = "Btip",
          buttons = export_utils$build_dt_buttons(
            "shinybeez_MixedEffects_Fixed_Effects"
          )
        )
      )
    })

    output$individual_random_effects_table <- shiny$renderUI({
      model_fit <- fitted_model_reactive()
      shiny$req(model_fit, model_fit$model)

      # Check if there are any fixed effects selected
      if (is.null(model_fit$param_info$factors)) {
        return(NULL) # Return NULL if no fixed effects are selected
      }

      individual_coefs_wide <- tryCatch(
        beezdemand$get_individual_coefficients(
          model_fit,
          params = c("Q0", "alpha"),
          format = "wide"
        ),
        error = function(e) {
          shiny$showNotification(
            paste("Error extracting individual coefficients:", e$message),
            type = "error"
          )
          NULL
        }
      )
      individual_coefs_wide[, -1] <- round(individual_coefs_wide[, -1], 4)

      shiny$tagList(
        shiny$h3("Individual Random Effects"),
        DT$datatable(
          individual_coefs_wide,
          rownames = FALSE,
          extensions = c("Buttons"),
          options = list(
            dom = "Btip",
            buttons = export_utils$build_dt_buttons(
              "shinybeez_MixedEffects_Individual_Coefficients"
            )
          )
        )
      )
    })

    output$random_effects_table <- DT$renderDT({
      model_fit <- fitted_model_reactive()
      shiny$req(model_fit, model_fit$model)

      re_coefs <- tryCatch(
        stats$coef(model_fit),
        error = function(e) {
          shiny$showNotification(
            paste("Error extracting coefficients:", e$message),
            type = "error"
          )
          NULL
        }
      )
      shiny$req(re_coefs)
      re_df <- as.data.frame(re_coefs)

      id_column_name <- model_fit$param_info$id_var
      shiny$req(id_column_name) # Ensure id_var is available

      re_df[[id_column_name]] <- rownames(re_df)

      re_df <- re_df |>
        dplyr$select(
          !!rlang$sym(id_column_name),
          dplyr$everything()
        ) |>
        dplyr$mutate(dplyr$across(where(is.numeric), ~ round(., 4)))

      DT$datatable(
        re_df, # Removed the direct round() here as it's done in dplyr
        rownames = FALSE,
        extensions = c("Buttons"),
        options = list(
          scrollX = TRUE,
          pageLength = 5,
          autoWidth = TRUE,
          dom = "Btip",
          buttons = export_utils$build_dt_buttons(
            "shinybeez_MixedEffects_Random_Effects"
          )
        ),
        class = "compact hover", # Added class for styling
        filter = "top" # Added column filters
      )
    })

    # EMMs and EV - Shared reactive for EMM data
    emms_data_reactive <- shiny$reactive({
      model_fit <- fitted_model_reactive()
      shiny$req(model_fit, model_fit$model)

      # Use the model's actual factors (handles asymmetric collapse where
      # Q0 and alpha may have different factor structures)
      model_factors <- model_fit$param_info$factors
      if (is.null(model_factors) || length(model_factors) == 0) {
        model_factors <- NULL
      }

      # Build 'at' for covariate conditioning
      df_now <- data_to_analyze()
      cov_info <- build_covariate_modeling_info(df_now)
      emms_data <- tryCatch(
        beezdemand$get_observed_demand_param_emms(
          fit_obj = model_fit,
          factors_in_emm = model_factors,
          at = cov_info$at_list,
          include_ev = TRUE,
          ci_level = 0.95
        ),
        error = function(e) {
          shiny$showNotification(
            paste("Error getting EMMs:", e$message),
            type = "error"
          )
          NULL
        }
      )
      emms_data
    })

    # Q0 Estimates Table
    output$emms_q0_table <- DT$renderDT({
      emms_data <- emms_data_reactive()
      shiny$req(emms_data)

      if (nrow(emms_data) == 0) {
        return(DT$datatable(
          data.frame(Message = "No Q0 EMMs available"),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }

      # Select Q0-related columns: factor columns + Q0 columns
      # Get factor columns (non-numeric, excluding alpha-specific factor columns)
      factor_cols <- names(emms_data)[!sapply(emms_data, is.numeric)]
      # For Q0 table, use original factor name (not _alpha suffix)
      q0_factor_cols <- factor_cols[!grepl("_alpha$", factor_cols)]

      q0_cols <- names(emms_data)[grepl("Q0", names(emms_data))]
      q0_table_cols <- c(q0_factor_cols, q0_cols)
      q0_table_cols <- intersect(q0_table_cols, names(emms_data))

      q0_data <- emms_data[, q0_table_cols, drop = FALSE]
      # Remove duplicate rows (since alpha levels create duplicates for Q0)
      q0_data <- dplyr$distinct(q0_data)

      DT$datatable(
        dplyr$mutate(q0_data, dplyr$across(where(is.numeric), ~ round(., 4))),
        rownames = FALSE,
        extensions = c("Buttons"),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = "Btip",
          buttons = export_utils$build_dt_buttons(
            "shinybeez_MixedEffects_Q0_EMMs"
          )
        )
      )
    })

    # Alpha Estimates Table
    output$emms_alpha_table <- DT$renderDT({
      emms_data <- emms_data_reactive()
      shiny$req(emms_data)

      if (nrow(emms_data) == 0) {
        return(DT$datatable(
          data.frame(Message = "No Alpha EMMs available"),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }

      # Select alpha-related columns: factor columns + alpha columns
      factor_cols <- names(emms_data)[!sapply(emms_data, is.numeric)]

      # For alpha table, we need:
      # 1. Columns ending with _alpha (collapsed factors like dose_alpha)
      # 2. Original factor columns that weren't collapsed (like drug)
      alpha_suffix_cols <- factor_cols[grepl("_alpha$", factor_cols)]

      if (length(alpha_suffix_cols) == 0) {
        # No differential collapse - use all original factor columns
        alpha_factor_cols <- factor_cols
      } else {
        # Get original factor names that were collapsed (e.g., "dose" from "dose_alpha")
        collapsed_original_names <- sub("_alpha$", "", alpha_suffix_cols)
        # Get uncollapsed factor columns (not ending in _alpha AND not the original of a collapsed factor)
        uncollapsed_factor_cols <- factor_cols[
          !factor_cols %in% collapsed_original_names &
            !grepl("_alpha$", factor_cols)
        ]
        # Combine: collapsed + uncollapsed
        alpha_factor_cols <- c(alpha_suffix_cols, uncollapsed_factor_cols)
      }

      alpha_cols <- names(emms_data)[
        grepl("alpha", names(emms_data)) & !grepl("_alpha$", names(emms_data))
      ]
      alpha_table_cols <- c(alpha_factor_cols, alpha_cols)
      alpha_table_cols <- intersect(alpha_table_cols, names(emms_data))

      alpha_data <- emms_data[, alpha_table_cols, drop = FALSE]
      # Remove duplicate rows (since Q0 levels create duplicates for alpha)
      alpha_data <- dplyr$distinct(alpha_data)

      DT$datatable(
        dplyr$mutate(
          alpha_data,
          dplyr$across(
            where(is.numeric) & !contains("alpha_natural"),
            ~ round(., 4)
          ),
          dplyr$across(
            where(is.numeric) & contains("alpha_natural"),
            ~ round(., 8)
          )
        ),
        rownames = FALSE,
        extensions = c("Buttons"),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = "Btip",
          buttons = export_utils$build_dt_buttons(
            "shinybeez_MixedEffects_Alpha_EMMs"
          )
        )
      )
    })

    # Essential Value (EV) Table
    output$emms_ev_table <- DT$renderDT({
      emms_data <- emms_data_reactive()
      shiny$req(emms_data)

      if (
        nrow(emms_data) == 0 || !any(grepl("^EV$|^EV_|_EV$", names(emms_data)))
      ) {
        return(DT$datatable(
          data.frame(Message = "No EV estimates available"),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }

      # Select EV-related columns: factor columns + EV columns
      factor_cols <- names(emms_data)[!sapply(emms_data, is.numeric)]

      # For EV (derived from alpha), we need:
      # 1. Columns ending with _alpha (collapsed factors like dose_alpha)
      # 2. Original factor columns that weren't collapsed (like drug)
      alpha_suffix_cols <- factor_cols[grepl("_alpha$", factor_cols)]

      if (length(alpha_suffix_cols) == 0) {
        # No differential collapse - use all original factor columns
        ev_factor_cols <- factor_cols
      } else {
        # Get original factor names that were collapsed (e.g., "dose" from "dose_alpha")
        collapsed_original_names <- sub("_alpha$", "", alpha_suffix_cols)
        # Get uncollapsed factor columns (not ending in _alpha AND not the original of a collapsed factor)
        uncollapsed_factor_cols <- factor_cols[
          !factor_cols %in% collapsed_original_names &
            !grepl("_alpha$", factor_cols)
        ]
        # Combine: collapsed + uncollapsed
        ev_factor_cols <- c(alpha_suffix_cols, uncollapsed_factor_cols)
      }

      ev_cols <- names(emms_data)[grepl("EV", names(emms_data))]
      ev_table_cols <- c(ev_factor_cols, ev_cols)
      ev_table_cols <- intersect(ev_table_cols, names(emms_data))

      ev_data <- emms_data[, ev_table_cols, drop = FALSE]
      # Remove duplicate rows
      ev_data <- dplyr$distinct(ev_data)

      DT$datatable(
        dplyr$mutate(ev_data, dplyr$across(where(is.numeric), ~ round(., 4))),
        rownames = FALSE,
        extensions = c("Buttons"),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = "Btip",
          buttons = export_utils$build_dt_buttons("shinybeez_MixedEffects_EV")
        )
      )
    })

    # Populate factor selectors for comparisons
    # Use the model's actual factors (handles asymmetric collapse)
    shiny$observe({
      model_fit <- fitted_model_reactive()
      shiny$req(model_fit, model_fit$model)

      # Get factors from the fitted model - source of truth for comparisons
      model_factors <- model_fit$param_info$factors %||% character(0)

      output$comparison_factor_selector_ui <- shiny$renderUI({
        if (!comparisons$can_compare(model_factors)) {
          return(shiny$helpText(
            "No factors available for comparison (intercept-only model)."
          ))
        }
        shiny$selectInput(
          ns("comparison_factor"),
          "Compare levels of:",
          choices = model_factors,
          selected = model_factors[1]
        )
      })

      output$contrast_by_selector_ui <- shiny$renderUI({
        main_comparison_factor <- input$comparison_factor
        shiny$req(main_comparison_factor)

        other_factors <- comparisons$get_other_factors(
          model_factors,
          main_comparison_factor
        )
        if (length(other_factors) == 0) {
          return(NULL)
        }

        shiny$selectInput(
          ns("contrast_by_factor"),
          "...within levels of (optional):",
          choices = c("None" = "", other_factors),
          selected = ""
        )
      })
    })

    # Pairwise Comparisons
    comparisons_reactive <- shiny$reactive({
      model_fit <- fitted_model_reactive()
      shiny$req(model_fit, model_fit$model)

      # Check if model has any factors to compare
      model_factors <- model_fit$param_info$factors
      if (!comparisons$can_compare(model_factors)) {
        return(NULL)
      }

      # Require a valid comparison factor selection
      main_factor <- input$comparison_factor
      if (!comparisons$is_valid_comparison_factor(main_factor, model_factors)) {
        return(NULL)
      }

      by_factor <- input$contrast_by_factor

      # Build specs string and contrast_by arg using module helpers
      specs_str <- comparisons$build_specs_string(
        main_factor,
        by_factor,
        model_factors
      )
      contrast_by_arg <- comparisons$get_contrast_by_arg(
        by_factor,
        model_factors
      )

      # Build 'at' for covariate conditioning
      df_now <- data_to_analyze()
      cov_info <- build_covariate_modeling_info(df_now)

      comps <- tryCatch(
        beezdemand$get_demand_comparisons(
          fit_obj = model_fit,
          params_to_compare = c("Q0", "alpha"),
          compare_specs = comparisons$build_specs_formula(specs_str),
          contrast_by = contrast_by_arg,
          at = cov_info$at_list,
          adjust = input$comparison_adjust_method,
          report_ratios = TRUE
        ),
        error = function(e) {
          shiny$showNotification(
            paste("Error in comparisons:", e$message),
            type = "error"
          )
          NULL
        }
      )
      comps
    }) |>
      shiny$bindCache(
        sidebar_reactives$run_trigger(),
        input$comparison_factor,
        input$contrast_by_factor,
        input$comparison_adjust_method,
        input$comparison_display_type,
        sidebar_reactives$covariate(),
        sidebar_reactives$cov_center(),
        sidebar_reactives$cov_scale(),
        sidebar_reactives$cov_at_natural()
      )

    output$comparisons_q0_table <- DT$renderDT({
      comps <- comparisons_reactive()
      shiny$req(comps, comps$Q0)

      # Get the appropriate data based on display type
      raw_data <- comparisons$get_comparison_data(
        comps$Q0,
        input$comparison_display_type
      )

      # Handle empty or NULL data
      if (comparisons$is_empty_comparison(raw_data)) {
        return(NULL)
      }

      display_data <- comparisons$round_comparison_data(raw_data)
      caption_text <- comparisons$build_comparison_caption(
        "Q0",
        input$comparison_display_type
      )

      DT$datatable(
        display_data,
        caption = htmltools$tags$caption(
          style = "caption-side: top; text-align: center;",
          caption_text
        ),
        rownames = FALSE,
        extensions = c("Buttons"),
        options = list(
          scrollX = TRUE,
          pageLength = 5,
          autoWidth = TRUE,
          dom = "Btip",
          buttons = export_utils$build_dt_buttons(
            "shinybeez_MixedEffects_Q0_Comparisons"
          )
        ),
        class = "compact hover",
        filter = "top"
      )
    })

    # Conditional UI for Q0 Comparisons
    output$comparisons_q0_ui <- shiny$renderUI({
      comps <- comparisons_reactive()
      if (is.null(comps) || is.null(comps$Q0)) {
        return(NULL)
      }

      display_data <- comparisons$get_comparison_data(
        comps$Q0,
        input$comparison_display_type
      )

      # Handle empty comparisons (intercept-only for Q0 due to collapse)
      if (comparisons$is_empty_comparison(display_data)) {
        return(shiny$tagList(
          shiny$h4("Q0 Comparisons"),
          shiny$helpText(shiny$em(comparisons$build_empty_comparison_message(
            "Q0"
          )))
        ))
      }

      shiny$tagList(
        shiny$h4("Q0 Comparisons"),
        DT$DTOutput(ns("comparisons_q0_table"))
      )
    })

    # Conditional UI for Alpha Comparisons
    output$comparisons_alpha_ui <- shiny$renderUI({
      comps <- comparisons_reactive()
      if (is.null(comps) || is.null(comps$alpha)) {
        return(NULL)
      }

      display_data <- comparisons$get_comparison_data(
        comps$alpha,
        input$comparison_display_type
      )

      # Handle empty comparisons (intercept-only for alpha due to collapse)
      if (comparisons$is_empty_comparison(display_data)) {
        return(shiny$tagList(
          shiny$h4("Alpha Comparisons"),
          shiny$helpText(shiny$em(comparisons$build_empty_comparison_message(
            "alpha"
          )))
        ))
      }

      shiny$tagList(
        shiny$h4("Alpha Comparisons"),
        DT$DTOutput(ns("comparisons_alpha_table"))
      )
    })

    output$comparisons_alpha_table <- DT$renderDT({
      comps <- comparisons_reactive()
      shiny$req(comps, comps$alpha)

      # Get the appropriate data based on display type
      raw_data <- comparisons$get_comparison_data(
        comps$alpha,
        input$comparison_display_type
      )

      # Handle empty or NULL data
      if (comparisons$is_empty_comparison(raw_data)) {
        return(NULL)
      }

      display_data <- comparisons$round_comparison_data(raw_data)
      caption_text <- comparisons$build_comparison_caption(
        "alpha",
        input$comparison_display_type
      )

      DT$datatable(
        display_data,
        caption = htmltools$tags$caption(
          style = "caption-side: top; text-align: center;",
          caption_text
        ),
        rownames = FALSE,
        extensions = c("Buttons"),
        options = list(
          scrollX = TRUE,
          pageLength = 5,
          autoWidth = TRUE,
          dom = "Btip",
          buttons = export_utils$build_dt_buttons(
            "shinybeez_MixedEffects_Alpha_Comparisons"
          )
        ),
        class = "compact hover",
        filter = "top"
      )
    })

    # Plotting Logic
    # Populate plot aesthetic choices
    shiny$observe({
      model_fit <- fitted_model_reactive()
      shiny$req(model_fit, model_fit$model)
      # Use the factors from the fitted model as the source of truth
      factors_in_model <- model_fit$param_info$factors
      if (is.null(factors_in_model)) {
        factors_in_model <- character(0)
      }
      choices_with_none <- c("None" = "", factors_in_model)

      # Compute smart defaults using plotting module
      defaults <- plotting$compute_aesthetic_defaults(
        current_color = shiny$isolate(input$plot_color_by) %||% "",
        current_linetype = shiny$isolate(input$plot_linetype_by) %||% "",
        current_facet = shiny$isolate(input$plot_facet_by) %||% "",
        factors_in_model = factors_in_model
      )

      shiny$updateSelectInput(
        session,
        "plot_color_by",
        choices = choices_with_none,
        selected = defaults$color
      )
      shiny$updateSelectInput(
        session,
        "plot_linetype_by",
        choices = choices_with_none,
        selected = defaults$linetype
      )
      shiny$updateSelectInput(
        session,
        "plot_facet_by",
        choices = choices_with_none,
        selected = defaults$facet
      )
    }) |>
      shiny$bindEvent(fitted_model_reactive())

    plot_object_reactive <- shiny$reactive({
      model_fit <- fitted_model_reactive()
      shiny$req(model_fit, model_fit$model)

      # Get valid factors from the fitted model
      valid_factors <- model_fit$param_info$factors %||% character(0)

      # Validate aesthetics using plotting module
      aesthetics <- plotting$build_validated_aesthetics(
        color_input = input$plot_color_by,
        linetype_input = input$plot_linetype_by,
        facet_input = input$plot_facet_by,
        valid_factors = valid_factors
      )

      # Check if there's content to plot
      if (
        !plotting$has_plot_content(
          input$show_population_lines,
          input$show_individual_lines,
          input$show_observed_points_plot
        )
      ) {
        shiny$showNotification(
          "Nothing to plot. Select observed points or prediction lines.",
          type = "warning"
        )
        return(ggplot2$ggplot() + ggplot2$theme_void())
      }

      # Build plot arguments using plotting module helpers
      show_lines_arg <- plotting$build_pred_lines_arg(
        input$show_population_lines,
        input$show_individual_lines
      )

      y_is_ll4 <- isTRUE(model_fit$param_info$y_is_ll4)
      inv_transform_fun <- if (y_is_ll4) beezdemand$ll4_inv else identity

      # Build 'at' for covariate conditioning in plot
      df_now <- data_to_analyze()
      cov_info <- build_covariate_modeling_info(df_now)

      p <- plot(
        model_fit,
        inv_fun = inv_transform_fun,
        y_trans = plotting$get_axis_transform(input$plot_y_trans_log),
        x_trans = plotting$get_axis_transform(input$plot_x_trans_log),
        at = cov_info$at_list,
        facet_formula = aesthetics$facet_formula,
        color_by = aesthetics$color,
        linetype_by = aesthetics$linetype,
        show_observed_data = input$show_observed_points_plot,
        show_pred_lines = show_lines_arg,
        title = input$plot_title,
        xlab = input$plot_xlab,
        ylab = input$plot_ylab
      )

      # Apply theme and styling using plotting module
      font_size <- input$plot_font_size %||% 14
      p <- plotting$apply_plot_theme(p, input$plot_theme, font_size)
      p <- plotting$apply_legend_position(p, input$plot_legend_position)

      # Add watermark if enabled
      if (isTRUE(input$show_watermark)) {
        p <- p + utils$add_shiny_logo(utils$watermark_tr)
      }

      # Apply palette if coloring by a discrete factor
      p <- plotting$apply_color_palette(
        p,
        color_var = aesthetics$color,
        fit_data = model_fit$data,
        palette_name = input$plot_palette,
        get_palette_fn = utils$get_palette_colors
      )

      return(p)
    }) |>
      shiny$bindCache(
        sidebar_reactives$run_trigger(),
        input$plot_color_by,
        input$plot_linetype_by,
        input$plot_facet_by,
        input$plot_theme,
        input$plot_font_size,
        input$plot_legend_position,
        input$plot_palette,
        input$plot_x_trans_log,
        input$plot_y_trans_log,
        input$show_population_lines,
        input$show_individual_lines,
        input$show_observed_points_plot,
        input$show_watermark,
        input$plot_title,
        input$plot_xlab,
        input$plot_ylab,
        sidebar_reactives$covariate(),
        sidebar_reactives$cov_center(),
        sidebar_reactives$cov_scale(),
        sidebar_reactives$cov_at_natural()
      ) |>
      shiny$bindEvent(
        input$update_plot_settings,
        fitted_model_reactive(),
        ignoreNULL = FALSE
      )

    esquisse$render_ggplot(
      id = "mixed_model_plot",
      expr = plot_object_reactive(),
      filename = "shinybeez-mixed-effects-demand-plot",
      width = shiny$reactive(
        if (is.null(input$esquisse_width_plot)) {
          700
        } else {
          input$esquisse_width_plot
        }
      ), # default width
      height = shiny$reactive(
        if (is.null(input$esquisse_height_plot)) {
          500
        } else {
          input$esquisse_height_plot
        }
      ) # default height
    )

    # Excel Export - Download Handler
    output$export_all_xlsx <- shiny$downloadHandler(
      filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        paste0("shinybeez_mixedeffects_export_", timestamp, ".xlsx")
      },
      content = function(file) {
        # Check if model is fitted (allow partial export if not)
        model_fit <- tryCatch(fitted_model_reactive(), error = function(e) NULL)
        has_model <- !is.null(model_fit) && !is.null(model_fit$model)

        wb <- openxlsx$createWorkbook()

        # --- Sheet 1: Summary (using export_utils module) ---
        openxlsx$addWorksheet(wb, "Summary")

        # Build summary using extracted module
        settings <- list(
          equation = sidebar_reactives$equation_form(),
          factors = sidebar_reactives$selected_factors(),
          factor_interaction = sidebar_reactives$factor_interaction(),
          id_var = sidebar_reactives$id_var(),
          x_var = sidebar_reactives$x_var(),
          y_var = sidebar_reactives$y_var(),
          random_effects = sidebar_reactives$random_effects_spec(),
          covariance_structure = sidebar_reactives$covariance_structure()
        )
        summary_data <- export_utils$build_summary_sheet(
          settings,
          version = "1.0.0"
        )

        # Add collapse levels info using module
        collapse_info <- tryCatch(
          sidebar_reactives$collapse_levels_reactive(),
          error = function(e) NULL
        )
        summary_data <- export_utils$add_collapse_info(
          summary_data,
          collapse_info
        )

        # Add fitting settings using module
        nlme_ctrl <- tryCatch(
          sidebar_reactives$nlme_controls(),
          error = function(e) NULL
        )
        summary_data <- export_utils$add_fitting_settings(
          summary_data,
          nlme_ctrl
        )

        openxlsx$writeData(wb, "Summary", summary_data, colNames = FALSE)

        # Style the title
        title_style <- openxlsx$createStyle(
          fontSize = 16,
          textDecoration = "bold"
        )
        openxlsx$addStyle(wb, "Summary", title_style, rows = 1, cols = 1)

        # Style section headers
        header_style <- openxlsx$createStyle(textDecoration = "bold")
        header_rows <- which(grepl("^---", summary_data$Item))
        for (row in header_rows) {
          openxlsx$addStyle(wb, "Summary", header_style, rows = row, cols = 1)
        }

        openxlsx$setColWidths(wb, "Summary", cols = 1:2, widths = c(35, 50))

        # --- Sheet 2: Data ---
        openxlsx$addWorksheet(wb, "Data")
        raw_data <- tryCatch(data_to_analyze(), error = function(e) NULL)
        if (!is.null(raw_data) && nrow(raw_data) > 0) {
          openxlsx$writeData(wb, "Data", raw_data)
          openxlsx$setColWidths(
            wb,
            "Data",
            cols = 1:ncol(raw_data),
            widths = "auto"
          )
        }

        # --- Sheet 3: Descriptives (using export_utils module) ---
        openxlsx$addWorksheet(wb, "Descriptives")
        if (!is.null(raw_data) && "y_for_model" %in% names(raw_data)) {
          factors <- sidebar_reactives$selected_factors()
          x_var_sel <- sidebar_reactives$x_var()
          grouping_vars <- c(factors, x_var_sel)
          grouping_vars <- grouping_vars[!grouping_vars %in% c("None", "")]

          desc_data <- export_utils$build_descriptives(raw_data, grouping_vars)
          if (!is.null(desc_data) && nrow(desc_data) > 0) {
            openxlsx$writeData(wb, "Descriptives", desc_data)
            openxlsx$setColWidths(
              wb,
              "Descriptives",
              cols = 1:ncol(desc_data),
              widths = "auto"
            )
          }
        }

        # --- Sheet 4: Systematic Criteria ---
        openxlsx$addWorksheet(wb, "Systematic_Criteria")
        df_raw <- tryCatch(
          sidebar_reactives$data_to_analyze_trigger(),
          error = function(e) NULL
        )
        if (!is.null(df_raw)) {
          id_col <- sidebar_reactives$id_var()
          x_col <- sidebar_reactives$x_var()
          y_col <- sidebar_reactives$y_var()
          if (all(c(id_col, x_col, y_col) %in% names(df_raw))) {
            df_sys <- df_raw[, c(id_col, x_col, y_col), drop = FALSE]
            names(df_sys) <- c("id", "x", "y")
            suppressWarnings({
              df_sys$x <- as.numeric(df_sys$x)
              df_sys$y <- as.numeric(df_sys$y)
            })
            df_sys <- df_sys[!is.na(df_sys$y), , drop = FALSE]
            systematic <- tryCatch(
              beezdemand$CheckUnsystematic(
                dat = df_sys,
                deltaq = 0.025,
                bounce = 0.1,
                reversals = 0,
                ncons0 = 2
              ),
              error = function(e) NULL
            )
            if (!is.null(systematic) && nrow(systematic) > 0) {
              openxlsx$writeData(wb, "Systematic_Criteria", systematic)
              openxlsx$setColWidths(
                wb,
                "Systematic_Criteria",
                cols = 1:ncol(systematic),
                widths = "auto"
              )
            }
          }
        }

        # === MODEL-DEPENDENT SHEETS (only if model is fitted) ===
        if (has_model) {
          # --- Sheet 5: Model Summary ---
          openxlsx$addWorksheet(wb, "Model_Summary")
          model_summary_text <- tryCatch(
            utils::capture.output(print(model_fit)),
            error = function(e) "Model summary not available"
          )
          model_summary_df <- data.frame(
            Output = model_summary_text,
            stringsAsFactors = FALSE
          )
          openxlsx$writeData(
            wb,
            "Model_Summary",
            model_summary_df,
            colNames = FALSE
          )
          openxlsx$setColWidths(wb, "Model_Summary", cols = 1, widths = 120)

          # --- Sheet 6: Fixed Effects ---
          openxlsx$addWorksheet(wb, "Fixed_Effects")
          fe <- nlme$fixef(model_fit)
          fe_df <- data.frame(Parameter = names(fe), Value = round(fe, 6))
          openxlsx$writeData(wb, "Fixed_Effects", fe_df)
          openxlsx$setColWidths(
            wb,
            "Fixed_Effects",
            cols = 1:2,
            widths = "auto"
          )

          # --- Sheet 7: Random Effects ---
          openxlsx$addWorksheet(wb, "Random_Effects")
          re_coefs <- tryCatch(stats$coef(model_fit), error = function(e) NULL)
          if (!is.null(re_coefs)) {
            re_df <- as.data.frame(re_coefs)
            id_col <- model_fit$param_info$id_var
            re_df[[id_col]] <- rownames(re_df)
            re_df <- re_df[, c(id_col, setdiff(names(re_df), id_col))]
            re_df[, sapply(re_df, is.numeric)] <- round(
              re_df[, sapply(re_df, is.numeric)],
              6
            )
            openxlsx$writeData(wb, "Random_Effects", re_df)
            openxlsx$setColWidths(
              wb,
              "Random_Effects",
              cols = 1:ncol(re_df),
              widths = "auto"
            )
          }

          # --- Individual Coefficients (if factors present) ---
          if (!is.null(model_fit$param_info$factors)) {
            individual_coefs <- tryCatch(
              beezdemand$get_individual_coefficients(
                model_fit,
                params = c("Q0", "alpha"),
                format = "wide"
              ),
              error = function(e) NULL
            )
            if (!is.null(individual_coefs) && nrow(individual_coefs) > 0) {
              openxlsx$addWorksheet(wb, "Individual_Coefficients")
              individual_coefs[, -1] <- round(individual_coefs[, -1], 6)
              openxlsx$writeData(
                wb,
                "Individual_Coefficients",
                individual_coefs
              )
              openxlsx$setColWidths(
                wb,
                "Individual_Coefficients",
                cols = 1:ncol(individual_coefs),
                widths = "auto"
              )
            }
          }
        } # end has_model

        # --- EMMs Data (Q0, Alpha, EV) - requires model ---
        emms_data <- if (has_model) {
          tryCatch(emms_data_reactive(), error = function(e) NULL)
        } else {
          NULL
        }
        if (!is.null(emms_data) && nrow(emms_data) > 0) {
          # Helper to extract parameter-specific columns
          extract_param_data <- function(data, param_pattern, factor_cols) {
            param_cols <- names(data)[
              grepl(param_pattern, names(data), ignore.case = TRUE) &
                !grepl(
                  paste0("_", param_pattern, "$"),
                  names(data),
                  ignore.case = TRUE
                )
            ]
            if (length(param_cols) == 0) {
              return(NULL)
            }
            cols_to_use <- intersect(c(factor_cols, param_cols), names(data))
            result <- data[, cols_to_use, drop = FALSE]
            result <- dplyr$distinct(result)
            return(result)
          }

          factor_cols <- names(emms_data)[!sapply(emms_data, is.numeric)]

          # Sheet 5: Q0 Estimates
          q0_data <- extract_param_data(emms_data, "q0|Q0", factor_cols)
          if (!is.null(q0_data) && nrow(q0_data) > 0) {
            openxlsx$addWorksheet(wb, "Q0_Estimates")
            q0_data[, sapply(q0_data, is.numeric)] <- round(
              q0_data[, sapply(q0_data, is.numeric)],
              6
            )
            openxlsx$writeData(wb, "Q0_Estimates", q0_data)
            openxlsx$setColWidths(
              wb,
              "Q0_Estimates",
              cols = 1:ncol(q0_data),
              widths = "auto"
            )
          }

          # Sheet 6: Alpha Estimates
          alpha_data <- extract_param_data(emms_data, "alpha", factor_cols)
          if (!is.null(alpha_data) && nrow(alpha_data) > 0) {
            openxlsx$addWorksheet(wb, "Alpha_Estimates")
            alpha_data[, sapply(alpha_data, is.numeric)] <- round(
              alpha_data[, sapply(alpha_data, is.numeric)],
              8
            )
            openxlsx$writeData(wb, "Alpha_Estimates", alpha_data)
            openxlsx$setColWidths(
              wb,
              "Alpha_Estimates",
              cols = 1:ncol(alpha_data),
              widths = "auto"
            )
          }

          # Sheet 7: EV Estimates
          ev_data <- extract_param_data(emms_data, "EV", factor_cols)
          if (!is.null(ev_data) && nrow(ev_data) > 0) {
            openxlsx$addWorksheet(wb, "EV_Estimates")
            ev_data[, sapply(ev_data, is.numeric)] <- round(
              ev_data[, sapply(ev_data, is.numeric)],
              4
            )
            openxlsx$writeData(wb, "EV_Estimates", ev_data)
            openxlsx$setColWidths(
              wb,
              "EV_Estimates",
              cols = 1:ncol(ev_data),
              widths = "auto"
            )
          }
        }

        # --- Comparisons (requires model) ---
        comparisons <- if (has_model) {
          tryCatch(comparisons_reactive(), error = function(e) NULL)
        } else {
          NULL
        }

        # Q0 Comparisons (both formats)
        if (!is.null(comparisons$Q0)) {
          if (
            !is.null(comparisons$Q0$contrasts_ratio) &&
              nrow(comparisons$Q0$contrasts_ratio) > 0
          ) {
            openxlsx$addWorksheet(wb, "Q0_Comparisons_Ratio")
            ratio_df <- dplyr$mutate(
              comparisons$Q0$contrasts_ratio,
              dplyr$across(where(is.numeric), ~ round(., 4))
            )
            openxlsx$writeData(wb, "Q0_Comparisons_Ratio", ratio_df)
            openxlsx$setColWidths(
              wb,
              "Q0_Comparisons_Ratio",
              cols = 1:ncol(ratio_df),
              widths = "auto"
            )
          }
          if (
            !is.null(comparisons$Q0$contrasts_log10) &&
              nrow(comparisons$Q0$contrasts_log10) > 0
          ) {
            openxlsx$addWorksheet(wb, "Q0_Comparisons_Log10")
            log10_df <- dplyr$mutate(
              comparisons$Q0$contrasts_log10,
              dplyr$across(where(is.numeric), ~ round(., 4))
            )
            openxlsx$writeData(wb, "Q0_Comparisons_Log10", log10_df)
            openxlsx$setColWidths(
              wb,
              "Q0_Comparisons_Log10",
              cols = 1:ncol(log10_df),
              widths = "auto"
            )
          }
        }

        # Alpha Comparisons (both formats)
        if (!is.null(comparisons$alpha)) {
          if (
            !is.null(comparisons$alpha$contrasts_ratio) &&
              nrow(comparisons$alpha$contrasts_ratio) > 0
          ) {
            openxlsx$addWorksheet(wb, "Alpha_Comparisons_Ratio")
            ratio_df <- dplyr$mutate(
              comparisons$alpha$contrasts_ratio,
              dplyr$across(where(is.numeric), ~ round(., 4))
            )
            openxlsx$writeData(wb, "Alpha_Comparisons_Ratio", ratio_df)
            openxlsx$setColWidths(
              wb,
              "Alpha_Comparisons_Ratio",
              cols = 1:ncol(ratio_df),
              widths = "auto"
            )
          }
          if (
            !is.null(comparisons$alpha$contrasts_log10) &&
              nrow(comparisons$alpha$contrasts_log10) > 0
          ) {
            openxlsx$addWorksheet(wb, "Alpha_Comparisons_Log10")
            log10_df <- dplyr$mutate(
              comparisons$alpha$contrasts_log10,
              dplyr$across(where(is.numeric), ~ round(., 4))
            )
            openxlsx$writeData(wb, "Alpha_Comparisons_Log10", log10_df)
            openxlsx$setColWidths(
              wb,
              "Alpha_Comparisons_Log10",
              cols = 1:ncol(log10_df),
              widths = "auto"
            )
          }
        }

        # --- Plot Sheet (requires model) ---
        if (has_model) {
          plot_obj <- tryCatch(plot_object_reactive(), error = function(e) NULL)
          if (!is.null(plot_obj)) {
            openxlsx$addWorksheet(wb, "Plot")
            temp_plot <- tempfile(fileext = ".png")
            ggplot2$ggsave(
              temp_plot,
              plot_obj,
              width = 10,
              height = 7,
              dpi = 150,
              bg = "white"
            )
            openxlsx$insertImage(
              wb,
              "Plot",
              temp_plot,
              width = 10,
              height = 7,
              startRow = 2,
              startCol = 2
            )
          }
        }

        # Save workbook
        openxlsx$saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
  })
}
