# # app/view/abuse_liability.R

box::use(
  bsicons,
  bslib,
  shiny,
  DT,
  esquisse,
  htmltools,
  ggplot2, # For plot customization if needed beyond esquisse
  dplyr, # For data manipulation
  readr,
  rlang, # For non-standard evaluation if needed
  shiny,
  stats,
  nlme,
  beezdemand # Your package for mixed-effects models
)

box::use(
  app / view / file_input,
  app / logic / utils, # For watermarks, etc.
  app / logic / validate # For data validation and transformations
)

#' @export
sidebar_ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    # upload file
    file_input$ui(ns("upload_abuse_liability")),
    shiny$hr(),
    shiny$h5("Basics (Required):"),
    # model selection
    shiny$selectInput(
      ns("model_choice"),
      label = "Select Model:",
      choices = list(
        "ZBEn (requires LL4-transformed Y)" = "zben",
        "Simplified Exponential (uses untransformed Y)" = "simplified"
      ),
      selected = "zben"
    ),
    # id variable selection
    shiny$selectInput(
      ns("id_variable_choice"),
      label = "Select ID variable:",
      choices = list(),
      selected = NULL
    ),
    # x variable selection
    shiny$selectInput(
      ns("x_variable_choice"),
      label = "Select X variable:",
      choices = list(),
      selected = NULL
    ),
    # y variable selection
    shiny$selectInput(
      ns("y_variable_choice"),
      label = "Select Y variable (or transform if 'y' is chosen):",
      choices = list(),
      selected = NULL
    ),
    shiny$selectInput(
      ns("y_trans_choice"),
      label = "Select Y transformation:",
      choices = list(
        "None" = "none",
        "LL4" = "ll4"
      ),
      selected = "none"
    ),
    shiny$hr(),
    shiny$h5("Fixed Effects Factors (Optional):"),
    shiny$selectInput(
      ns("factor1_choice"),
      label = "Select Factor 1:",
      choices = list("None" = ""), # Add a "None" option
      selected = ""
    ),
    shiny$selectInput(
      ns("factor2_choice"),
      label = "Select Factor 2:",
      choices = list("None" = ""), # Add a "None" option
      selected = ""
    ),
    shiny$checkboxInput(
      inputId = ns("factor_interaction"),
      label = "Include interaction between Factor 1 and Factor 2?",
      value = FALSE
    ),
    shiny$hr(),
    shiny$h5("Factor Level Collapsing (for Factor 1 if selected):"),
    shiny$checkboxInput(
      ns("enable_collapse_factor1"),
      "Enable Collapsing for Factor 1?",
      value = FALSE
    ),
    shiny$uiOutput(ns("collapse_factor1_ui")),
    shiny$hr(),
    shiny$h5("Random Effects:"),
    shiny$checkboxGroupInput(
      ns("random_effects_spec"),
      "Include random intercepts for:",
      choices = list(
        "Alpha (Sensitivity)" = "alpha",
        "Q0 (Max Consumption)" = "q0"
      ),
      selected = c("alpha", "q0")
    ),
    shiny$selectInput(
      ns("covariance_structure"), # Renamed from "cov" for clarity
      label = "Select random effects covariance structure:",
      choices = list(
        "Diagonal (no covariance)" = "pdDiag", # nlme syntax
        "General Positive-Definite (covariance estimated)" = "pdSymm" # nlme syntax
      ),
      selected = "pdDiag"
    ),
    shiny$p(shiny$strong("Analysis Type:"), "Mixed Effects Model"),
    shiny$actionButton(
      ns("run_mixed_model"),
      "Run Mixed Effects Model",
      icon = shiny$icon("cogs"),
      class = "btn-primary w-100"
    )
  )
}

#' @export
sidebar_server <- function(id, data_reactive) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$NS
    file_input$server("upload_abuse_liability", type = "abuse_liability")
    # Reactive to get the current data (uploaded or default 'ko')
    current_data <- shiny$reactive({
      data_uploaded <- session$userData$data$abuse_liability
      if (!is.null(data_uploaded)) {
        return(data_uploaded)
      } else {
        # Load example 'ko' data if no file is uploaded yet
        ko <- readr$read_csv("app/static/data/examples/shinybeez-ex-ko.csv")
        return(ko)
      }
    })

    # Observe the data and update input choices
    shiny$observe({
      df <- current_data()
      shiny$req(df)
      col_names <- names(df)

      # Guess likely candidates (can be refined)
      likely_id <- intersect(
        c("monkey", "ID", "id", "subject", "participant", "subjid", "subj_id"),
        tolower(col_names)
      )[1]
      if (is.na(likely_id)) {
        likely_id <- col_names[1]
      } # fallback

      likely_x <- intersect(
        c("x", "price", "ratio"),
        tolower(col_names)
      )[1]
      if (is.na(likely_x)) {
        likely_x <- col_names[2]
      } # fallback

      likely_y <- intersect(
        c("y", "y_ll4", "consumption", "response"),
        tolower(col_names)
      )[1]
      if (is.na(likely_y)) {
        likely_y <- col_names[3]
      } # fallback

      # For factors: exclude id, x, y, and other columns likely no applicable
      potential_factor_cols <- setdiff(
        col_names,
        c(
          likely_id,
          likely_x,
          likely_y,
          "monkey",
          "ID",
          "id",
          "subject",
          "participant",
          "subjid",
          "subj_id",
          "x",
          "price",
          "ratio",
          "y",
          "y_ll4",
          "consumption",
          "response"
        )
      )
      factor_choices <- Filter(
        function(col) {
          is.factor(df[[col]]) ||
            is.character(df[[col]]) ||
            is.numeric(df[[col]])
        },
        potential_factor_cols
      )

      shiny$updateSelectInput(
        session,
        "id_variable_choice",
        choices = col_names,
        selected = if (!is.na(likely_id) && likely_id %in% col_names) {
          likely_id
        } else {
          col_names[1]
        }
      )
      shiny$updateSelectInput(
        session,
        "x_variable_choice",
        choices = col_names,
        selected = if (!is.na(likely_x) && likely_x %in% col_names) {
          likely_x
        } else {
          col_names[2]
        }
      )
      shiny$updateSelectInput(
        session,
        "y_variable_choice",
        choices = col_names,
        selected = if (!is.na(likely_y) && likely_y %in% col_names) {
          likely_y
        } else {
          col_names[3]
        }
      )

      choices_with_none <- c("None" = "None", factor_choices)

      # Get current values if they exist
      current_factor1 <- if (!is.null(input$factor1_choice)) {
        input$factor1_choice
      } else {
        ""
      }
      current_factor2 <- if (!is.null(input$factor2_choice)) {
        input$factor2_choice
      } else {
        ""
      }

      # Determine initial selected values based on the data
      # For initial loading, suggest 'drug' and 'dose' if available
      if (current_factor1 == "" && current_factor2 == "") {
        # First time setup - make initial suggestions
        selected_factor1 <- ""

        selected_factor2 <- ""
      } else {
        # Use existing values if they're still valid
        selected_factor1 <- if (
          current_factor1 %in% c("None", factor_choices)
        ) {
          current_factor1
        } else {
          "None"
        }
        selected_factor2 <- if (
          current_factor2 %in% c("None", factor_choices)
        ) {
          current_factor2
        } else {
          "None"
        }
      }

      # Create filtered choices for each factor
      filtered_choices1 <- choices_with_none
      if (selected_factor2 != "") {
        # Remove factor2's selection from factor1's choices
        filtered_choices1 <- c(
          "None" = "None",
          setdiff(factor_choices, selected_factor2)
        )
      }

      filtered_choices2 <- choices_with_none
      if (selected_factor1 != "") {
        # Remove factor1's selection from factor2's choices
        filtered_choices2 <- c(
          "None" = "None",
          setdiff(factor_choices, selected_factor1)
        )
      }

      shiny$updateSelectInput(
        session,
        "factor1_choice",
        choices = filtered_choices1,
        selected = selected_factor1
      )

      shiny$updateSelectInput(
        session,
        "factor2_choice",
        choices = filtered_choices2,
        selected = selected_factor2
      )
    }) |>
      shiny$bindEvent(
        current_data(),
        input$factor1_choice,
        input$factor2_choice
      )

    # Collect selected factors for the model
    selected_factors_reactive <- shiny$reactive({
      factors <- c(input$factor1_choice, input$factor2_choice)
      factors <- factors[factors != ""] # Remove "None"
      if (length(factors) == 0) NULL else unique(factors)
    })

    factor1_levels_reactive <- shiny$reactive({
      df <- current_data()
      factor_col_name <- input$factor1_choice
      shiny$req(
        df,
        factor_col_name,
        factor_col_name != "None",
        factor_col_name != ""
      )
      if (!(factor_col_name %in% names(df))) {
        return(NULL)
      }

      unique_levels <- unique(as.character(df[[factor_col_name]]))
      sort(unique_levels[!is.na(unique_levels)])
    })

    output$collapse_factor1_ui <- shiny$renderUI({
      ns <- session$ns
      shiny$req(
        input$enable_collapse_factor1,
        input$factor1_choice != "None",
        input$factor1_choice != ""
      )
      all_levels <- factor1_levels_reactive()
      shiny$req(all_levels)

      shiny$tagList(
        shiny$p(paste("Collapsing levels for Factor:", input$factor1_choice)),
        shiny$textInput(
          ns("f1_group1_name"),
          "Name for New Group 1:",
          placeholder = "e.g., LowDose"
        ),
        shiny$selectizeInput(
          ns("f1_group1_levels"),
          "Selected levels to collapse into Group 1:",
          choices = all_levels,
          multiple = TRUE
        ),
        shiny$hr(style = "margin-top: 10px; margin-bottom: 10px;"),
        shiny$textInput(
          ns("f1_group2_name"),
          "Name for New Group 2:",
          placeholder = "e.g., HighDose"
        ),
        shiny$selectizeInput(
          ns("f1_group2_levels"),
          "Selected levels to collapse into Group 2:",
          choices = all_levels,
          multiple = TRUE
        )
      )
    })

    # This observer is triggered ONLY when the selection for "New Group 1" changes.
    # Its job is to update the choices and selection for "New Group 2" to prevent overlap.
    shiny$observeEvent(
      input$f1_group1_levels,
      {
        all_levels <- factor1_levels_reactive()
        selected_g1 <- input$f1_group1_levels %||% character(0)
        shiny$req(input$enable_collapse_factor1, all_levels)

        # Isolate the read of the other input to prevent a reactive feedback loop.
        current_selected_g2 <- shiny::isolate(
          input$f1_group2_levels %||% character(0)
        )
        # Determine what levels are still available for Group 2.
        available_for_g2 <- setdiff(all_levels, selected_g1)
        # Keep any of Group 2's previous selections that are still valid.
        new_selected_g2 <- intersect(current_selected_g2, available_for_g2)

        shiny$updateSelectizeInput(
          session,
          "f1_group2_levels",
          choices = available_for_g2,
          selected = new_selected_g2
        )
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # This observer does the mirror image: triggered ONLY by "New Group 2" changes.
    # It updates the choices and selection for "New Group 1".
    shiny$observeEvent(
      input$f1_group2_levels,
      {
        all_levels <- factor1_levels_reactive()
        selected_g2 <- input$f1_group2_levels %||% character(0)

        shiny$req(input$enable_collapse_factor1, all_levels)

        current_selected_g1 <- shiny::isolate(
          input$f1_group1_levels %||% character(0)
        )

        available_for_g1 <- setdiff(all_levels, selected_g2)
        new_selected_g1 <- intersect(current_selected_g1, available_for_g1)

        shiny$updateSelectizeInput(
          session,
          "f1_group1_levels",
          choices = available_for_g1,
          selected = new_selected_g1
        )
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # A reactive that processes the user's input from the collapse UI into the
    # specific nested list format required by the `beezdemand::fit_demand_mixed` function.
    # Also includes validation to prevent overlapping level assignments.
    parsed_collapse_levels_reactive <- shiny$reactive({
      if (
        !input$enable_collapse_factor1 ||
          is.null(input$factor1_choice) ||
          input$factor1_choice %in% c("None", "")
      ) {
        return(NULL)
      }

      factor_to_collapse_name <- input$factor1_choice

      g1_name <- trimws(input$f1_group1_name)
      g1_levels <- input$f1_group1_levels
      g2_name <- trimws(input$f1_group2_name)
      g2_levels <- input$f1_group2_levels

      # Final validation for overlap before sending to the model.
      if (
        length(g1_levels) > 0 &&
          length(g2_levels) > 0 &&
          length(intersect(g1_levels, g2_levels)) > 0
      ) {
        shiny$showNotification(
          "Overlap detected in selected levels for new groups. Please ensure unique selections.",
          type = "error",
          duration = 7
        )
        return("ERROR_OVERLAP")
      }

      collapse_list_for_factor <- list()
      if (!is.null(g1_name) && g1_name != "" && length(g1_levels) > 0) {
        collapse_list_for_factor[[g1_name]] <- g1_levels
      }
      if (!is.null(g2_name) && g2_name != "" && length(g2_levels) > 0) {
        collapse_list_for_factor[[g2_name]] <- g2_levels
      }

      if (length(collapse_list_for_factor) > 0) {
        final_list <- list()
        final_list[[factor_to_collapse_name]] <- collapse_list_for_factor
        return(final_list)
      } else {
        return(NULL)
      }
    })

    return(
      list(
        run_trigger = shiny$reactive(input$run_mixed_model),
        data_to_analyze_trigger = current_data,
        id_var = shiny$reactive(input$id_variable_choice),
        x_var = shiny$reactive(input$x_variable_choice),
        y_var = shiny$reactive(input$y_variable_choice),
        y_transform = shiny$reactive(input$y_trans_choice),
        selected_factors = selected_factors_reactive,
        factor_interaction = shiny$reactive(input$factor_interaction),
        random_effects_spec = shiny$reactive(input$random_effects_spec),
        covariance_structure = shiny$reactive(input$covariance_structure),
        equation_form = shiny$reactive(input$model_choice),
        collapse_levels_reactive = parsed_collapse_levels_reactive
      )
    )
  })
}

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
        DT$DTOutput(ns("emms_ev_table"))
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
      )
    )
  )
}

#' @export
navpanel_server <- function(id, sidebar_reactives) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_to_analyze <- shiny$reactive({
      df <- sidebar_reactives$data_to_analyze_trigger()
      shiny$req(df)

      y_var_col_name <- sidebar_reactives$y_var()
      y_transform_method <- sidebar_reactives$y_transform()
      shiny$req(y_var_col_name, y_transform_method)

      if (y_transform_method == "ll4") {
        if (y_var_col_name %in% names(df)) {
          # Create a new column for the transformed Y, or use y_ll4 if it's already the target
          # and the selected y_var IS y_ll4 (avoiding re-transforming y_ll4)
          if (y_var_col_name == "y_ll4" && "y_ll4" %in% names(df)) {
            df[["y_for_model"]] <- df[[y_var_col_name]]
            shiny$showNotification(
              paste(
                "Using existing column '",
                y_var_col_name,
                "' for y_for_model (LL4)."
              ),
              type = "message",
              duration = 3
            )
          } else {
            shiny$showNotification(
              paste(
                "Applying LL4 transformation to '",
                y_var_col_name,
                "' and using as y_for_model."
              ),
              type = "message",
              duration = 3
            )
            df[["y_for_model"]] <- beezdemand$ll4(df[[y_var_col_name]])
          }
        } else {
          shiny$showNotification(
            paste(
              "Selected Y variable '",
              y_var_col_name,
              "' not found for transformation."
            ),
            type = "error"
          )
          return(NULL)
        }
      } else {
        # "none" transformation
        # If no transformation, y_for_model is just the selected y_variable_choice
        if (y_var_col_name %in% names(df)) {
          df[["y_for_model"]] <- df[[y_var_col_name]]
          shiny$showNotification(
            paste(
              "Using existing column '",
              y_var_col_name,
              "' and using as y_for_model."
            ),
            type = "message",
            duration = 3
          )
        } else {
          shiny$showNotification(
            paste("Selected Y variable '", y_var_col_name, "' not found."),
            type = "error"
          )
          return(NULL)
        }
      }

      # convert any nonfactors selected to factors
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
      return(df)
    })

    output$input_data_table <- DT$renderDT({
      shiny$req(data_to_analyze())
      DT$datatable(
        data_to_analyze(),
        options = list(scrollX = TRUE, pageLength = 5, autoWidth = TRUE),
        filter = "top",
        class = "compact hover"
      )
    })

    # Descriptive stats for y_ll4
    output$descriptives_ll4_table <- DT$renderDT({
      df_processed <- data_to_analyze()
      shiny$req(df_processed, "y_for_model" %in% names(df_processed))

      factors <- sidebar_reactives$selected_factors()
      # browser()
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
        options = list(scrollX = TRUE, pageLength = 10, autoWidth = TRUE),
        filter = "top",
        class = "compact hover"
      )
    })

    # Reactive for the fitted model
    fitted_model_reactive <- shiny$eventReactive(
      sidebar_reactives$run_trigger(),
      {
        df <- data_to_analyze()
        shiny$req(df)

        # rand_eff_spec <- sidebar_reactives$random_effects_spec() # e.g., c("alpha", "q0")

        # # Construct the formula string for the random effects.
        # # These MUST match the parameter names Q0 and alpha used in fit_demand_mixed's fixed effects list.
        # # Your sidebar_reactives$random_effects_spec() gives c("alpha", "q0")
        # # We need to map these to the internal parameter names used by fit_demand_mixed if they differ,
        # # but fit_demand_mixed internally uses "Q0" and "alpha".
        # # So, if rand_eff_spec contains "q0", it should map to the "Q0" parameter.

        # random_params_for_formula <- character(0)
        # if ("q0" %in% rand_eff_spec) {
        #   random_params_for_formula <- c(random_params_for_formula, "Q0")
        # }
        # if ("alpha" %in% rand_eff_spec) {
        #   random_params_for_formula <- c(random_params_for_formula, "alpha")
        # }

        # if (length(random_params_for_formula) == 0) {
        #   shiny$showNotification(
        #     "No random effects selected for model parameters.",
        #     type = "error"
        #   )
        #   return(NULL)
        # }

        # # The formula for pdMat should be ~ Param1 + Param2 ...
        # # This refers to the parameters for which random effects are desired.
        # random_effects_formula_inner_str <- paste(
        #   random_params_for_formula,
        #   collapse = " + "
        # )
        # random_effects_formula_for_pdMat <- stats$as.formula(paste0(
        #   "~ ",
        #   random_effects_formula_inner_str
        # ))

        # cov_struct_choice <- sidebar_reactives$covariance_structure()

        # # Create the pdMat object
        # # This structure (a pdMat object) is directly passed to the `random` argument of `nlme()`
        # # when `groups` is also specified. `fit_demand_mixed` handles this correctly.
        # if (cov_struct_choice == "pdSymm") {
        #   random_effects_arg <- nlme$pdSymm(random_effects_formula_for_pdMat)
        # } else {
        #   # "pdDiag" or default
        #   random_effects_arg <- nlme$pdDiag(random_effects_formula_for_pdMat)
        # }

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
        random_effects_formula_to_pass <- stats::as.formula(paste0(
          random_formula_str,
          " ~ 1"
        ))

        # browser()
        y_var_actual <- if (
          sidebar_reactives$y_var() == "y_transform" ||
            !("y_ll4" %in% names(df))
        ) {
          shiny$req("y_ll4" %in% names(df)) # ensure transformation happened
          "y_ll4"
        } else {
          "y_ll4" # Defaulting to y_ll4 as per ZBEn
        }

        if (!(y_var_actual %in% names(df))) {
          shiny$showNotification(
            paste(
              "Required Y variable '",
              y_var_actual,
              "' not found in the data."
            ),
            type = "error"
          )
          return(NULL)
        }

        sel_factors <- setdiff(
          sidebar_reactives$selected_factors(),
          "None"
        )
        if (length(sel_factors) == 0) {
          sel_factors <- NULL
        }

        # Use quick controls from vignette for faster processing during development
        quick_nlme_control <- nlme$nlmeControl(
          msMaxIter = 50,
          niterEM = 25,
          maxIter = 50,
          pnlsTol = 0.1,
          tolerance = 1e-4,
          opt = "nlminb",
          msVerbose = FALSE
        )

        current_collapse_levels <- sidebar_reactives$collapse_levels_reactive()
        if (identical(current_collapse_levels, "ERROR_OVERLAP")) {
          shiny$showNotification(
            "Cannot fit model due to overlapping levels in collapse definition.",
            type = "error",
            duration = 7
          )
          return(NULL)
        }

        shiny$showNotification(
          "Fitting mixed-effects model... This may take a moment.",
          type = "message",
          duration = 10
        )
        # browser()
        print(random_effects_formula_to_pass)
        model_fit <- tryCatch(
          {
            beezdemand$fit_demand_mixed(
              data = df,
              y_var = y_var_actual,
              x_var = sidebar_reactives$x_var(), # Assuming 'x' is the price/ratio column from ko
              id_var = sidebar_reactives$id_var(), # Assuming 'monkey' is the ID from ko
              factors = sel_factors,
              factor_interaction = sidebar_reactives$factor_interaction(),
              equation_form = sidebar_reactives$equation_form(),
              collapse_levels = current_collapse_levels,
              random_effects = random_effects_formula_to_pass,
              covariance_structure = sidebar_reactives$covariance_structure(),
              nlme_control = quick_nlme_control, # Use faster controls for dev
              start_value_method = "pooled_nls" # Often more robust for complex models
            )
          },
          error = function(e) {
            shiny$showNotification(
              paste("Model fitting error:", e$message),
              type = "error",
              duration = NULL
            )
            NULL
          }
        )

        if (is.null(model_fit) || is.null(model_fit$model)) {
          shiny$showNotification(
            "Model fitting failed or did not converge.",
            type = "error"
          )
          return(NULL)
        }
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
        options = list(dom = "t")
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
          options = list(dom = "t")
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
        options = list(scrollX = TRUE, pageLength = 5, autoWidth = TRUE), # Added autoWidth
        class = "compact hover", # Added class for styling
        filter = "top" # Added column filters
      )
    })

    # EMMs and EV
    output$emms_ev_table <- DT$renderDT({
      model_fit <- fitted_model_reactive()
      shiny$req(model_fit, model_fit$model)
      sel_factors <- setdiff(
        sidebar_reactives$selected_factors(),
        "None"
      )
      if (is.null(sel_factors) || length(sel_factors) == 0) {
        sel_factors <- NULL
      }
      emms_data <- tryCatch(
        beezdemand$get_observed_demand_param_emms(
          fit_obj = model_fit,
          factors_in_emm = sel_factors, # Use selected factors
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
      shiny$req(emms_data)
      DT$datatable(
        dplyr$mutate(
          emms_data,
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
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    # Populate factor selectors for comparisons
    shiny$observe({
      model_fit <- fitted_model_reactive()
      shiny$req(model_fit, model_fit$model)
      sel_factors <- setdiff(
        sidebar_reactives$selected_factors(),
        "None"
      )

      output$comparison_factor_selector_ui <- shiny$renderUI({
        shiny$req(length(sel_factors) > 0)
        shiny$selectInput(
          ns("comparison_factor"),
          "Compare levels of:",
          choices = sel_factors,
          selected = sel_factors[1]
        )
      })

      output$contrast_by_selector_ui <- shiny$renderUI({
        shiny$req(length(sel_factors) > 1)
        main_comparison_factor <- input$comparison_factor
        shiny$req(main_comparison_factor)

        other_factors <- setdiff(sel_factors, main_comparison_factor)
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
      shiny$req(model_fit, model_fit$model, input$comparison_factor)

      main_factor <- input$comparison_factor
      by_factor <- input$contrast_by_factor

      specs_str <- main_factor
      if (!is.null(by_factor) && by_factor != "") {
        specs_str <- paste(main_factor, "*", by_factor)
      }

      # The factor to compare `by` should be the `contrast_by` argument
      # The primary factor for pairwise comparison is implicitly handled by `emmeans`
      # when `specs` defines the interaction.

      contrast_by_arg <- if (!is.null(by_factor) && by_factor != "") {
        by_factor
      } else {
        NULL
      }

      comps <- tryCatch(
        beezdemand$get_demand_comparisons(
          fit_obj = model_fit,
          params_to_compare = c("Q0", "alpha"),
          compare_specs = stats$as.formula(paste("~", specs_str)),
          contrast_by = contrast_by_arg, # Compare `main_factor` within levels of `by_factor`
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
    })

    output$comparisons_q0_table <- DT$renderDT({
      comps <- comparisons_reactive()
      shiny$req(comps, comps$Q0)
      display_data <- if (input$comparison_display_type == "ratio") {
        shiny$req(comps$Q0$contrasts_ratio)
        dplyr$mutate(
          comps$Q0$contrasts_ratio,
          dplyr$across(where(is.numeric), ~ round(., 4))
        )
      } else {
        shiny$req(comps$Q0$contrasts_log10)
        dplyr$mutate(
          comps$Q0$contrasts_log10,
          dplyr$across(where(is.numeric), ~ round(., 4))
        )
      }

      caption_text <- if (input$comparison_display_type == "ratio") {
        "Pairwise Comparisons for Q0 (Natural Scale Ratios)"
      } else {
        "Pairwise Comparisons for Q0 (log10 difference)"
      }

      DT$datatable(
        display_data,
        caption = htmltools$tags$caption(
          style = "caption-side: top; text-align: center;",
          caption_text
        ),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 5, autoWidth = TRUE),
        class = "compact hover",
        filter = "top"
      )
    })

    # Conditional UI for Q0 Comparisons
    output$comparisons_q0_ui <- shiny$renderUI({
      comps <- comparisons_reactive()
      if (!is.null(comps) && !is.null(comps$Q0)) {
        display_data <- if (input$comparison_display_type == "ratio") {
          comps$Q0$contrasts_ratio
        } else {
          comps$Q0$contrasts_log10
        }
        if (!is.null(display_data) && nrow(display_data) > 0) {
          shiny$tagList(
            shiny$h4("Q0 Comparisons"),
            DT$DTOutput(ns("comparisons_q0_table"))
          )
        }
      }
    })

    # Conditional UI for Alpha Comparisons
    output$comparisons_alpha_ui <- shiny$renderUI({
      comps <- comparisons_reactive()
      if (!is.null(comps) && !is.null(comps$alpha)) {
        display_data <- if (input$comparison_display_type == "ratio") {
          comps$alpha$contrasts_ratio
        } else {
          comps$alpha$contrasts_log10
        }
        if (!is.null(display_data) && nrow(display_data) > 0) {
          shiny$tagList(
            shiny$h4("Alpha Comparisons"),
            DT$DTOutput(ns("comparisons_alpha_table"))
          )
        }
      }
    })

    output$comparisons_alpha_table <- DT$renderDT({
      comps <- comparisons_reactive()
      shiny$req(comps, comps$alpha)

      display_data <- if (input$comparison_display_type == "ratio") {
        shiny$req(comps$alpha$contrasts_ratio)
        dplyr$mutate(
          comps$alpha$contrasts_ratio,
          dplyr$across(where(is.numeric), ~ round(., 4))
        )
      } else {
        shiny$req(comps$alpha$contrasts_log10)
        dplyr$mutate(
          comps$alpha$contrasts_log10,
          dplyr$across(where(is.numeric), ~ round(., 4))
        )
      }

      caption_text <- if (input$comparison_display_type == "ratio") {
        "Pairwise Comparisons for Alpha (Natural Scale Ratios)"
      } else {
        "Pairwise Comparisons for Alpha (log10 difference)"
      }

      DT$datatable(
        display_data,
        caption = htmltools$tags$caption(
          style = "caption-side: top; text-align: center;",
          caption_text
        ),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 5, autoWidth = TRUE),
        class = "compact hover",
        filter = "top"
      )
    })

    # Plotting Logic
    # Populate plot aesthetic choices
    shiny$observe({
      model_fit <- fitted_model_reactive()
      shiny$req(model_fit, model_fit$model)
      # sel_factors <- setdiff(
      #   sidebar_reactives$selected_factors(),
      #   "None"
      # )
      # Use the factors from the fitted model as the source of truth
      factors_in_model <- model_fit$param_info$factors
      if (is.null(factors_in_model)) {
        factors_in_model <- character(0)
      }
      choices_with_none <- c("None" = "", factors_in_model)
      # choices_with_none <- c("None" = "", sel_factors)

      # Preserve the user's current selections if they are still valid factors
      # Otherwise, reset them to "" (None).
      selected_color <- shiny::isolate(input$plot_color_by)
      if (!selected_color %in% factors_in_model) {
        selected_color <- ""
      }

      selected_linetype <- shiny::isolate(input$plot_linetype_by)
      if (!selected_linetype %in% factors_in_model) {
        selected_linetype <- ""
      }

      selected_facet <- shiny::isolate(input$plot_facet_by)
      if (!selected_facet %in% factors_in_model) {
        selected_facet <- ""
      }

      # Set smart defaults if the model has factors and selections are now empty
      if (length(factors_in_model) > 0) {
        if (selected_color == "" && selected_linetype != factors_in_model[1]) {
          selected_color <- factors_in_model[1]
        }
        if (
          length(factors_in_model) > 1 &&
            selected_linetype == "" &&
            selected_color != factors_in_model[2]
        ) {
          selected_linetype <- factors_in_model[2]
        }
      }

      shiny$updateSelectInput(
        session,
        "plot_color_by",
        choices = choices_with_none,
        selected = selected_color
      )
      shiny$updateSelectInput(
        session,
        "plot_linetype_by",
        choices = choices_with_none,
        selected = selected_linetype
      )
      shiny$updateSelectInput(
        session,
        "plot_facet_by",
        choices = choices_with_none,
        selected = selected_facet
      )

      # shiny$updateSelectInput(
      #   session,
      #   "plot_color_by",
      #   choices = choices_with_none,
      #   selected = if (!is.null(sel_factors) && length(sel_factors) > 0) {
      #     sel_factors[1]
      #   } else {
      #     ""
      #   }
      # )
      # shiny$updateSelectInput(
      #   session,
      #   "plot_linetype_by",
      #   choices = choices_with_none,
      #   selected = if (!is.null(sel_factors) && length(sel_factors) > 1) {
      #     sel_factors[2]
      #   } else {
      #     ""
      #   }
      # )
      # shiny$updateSelectInput(
      #   session,
      #   "plot_facet_by",
      #   choices = choices_with_none,
      #   selected = ""
      # )
    }) |>
      shiny$bindEvent(fitted_model_reactive())

    plot_object_reactive <- shiny$reactive({
      model_fit <- fitted_model_reactive()
      shiny$req(model_fit, model_fit$model)

      # plot_color <- if (input$plot_color_by == "") NULL else input$plot_color_by
      # plot_linetype <- if (input$plot_linetype_by == "") {
      #   NULL
      # } else {
      #   input$plot_linetype_by
      # }
      # plot_facet_str <- if (input$plot_facet_by == "") {
      #   NULL
      # } else {
      #   paste("~", input$plot_facet_by)
      # }

      # --- START: ROBUST PLOTTING LOGIC ---
      # Get the list of factors that are actually in the fitted model. This is the source of truth.
      valid_factors_in_model <- model_fit$param_info$factors
      if (is.null(valid_factors_in_model)) {
        valid_factors_in_model <- character(0)
      }

      # Validate selected color aesthetic. If the selection is empty or not a valid factor
      # in the current model, treat it as NULL.
      plot_color <- input$plot_color_by
      if (
        is.null(plot_color) ||
          !nzchar(plot_color) ||
          !(plot_color %in% valid_factors_in_model)
      ) {
        plot_color <- NULL
      }

      # Validate selected linetype aesthetic.
      plot_linetype <- input$plot_linetype_by
      if (
        is.null(plot_linetype) ||
          !nzchar(plot_linetype) ||
          !(plot_linetype %in% valid_factors_in_model)
      ) {
        plot_linetype <- NULL
      }

      # Validate selected facet aesthetic.
      plot_facet_var <- input$plot_facet_by
      plot_facet_str <- NULL # Default to no facet
      if (
        !is.null(plot_facet_var) &&
          nzchar(plot_facet_var) &&
          (plot_facet_var %in% valid_factors_in_model)
      ) {
        plot_facet_str <- paste("~", plot_facet_var)
      }
      # --- END: ROBUST PLOTTING LOGIC ---

      pred_lines_to_show <- c()
      if (input$show_population_lines) {
        pred_lines_to_show <- c(pred_lines_to_show, "population")
      }
      if (input$show_individual_lines) {
        pred_lines_to_show <- c(pred_lines_to_show, "individual")
      }
      if (length(pred_lines_to_show) == 0 && !input$show_observed_points_plot) {
        shiny$showNotification(
          "Nothing to plot. Select observed points or prediction lines.",
          type = "warning"
        )
        return(ggplot2$ggplot() + ggplot2$theme_void())
      }

      # Determine inv_fun based on original y_var.
      # Assuming LL4 transform always leads to log10 scale for 'zben'.
      # If y_var was y_ll4, it's already log10. If it was y, it got transformed.
      inv_transform_fun <- beezdemand$ll4_inv # Assuming ll4_inv is 10^x or similar

      # If y_trans is log, use inv_fun, otherwise plot directly.
      # The plot function's inv_fun is for the MODEL's y-scale back to natural.
      # The y_trans in plot is for the VISUAL scale of the y-axis.
      current_y_trans <- if (input$plot_y_trans_log) {
        "pseudo_log"
      } else {
        "identity"
      }
      current_x_trans <- if (input$plot_x_trans_log) {
        "pseudo_log"
      } else {
        "identity"
      }

      # Simplify show_pred_lines
      if (length(pred_lines_to_show) == 0) {
        show_lines_arg <- FALSE
      } else if (all(c("population", "individual") %in% pred_lines_to_show)) {
        show_lines_arg <- c("population", "individual")
      } else if ("population" %in% pred_lines_to_show) {
        show_lines_arg <- "population" # or TRUE
      } else if ("individual" %in% pred_lines_to_show) {
        show_lines_arg <- "individual"
      } else {
        show_lines_arg <- FALSE
      }

      p <- plot(
        model_fit,
        inv_fun = inv_transform_fun, # To get natural scale for predictions
        y_trans = current_y_trans, # Visual scale for y-axis
        x_trans = current_x_trans, # Visual scale for x-axis
        facet_formula = if (!is.null(plot_facet_str)) {
          stats$as.formula(plot_facet_str)
        } else {
          NULL
        },
        color_by = plot_color,
        linetype_by = plot_linetype,
        show_observed_data = input$show_observed_points_plot,
        show_pred_lines = show_lines_arg,
        title = input$plot_title,
        xlab = input$plot_xlab,
        ylab = input$plot_ylab
      ) +
        utils$add_shiny_logo(utils$watermark_tr)

      return(p)
    }) |>
      shiny$bindEvent(
        input$update_plot_settings,
        fitted_model_reactive(),
        ignoreNULL = FALSE
      )

    esquisse$render_ggplot(
      id = "mixed_model_plot",
      expr = plot_object_reactive(),
      filename = "shinybeez-abuse-liability-mixed-plot",
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
  })
}
