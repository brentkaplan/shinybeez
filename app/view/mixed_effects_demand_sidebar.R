#' Mixed Effects Demand - Sidebar Module
#'
#' Contains sidebar_ui and sidebar_server for the mixed effects demand tab.

box::use(
  bsicons,
  bslib,
  dplyr,
  readr,
  shiny,
  stats
)

box::use(
  app / view / file_input,
  app / logic / logging_utils,
  app / logic / mixed_effects / collapse_levels,
  app / logic / mixed_effects / data_prep,
  app / logic / mixed_effects / model_fitting
)

# Module-local cache for example data
.local_cache <- new.env(parent = emptyenv())

get_default_ko <- function() {
  if (is.null(.local_cache$ko)) {
    .local_cache$ko <- readr$read_csv(
      "app/static/data/examples/shinybeez-ex-ko.csv",
      show_col_types = FALSE
    )
  }
  .local_cache$ko
}

#' @export
sidebar_ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    # upload file
    file_input$ui(ns("upload_mixed_effects_demand")),
    shiny$hr(),
    shiny$h5("Basics (Required):"),
    # model selection
    shiny$selectInput(
      ns("model_choice"),
      label = "Select Model:",
      choices = list(
        "ZBEn (y automatically LL4-transformed)" = "zben",
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
      label = "Select Y variable (will automatically be transformed if ZBEn is chosen):",
      choices = list(),
      selected = NULL
    ),
    shiny$hr(),
    shiny$h5("Fixed Effects Factors (Optional):"),
    shiny$selectInput(
      ns("factor1_choice"),
      label = "Select Factor 1:",
      choices = list("None" = ""), # Add a "None" option
      selected = "",
      selectize = FALSE
    ),
    shiny$selectInput(
      ns("factor2_choice"),
      label = "Select Factor 2:",
      choices = list("None" = ""), # Add a "None" option
      selected = "",
      selectize = FALSE
    ),
    shiny$checkboxInput(
      inputId = ns("factor_interaction"),
      label = "Include interaction between Factor 1 and Factor 2?",
      value = FALSE
    ),
    shiny$hr(),
    # Server-rendered containers ensure we only show collapsing when >= 2 levels
    shiny$uiOutput(ns("collapse_factor1_container")),
    shiny$uiOutput(ns("collapse_factor2_container")),
    shiny$hr(),
    shiny$h5("Continuous Covariate (Optional):"),
    shiny$selectInput(
      ns("covariate_choice"),
      label = "Select numeric covariate:",
      choices = list("None" = ""),
      selected = "",
      selectize = FALSE
    ),
    shiny$checkboxInput(
      ns("cov_center"),
      label = "Center covariate (global mean = 0)",
      value = TRUE
    ),
    shiny$checkboxInput(
      ns("cov_scale"),
      label = "Scale covariate (global sd = 1)",
      value = FALSE
    ),
    shiny$numericInput(
      ns("cov_at"),
      label = bslib$tooltip(
        trigger = list(
          "Covariate value for EMMs/plots (natural scale)",
          bsicons$bs_icon("question-circle")
        ),
        paste0(
          "Sets the value at which the continuous covariate is held constant ",
          "when calculating Estimated Marginal Means (EMMs), pairwise ",
          "comparisons, and drawing prediction lines in the plot.\n\n",
          "Enter this on the natural scale = the raw units of your data ",
          "column (before any centering/scaling). If 'Center' and/or ",
          "'Scale' are checked, this natural-scale value is internally ",
          "transformed using the column's global mean/SD to condition ",
          "the model."
        )
      ),
      value = NA,
      step = 0.1
    ),
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
    bslib$accordion(
      open = FALSE,
      style = "margin-bottom: 12px;",
      bslib$accordion_panel(
        title = "Fitting Controls (Advanced)",
        icon = bsicons$bs_icon("gear"),
        shiny$p(
          style = "margin-bottom: 6px; color: #6c757d;",
          paste0(
            "Tip: Start with Balanced. If PNLS stalls, raise pnlsMaxIter ",
            "to 20-30 and tighten pnlsTol to 1e-4; if the outer loop ",
            "stops early, try maxIter 150-200."
          )
        ),
        shiny$div(
          style = "margin-bottom: 8px;",
          shiny$selectInput(
            ns("fit_presets"),
            label = bslib$tooltip(
              trigger = list(
                "Quick presets",
                bsicons$bs_icon("info-circle")
              ),
              "Apply a preset configuration for iteration limits and tolerances."
            ),
            choices = c("Balanced", "Faster", "Stricter"),
            selected = "Balanced"
          ),
          shiny$actionButton(
            ns("fit_reset_defaults"),
            label = "Reset to defaults",
            icon = shiny$icon("rotate-left"),
            class = "btn-sm"
          )
        ),
        shiny$h6("Iterations"),
        shiny$numericInput(
          ns("maxIter"),
          label = bslib$tooltip(
            trigger = list(
              "maxIter (outer iterations)",
              bsicons$bs_icon("info-circle")
            ),
            "Max number of outer PNLS↔LME cycles. Raise if the outer loop hits its limit while still improving."
          ),
          value = 100,
          min = 30,
          max = 300,
          step = 10
        ),
        shiny$numericInput(
          ns("pnlsMaxIter"),
          label = bslib$tooltip(
            trigger = list(
              "pnlsMaxIter (PNLS inner iterations)",
              bsicons$bs_icon("info-circle")
            ),
            "Max PNLS iterations for fixed-effects updates. Raise (e.g., 20–30) if PNLS stalls or oscillates."
          ),
          value = 7,
          min = 7,
          max = 50,
          step = 1
        ),
        shiny$numericInput(
          ns("msMaxIter"),
          label = bslib$tooltip(
            trigger = list(
              "msMaxIter (LME optimizer iterations)",
              bsicons$bs_icon("info-circle")
            ),
            "Max iterations for the variance–covariance optimizer. Raise if it stops early while still improving."
          ),
          value = 100,
          min = 50,
          max = 500,
          step = 10
        ),
        shiny$h6("Tolerances and step"),
        shiny$numericInput(
          ns("tolerance"),
          label = bslib$tooltip(
            trigger = list("tolerance (outer)", bsicons$bs_icon("info-circle")),
            "Overall convergence tolerance. Loosen (1e-5–1e-4) for speed; tighten (1e-7) for exacting fits."
          ),
          value = 1e-3,
          min = 1e-7,
          max = 1e-4,
          step = 1e-5
        ),
        shiny$numericInput(
          ns("pnlsTol"),
          label = bslib$tooltip(
            trigger = list(
              "pnlsTol (PNLS tolerance)",
              bsicons$bs_icon("info-circle")
            ),
            "PNLS convergence tolerance. Tighten (1e-4) if stopping too early; loosen slightly (~2e-3) if jittery."
          ),
          value = 1e-3,
          min = 1e-4,
          max = 1e-2,
          step = 1e-4
        ),
        shiny$numericInput(
          ns("minScale"),
          label = bslib$tooltip(
            trigger = list(
              "minScale (PNLS step shrink)",
              bsicons$bs_icon("info-circle")
            ),
            paste0(
              "Minimum step shrink for PNLS backtracking. Lower (1e-4) ",
              "for finer steps if overshooting; higher (1e-2) for speed."
            )
          ),
          value = 1e-3,
          min = 1e-4,
          max = 1e-2,
          step = 1e-4
        ),
        shiny$h6("Initialization"),
        shiny$numericInput(
          ns("niterEM"),
          label = bslib$tooltip(
            trigger = list(
              "niterEM (EM warm-start)",
              bsicons$bs_icon("info-circle")
            ),
            "EM iterations to stabilize variance starts. Try 50–80 for unstable variance components."
          ),
          value = 50,
          min = 0,
          max = 100,
          step = 5
        )
      )
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

    # Create session-specific logger
    session_logger <- logging_utils$create_session_logger(session)
    session_logger$info(
      "Mixed Effects Demand sidebar module initialized",
      "module_init"
    )

    file_input$server(
      "upload_mixed_effects_demand",
      type = "mixed_effects_demand"
    )
    # Reactive to get the current data (uploaded or default 'ko')
    current_data <- shiny$reactive({
      data_uploaded <- session$userData$data$mixed_effects_demand
      if (!is.null(data_uploaded)) {
        session_logger$data_processing(
          operation = "user_data_loaded",
          data_info = list(
            rows = nrow(data_uploaded),
            cols = ncol(data_uploaded),
            source = "user_upload"
          )
        )
        return(data_uploaded)
      } else {
        # Load example 'ko' data from module cache if no file is uploaded yet
        ko <- get_default_ko()
        session_logger$data_processing(
          operation = "default_data_loaded",
          data_info = list(
            rows = nrow(ko),
            cols = ncol(ko),
            source = "example_ko_data"
          )
        )
        return(ko)
      }
    })

    # A. Update ID/X/Y choices ONLY when data changes
    shiny$observeEvent(
      current_data(),
      {
        df <- current_data()
        shiny$req(df)
        col_names <- names(df)

        # Use extracted data_prep module for column guessing
        guessed <- data_prep$guess_variable_columns(df)
        id_selected <- guessed$id
        x_selected <- guessed$x
        y_selected <- guessed$y

        # Explicitly set selected values to avoid defaulting to first choice
        shiny$updateSelectInput(
          session,
          "id_variable_choice",
          choices = col_names,
          selected = id_selected
        )
        shiny$updateSelectInput(
          session,
          "x_variable_choice",
          choices = col_names,
          selected = x_selected
        )
        shiny$updateSelectInput(
          session,
          "y_variable_choice",
          choices = col_names,
          selected = y_selected
        )
      },
      ignoreInit = FALSE
    )

    # C. Populate numeric-only covariate choices (exclude id/x/y and selected factors)
    shiny$observeEvent(
      list(
        current_data(),
        input$id_variable_choice,
        input$x_variable_choice,
        input$y_variable_choice,
        input$factor1_choice,
        input$factor2_choice
      ),
      {
        df <- current_data()
        shiny$req(df)
        col_names <- names(df)

        reserved_lower <- c(
          "monkey",
          "id",
          "subject",
          "participant",
          "subjid",
          "subj_id",
          "responseid",
          "x",
          "price",
          "ratio",
          "y",
          "y_ll4",
          "consumption",
          "response"
        )
        selected_ixy <- c(
          input$id_variable_choice,
          input$x_variable_choice,
          input$y_variable_choice
        )
        selected_factors <- c(input$factor1_choice, input$factor2_choice)
        selected_factors <- selected_factors[
          !is.na(selected_factors) & nzchar(selected_factors)
        ]
        exclusions <- unique(c(
          selected_ixy,
          selected_factors,
          col_names[tolower(col_names) %in% reserved_lower]
        ))

        is_numeric_like <- function(v) {
          if (is.numeric(v) || is.integer(v) || inherits(v, "integer64")) {
            return(TRUE)
          }
          if (is.character(v)) {
            suppressWarnings({
              num <- as.numeric(v)
            })
            non_na_orig <- sum(!is.na(v))
            non_na_conv <- sum(!is.na(num))
            if (non_na_orig == 0) {
              return(FALSE)
            }
            return((non_na_conv / non_na_orig) >= 0.95)
          }
          FALSE
        }

        numeric_candidates <- Filter(
          function(col) is_numeric_like(df[[col]]),
          setdiff(col_names, exclusions)
        )
        named_choices <- stats$setNames(numeric_candidates, numeric_candidates)
        choices_with_none <- c("None" = "", named_choices)

        # Preserve a valid selection if possible
        current_sel <- input$covariate_choice
        new_sel <- if (
          !is.null(current_sel) && current_sel %in% numeric_candidates
        ) {
          current_sel
        } else {
          ""
        }

        shiny$updateSelectInput(
          session,
          "covariate_choice",
          choices = choices_with_none,
          selected = new_sel
        )

        # Default the "at" value to the mean if not set
        if (nzchar(new_sel)) {
          x <- df[[new_sel]]
          xnum <- if (is.character(x)) suppressWarnings(as.numeric(x)) else x
          mu <- suppressWarnings(mean(xnum, na.rm = TRUE))
          if (is.finite(mu) && (is.null(input$cov_at) || is.na(input$cov_at))) {
            shiny$updateNumericInput(session, "cov_at", value = round(mu, 3))
          }
        }
      },
      ignoreInit = FALSE
    )

    # B. Maintain factor choices (stable):
    # 1) On data change: compute base choices and initialize both selects.
    shiny$observeEvent(
      current_data(),
      {
        df <- current_data()
        shiny$req(df)
        col_names <- names(df)

        reserved_lower <- c(
          "monkey",
          "id",
          "subject",
          "participant",
          "subjid",
          "subj_id",
          "responseid",
          "x",
          "price",
          "ratio",
          "y",
          "y_ll4",
          "consumption",
          "response"
        )
        exclusions_base <- unique(c(
          input$id_variable_choice,
          input$x_variable_choice,
          input$y_variable_choice,
          col_names[tolower(col_names) %in% reserved_lower]
        ))
        base_choices <- setdiff(col_names, exclusions_base)
        factor_choices <- Filter(
          function(col) {
            is.factor(df[[col]]) ||
              is.character(df[[col]]) ||
              is.numeric(df[[col]])
          },
          base_choices
        )
        choices_with_none <- c(
          "None" = "",
          stats$setNames(factor_choices, factor_choices)
        )

        f1 <- input$factor1_choice
        f2 <- input$factor2_choice
        f1 <- if (!is.null(f1) && f1 %in% factor_choices) f1 else ""
        f2 <- if (!is.null(f2) && f2 %in% factor_choices && f2 != f1) f2 else ""

        shiny$updateSelectInput(
          session,
          "factor1_choice",
          choices = choices_with_none,
          selected = f1
        )
        # For factor2, exclude f1 if set
        ch2 <- if (nzchar(f1)) setdiff(factor_choices, f1) else factor_choices
        shiny$updateSelectInput(
          session,
          "factor2_choice",
          choices = c("None" = "", stats$setNames(ch2, ch2)),
          selected = f2
        )
      },
      ignoreInit = FALSE
    )

    # 2) When factor1 changes: only update factor2 choices to exclude factor1 (preserve factor1)
    shiny$observeEvent(
      input$factor1_choice,
      {
        df <- current_data()
        shiny$req(df)
        col_names <- names(df)
        reserved_lower <- c(
          "monkey",
          "id",
          "subject",
          "participant",
          "subjid",
          "subj_id",
          "responseid",
          "x",
          "price",
          "ratio",
          "y",
          "y_ll4",
          "consumption",
          "response"
        )
        exclusions_base <- unique(c(
          input$id_variable_choice,
          input$x_variable_choice,
          input$y_variable_choice,
          col_names[tolower(col_names) %in% reserved_lower]
        ))
        base_choices <- setdiff(col_names, exclusions_base)
        factor_choices <- Filter(
          function(col) {
            is.factor(df[[col]]) ||
              is.character(df[[col]]) ||
              is.numeric(df[[col]])
          },
          base_choices
        )
        f1 <- input$factor1_choice
        ch2 <- if (nzchar(f1)) setdiff(factor_choices, f1) else factor_choices
        f2 <- input$factor2_choice
        f2 <- if (!is.null(f2) && f2 %in% ch2) f2 else ""
        shiny$updateSelectInput(
          session,
          "factor2_choice",
          choices = c("None" = "", stats$setNames(ch2, ch2)),
          selected = f2
        )
      },
      ignoreInit = TRUE
    )

    # 3) When factor2 changes: only update factor1 choices to exclude factor2 (preserve factor2)
    shiny$observeEvent(
      input$factor2_choice,
      {
        df <- current_data()
        shiny$req(df)
        col_names <- names(df)
        reserved_lower <- c(
          "monkey",
          "id",
          "subject",
          "participant",
          "subjid",
          "subj_id",
          "responseid",
          "x",
          "price",
          "ratio",
          "y",
          "y_ll4",
          "consumption",
          "response"
        )
        exclusions_base <- unique(c(
          input$id_variable_choice,
          input$x_variable_choice,
          input$y_variable_choice,
          col_names[tolower(col_names) %in% reserved_lower]
        ))
        base_choices <- setdiff(col_names, exclusions_base)
        factor_choices <- Filter(
          function(col) {
            is.factor(df[[col]]) ||
              is.character(df[[col]]) ||
              is.numeric(df[[col]])
          },
          base_choices
        )
        f2 <- input$factor2_choice
        ch1 <- if (nzchar(f2)) setdiff(factor_choices, f2) else factor_choices
        f1 <- input$factor1_choice
        f1 <- if (!is.null(f1) && f1 %in% ch1) f1 else ""
        shiny$updateSelectInput(
          session,
          "factor1_choice",
          choices = c("None" = "", stats$setNames(ch1, ch1)),
          selected = f1
        )
      },
      ignoreInit = TRUE
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

    factor2_levels_reactive <- shiny$reactive({
      df <- current_data()
      factor_col_name <- input$factor2_choice
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

    # --- Helpers to clear collapse inputs ---
    clear_factor1_collapse <- function() {
      # Set enable flag FALSE if present
      if (!is.null(input$enable_collapse_factor1)) {
        shiny$updateCheckboxInput(
          session,
          "enable_collapse_factor1",
          value = FALSE
        )
      }
      # Clear names and selections if inputs exist
      if (!is.null(input$f1_group1_name)) {
        shiny$updateTextInput(session, "f1_group1_name", value = "")
      }
      if (!is.null(input$f1_group1_levels)) {
        shiny$updateSelectizeInput(
          session,
          "f1_group1_levels",
          selected = character(0)
        )
      }
      if (!is.null(input$f1_group2_name)) {
        shiny$updateTextInput(session, "f1_group2_name", value = "")
      }
      if (!is.null(input$f1_group2_levels)) {
        shiny$updateSelectizeInput(
          session,
          "f1_group2_levels",
          selected = character(0)
        )
      }
    }

    clear_factor2_collapse <- function() {
      if (!is.null(input$enable_collapse_factor2)) {
        shiny$updateCheckboxInput(
          session,
          "enable_collapse_factor2",
          value = FALSE
        )
      }
      if (!is.null(input$f2_group1_name)) {
        shiny$updateTextInput(session, "f2_group1_name", value = "")
      }
      if (!is.null(input$f2_group1_levels)) {
        shiny$updateSelectizeInput(
          session,
          "f2_group1_levels",
          selected = character(0)
        )
      }
      if (!is.null(input$f2_group2_name)) {
        shiny$updateTextInput(session, "f2_group2_name", value = "")
      }
      if (!is.null(input$f2_group2_levels)) {
        shiny$updateSelectizeInput(
          session,
          "f2_group2_levels",
          selected = character(0)
        )
      }
    }

    # --- Proactive clearing when levels drop below 3 or factor changes ---
    shiny$observeEvent(factor1_levels_reactive(), {
      lvls <- factor1_levels_reactive()
      if (is.null(lvls) || length(lvls) < 3) {
        clear_factor1_collapse()
      }
    })

    shiny$observeEvent(factor2_levels_reactive(), {
      lvls <- factor2_levels_reactive()
      if (is.null(lvls) || length(lvls) < 3) {
        clear_factor2_collapse()
      }
    })

    shiny$observeEvent(input$factor1_choice, {
      # Clear on any change (switches or becomes None)
      clear_factor1_collapse()
    })

    shiny$observeEvent(input$factor2_choice, {
      clear_factor2_collapse()
    })

    # Containers: show collapse sections only when factor selected and has >= 2 levels
    output$collapse_factor1_container <- shiny$renderUI({
      ns <- session$ns
      df <- current_data()
      f1 <- input$factor1_choice
      if (is.null(df) || is.null(f1) || f1 == "") {
        return(NULL)
      }
      lvls <- tryCatch(factor1_levels_reactive(), error = function(...) NULL)
      if (is.null(lvls) || length(lvls) < 3) {
        return(NULL)
      }
      shiny$tagList(
        shiny$h5("Factor Level Collapsing (for Factor 1 if selected):"),
        shiny$checkboxInput(
          ns("enable_collapse_factor1"),
          "Enable Collapsing for Factor 1?",
          value = FALSE
        ),
        shiny$uiOutput(ns("collapse_factor1_ui")),
        shiny$hr()
      )
    })

    output$collapse_factor2_container <- shiny$renderUI({
      ns <- session$ns
      df <- current_data()
      f2 <- input$factor2_choice
      if (is.null(df) || is.null(f2) || f2 == "") {
        return(NULL)
      }
      lvls <- tryCatch(factor2_levels_reactive(), error = function(...) NULL)
      if (is.null(lvls) || length(lvls) < 3) {
        return(NULL)
      }
      shiny$tagList(
        shiny$h5("Factor Level Collapsing (for Factor 2 if selected):"),
        shiny$checkboxInput(
          ns("enable_collapse_factor2"),
          "Enable Collapsing for Factor 2?",
          value = FALSE
        ),
        shiny$uiOutput(ns("collapse_factor2_ui")),
        shiny$hr()
      )
    })

    # =========================================================================
    # Factor 1 Collapse UI - Separate toggles for Q0 and Alpha
    # =========================================================================
    # This design uses explicit checkboxes to opt-in to collapse for each

    # parameter, avoiding NULL input issues from hidden conditionalPanel elements.
    output$collapse_factor1_ui <- shiny$renderUI({
      ns <- session$ns
      shiny$req(
        input$enable_collapse_factor1,
        input$factor1_choice != "None",
        input$factor1_choice != ""
      )
      all_levels <- factor1_levels_reactive()
      shiny$req(all_levels)

      # Helper to create collapse group inputs for a parameter
      make_collapse_inputs <- function(prefix, param_label) {
        shiny$tagList(
          shiny$div(
            style = "background: rgba(255,255,255,0.05); padding: 10px; border-radius: 5px; margin-top: 10px;",
            shiny$h6(
              paste(param_label, "Collapse Groups:"),
              style = "font-weight: bold; margin-bottom: 10px;"
            ),
            shiny$textInput(
              ns(paste0(prefix, "_group1_name")),
              "Group 1 Name:",
              placeholder = "e.g., Low"
            ),
            shiny$selectizeInput(
              ns(paste0(prefix, "_group1_levels")),
              "Levels for Group 1:",
              choices = all_levels,
              multiple = TRUE
            ),
            shiny$textInput(
              ns(paste0(prefix, "_group2_name")),
              "Group 2 Name:",
              placeholder = "e.g., High"
            ),
            shiny$selectizeInput(
              ns(paste0(prefix, "_group2_levels")),
              "Levels for Group 2:",
              choices = all_levels,
              multiple = TRUE
            )
          )
        )
      }

      shiny$tagList(
        shiny$p(
          shiny$strong(paste("Collapsing levels for:", input$factor1_choice)),
          style = "margin-bottom: 10px;"
        ),
        # Separate checkboxes for Q0 and Alpha collapse
        shiny$checkboxInput(
          ns("f1_collapse_q0"),
          shiny$span("Collapse levels for Q0", style = "font-weight: 500;"),
          value = FALSE
        ),
        shiny$conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("f1_collapse_q0")),
          make_collapse_inputs("f1_q0", "Q0")
        ),
        shiny$checkboxInput(
          ns("f1_collapse_alpha"),
          shiny$span("Collapse levels for Alpha", style = "font-weight: 500;"),
          value = FALSE
        ),
        shiny$conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("f1_collapse_alpha")),
          make_collapse_inputs("f1_alpha", "Alpha")
        )
      )
    })

    # =========================================================================
    # Factor 1 Overlap Prevention Observers
    # =========================================================================
    # These observers prevent selecting the same level in both Group 1 and Group 2
    # by updating the available choices when one group's selection changes.

    # Helper function to create overlap prevention observer
    # (Defined inline to keep related code together)
    make_overlap_observer <- function(
      input_g1_id,
      input_g2_id,
      update_g2_id,
      levels_reactive,
      enable_input_id
    ) {
      shiny$observeEvent(
        input[[input_g1_id]],
        {
          all_levels <- levels_reactive()
          selected_g1 <- input[[input_g1_id]] %||% character(0)
          shiny$req(input[[enable_input_id]], all_levels)

          current_selected_g2 <- shiny$isolate(
            input[[input_g2_id]] %||% character(0)
          )
          available_for_g2 <- setdiff(all_levels, selected_g1)
          new_selected_g2 <- intersect(current_selected_g2, available_for_g2)

          shiny$updateSelectizeInput(
            session,
            update_g2_id,
            choices = available_for_g2,
            selected = new_selected_g2
          )
        },
        ignoreNULL = FALSE,
        ignoreInit = TRUE
      )
    }

    # Factor 1 Q0 collapse: Group 1 selection updates Group 2 choices
    make_overlap_observer(
      "f1_q0_group1_levels",
      "f1_q0_group2_levels",
      "f1_q0_group2_levels",
      factor1_levels_reactive,
      "enable_collapse_factor1"
    )

    # Factor 1 Alpha collapse: Group 1 selection updates Group 2 choices
    make_overlap_observer(
      "f1_alpha_group1_levels",
      "f1_alpha_group2_levels",
      "f1_alpha_group2_levels",
      factor1_levels_reactive,
      "enable_collapse_factor1"
    )

    # =========================================================================
    # Factor 2 Collapse UI - Separate toggles for Q0 and Alpha (mirrors Factor 1)
    # =========================================================================
    output$collapse_factor2_ui <- shiny$renderUI({
      ns <- session$ns
      shiny$req(
        input$enable_collapse_factor2,
        input$factor2_choice != "None",
        input$factor2_choice != ""
      )
      all_levels <- factor2_levels_reactive()
      shiny$req(all_levels)

      # Helper to create collapse group inputs for a parameter
      make_collapse_inputs <- function(prefix, param_label) {
        shiny$tagList(
          shiny$div(
            style = "background: rgba(255,255,255,0.05); padding: 10px; border-radius: 5px; margin-top: 10px;",
            shiny$h6(
              paste(param_label, "Collapse Groups:"),
              style = "font-weight: bold; margin-bottom: 10px;"
            ),
            shiny$textInput(
              ns(paste0(prefix, "_group1_name")),
              "Group 1 Name:",
              placeholder = "e.g., Low"
            ),
            shiny$selectizeInput(
              ns(paste0(prefix, "_group1_levels")),
              "Levels for Group 1:",
              choices = all_levels,
              multiple = TRUE
            ),
            shiny$textInput(
              ns(paste0(prefix, "_group2_name")),
              "Group 2 Name:",
              placeholder = "e.g., High"
            ),
            shiny$selectizeInput(
              ns(paste0(prefix, "_group2_levels")),
              "Levels for Group 2:",
              choices = all_levels,
              multiple = TRUE
            )
          )
        )
      }

      shiny$tagList(
        shiny$p(
          shiny$strong(paste("Collapsing levels for:", input$factor2_choice)),
          style = "margin-bottom: 10px;"
        ),
        # Separate checkboxes for Q0 and Alpha collapse
        shiny$checkboxInput(
          ns("f2_collapse_q0"),
          shiny$span("Collapse levels for Q0", style = "font-weight: 500;"),
          value = FALSE
        ),
        shiny$conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("f2_collapse_q0")),
          make_collapse_inputs("f2_q0", "Q0")
        ),
        shiny$checkboxInput(
          ns("f2_collapse_alpha"),
          shiny$span("Collapse levels for Alpha", style = "font-weight: 500;"),
          value = FALSE
        ),
        shiny$conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("f2_collapse_alpha")),
          make_collapse_inputs("f2_alpha", "Alpha")
        )
      )
    })

    # =========================================================================
    # Factor 2 Overlap Prevention Observers (using same helper as Factor 1)
    # =========================================================================

    # Factor 2 Q0 collapse: Group 1 selection updates Group 2 choices
    make_overlap_observer(
      "f2_q0_group1_levels",
      "f2_q0_group2_levels",
      "f2_q0_group2_levels",
      factor2_levels_reactive,
      "enable_collapse_factor2"
    )

    # Factor 2 Alpha collapse: Group 1 selection updates Group 2 choices
    make_overlap_observer(
      "f2_alpha_group1_levels",
      "f2_alpha_group2_levels",
      "f2_alpha_group2_levels",
      factor2_levels_reactive,
      "enable_collapse_factor2"
    )

    # =========================================================================
    # Collapse Levels Reactive
    # =========================================================================
    # Processes user's collapse UI inputs into the nested list format required
    # by beezdemand::fit_demand_mixed.
    #
    # NEW DESIGN: Uses explicit checkboxes (f1_collapse_q0, f1_collapse_alpha, etc.)

    # to opt-in to collapse for each parameter. This avoids NULL input issues
    # from hidden conditionalPanel elements.
    #
    # Output format: list(Q0 = list(factor = list(...)), alpha = list(factor = list(...)))

    parsed_collapse_levels_reactive <- shiny$reactive({
      # -----------------------------------------------------------------------
      # Helper: Process collapse using extracted module, with notification
      # -----------------------------------------------------------------------
      process_collapse_with_notification <- function(
        collapse_enabled,
        factor_name,
        g1_name,
        g1_levels,
        g2_name,
        g2_levels,
        param_label
      ) {
        result <- collapse_levels$process_param_collapse(
          collapse_enabled = collapse_enabled,
          g1_name = g1_name,
          g1_levels = g1_levels,
          g2_name = g2_name,
          g2_levels = g2_levels,
          factor_name = factor_name,
          param_label = param_label
        )

        # Show notification if there was an error
        if (!is.null(result$error)) {
          shiny$showNotification(
            result$error$error_message,
            type = "error",
            duration = 7
          )
          return("ERROR_OVERLAP")
        }

        result$collapse
      }

      # -----------------------------------------------------------------------
      # Process Factor 1
      # -----------------------------------------------------------------------
      f1_name <- input$factor1_choice
      f1_enabled <- isTRUE(input$enable_collapse_factor1) &&
        !is.null(f1_name) &&
        !f1_name %in% c("None", "")

      f1_q0 <- NULL
      f1_alpha <- NULL

      if (f1_enabled) {
        # Process Q0 collapse for Factor 1 using module
        f1_q0 <- process_collapse_with_notification(
          collapse_enabled = input$f1_collapse_q0,
          factor_name = f1_name,
          g1_name = trimws(input$f1_q0_group1_name %||% ""),
          g1_levels = input$f1_q0_group1_levels,
          g2_name = trimws(input$f1_q0_group2_name %||% ""),
          g2_levels = input$f1_q0_group2_levels,
          param_label = "Q0"
        )
        if (identical(f1_q0, "ERROR_OVERLAP")) {
          return(f1_q0)
        }

        # Process Alpha collapse for Factor 1 using module
        f1_alpha <- process_collapse_with_notification(
          collapse_enabled = input$f1_collapse_alpha,
          factor_name = f1_name,
          g1_name = trimws(input$f1_alpha_group1_name %||% ""),
          g1_levels = input$f1_alpha_group1_levels,
          g2_name = trimws(input$f1_alpha_group2_name %||% ""),
          g2_levels = input$f1_alpha_group2_levels,
          param_label = "Alpha"
        )
        if (identical(f1_alpha, "ERROR_OVERLAP")) return(f1_alpha)
      }

      # -----------------------------------------------------------------------
      # Process Factor 2
      # -----------------------------------------------------------------------
      f2_name <- input$factor2_choice
      f2_enabled <- isTRUE(input$enable_collapse_factor2) &&
        !is.null(f2_name) &&
        !f2_name %in% c("None", "")

      f2_q0 <- NULL
      f2_alpha <- NULL

      if (f2_enabled) {
        # Process Q0 collapse for Factor 2 using module
        f2_q0 <- process_collapse_with_notification(
          collapse_enabled = input$f2_collapse_q0,
          factor_name = f2_name,
          g1_name = trimws(input$f2_q0_group1_name %||% ""),
          g1_levels = input$f2_q0_group1_levels,
          g2_name = trimws(input$f2_q0_group2_name %||% ""),
          g2_levels = input$f2_q0_group2_levels,
          param_label = "Q0"
        )
        if (identical(f2_q0, "ERROR_OVERLAP")) {
          return(f2_q0)
        }

        # Process Alpha collapse for Factor 2 using module
        f2_alpha <- process_collapse_with_notification(
          collapse_enabled = input$f2_collapse_alpha,
          factor_name = f2_name,
          g1_name = trimws(input$f2_alpha_group1_name %||% ""),
          g1_levels = input$f2_alpha_group1_levels,
          g2_name = trimws(input$f2_alpha_group2_name %||% ""),
          g2_levels = input$f2_alpha_group2_levels,
          param_label = "Alpha"
        )
        if (identical(f2_alpha, "ERROR_OVERLAP")) return(f2_alpha)
      }

      # -----------------------------------------------------------------------
      # Build final structure for beezdemand
      # -----------------------------------------------------------------------
      q0_collapse <- list()
      alpha_collapse <- list()

      if (!is.null(f1_q0) && f1_enabled) {
        q0_collapse[[f1_name]] <- f1_q0
      }
      if (!is.null(f2_q0) && f2_enabled) {
        q0_collapse[[f2_name]] <- f2_q0
      }
      if (!is.null(f1_alpha) && f1_enabled) {
        alpha_collapse[[f1_name]] <- f1_alpha
      }
      if (!is.null(f2_alpha) && f2_enabled) {
        alpha_collapse[[f2_name]] <- f2_alpha
      }

      # Return NULL if nothing to collapse
      if (length(q0_collapse) == 0 && length(alpha_collapse) == 0) {
        return(NULL)
      }

      # Build final list
      final_list <- list()
      if (length(q0_collapse) > 0) {
        final_list$Q0 <- q0_collapse
      }
      if (length(alpha_collapse) > 0) {
        final_list$alpha <- alpha_collapse
      }

      final_list
    })

    # --- Advanced fitting controls: presets, clamping, and reactive export ---
    # Use extracted model_fitting module for preset values
    apply_preset <- function(preset) {
      vals <- model_fitting$build_nlme_control(preset = preset)
      shiny$updateNumericInput(session, "maxIter", value = vals$maxIter)
      shiny$updateNumericInput(session, "pnlsMaxIter", value = vals$pnlsMaxIter)
      shiny$updateNumericInput(session, "msMaxIter", value = vals$msMaxIter)
      shiny$updateNumericInput(session, "tolerance", value = vals$tolerance)
      shiny$updateNumericInput(session, "pnlsTol", value = vals$pnlsTol)
      shiny$updateNumericInput(session, "minScale", value = vals$minScale)
      shiny$updateNumericInput(session, "niterEM", value = vals$niterEM)
    }

    shiny$observeEvent(
      input$fit_presets,
      {
        shiny$req(input$fit_presets)
        apply_preset(input$fit_presets)
      },
      ignoreInit = TRUE
    )

    shiny$observeEvent(input$fit_reset_defaults, {
      apply_preset("Balanced")
      shiny$updateSelectInput(session, "fit_presets", selected = "Balanced")
    })

    clamp_num <- function(x, lo, hi) {
      x <- suppressWarnings(as.numeric(x))
      if (is.na(x)) {
        return(NA_real_)
      }
      pmin(pmax(x, lo), hi)
    }

    nlme_controls_reactive <- shiny$reactive({
      list(
        maxIter = clamp_num(input$maxIter, 30, 300),
        pnlsMaxIter = clamp_num(input$pnlsMaxIter, 7, 50),
        msMaxIter = clamp_num(input$msMaxIter, 50, 500),
        tolerance = clamp_num(input$tolerance, 1e-7, 1e-4),
        pnlsTol = clamp_num(input$pnlsTol, 1e-4, 1e-2),
        minScale = clamp_num(input$minScale, 1e-4, 1e-2),
        niterEM = clamp_num(input$niterEM, 0, 100)
      )
    })

    # Precompute basic covariate stats (global center/scale) with numeric-like coercion
    covariate_stats <- shiny$reactive({
      df <- current_data()
      covar <- input$covariate_choice
      if (
        is.null(df) ||
          is.null(covar) ||
          !nzchar(covar) ||
          !(covar %in% names(df))
      ) {
        return(NULL)
      }
      x <- df[[covar]]
      if (is.character(x)) {
        x <- suppressWarnings(as.numeric(x))
      }
      mu <- suppressWarnings(mean(x, na.rm = TRUE))
      sigma <- suppressWarnings(stats$sd(x, na.rm = TRUE))
      if (!is.finite(sigma) || is.na(sigma)) {
        sigma <- NA_real_
      }
      list(name = covar, mu = mu, sigma = sigma)
    })

    # Helper: build transformed covariate column using extracted module
    build_covariate_modeling_info <- function(df_in) {
      covar <- input$covariate_choice
      if (is.null(covar) || !nzchar(covar) || !(covar %in% names(df_in))) {
        return(list(df = df_in, model_covariate_name = NULL, at_list = NULL))
      }

      result <- model_fitting$process_covariate(
        df = df_in,
        covariate_col = covar,
        center = isTRUE(input$cov_center),
        scale = isTRUE(input$cov_scale),
        at_value = input$cov_at
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

    # Log user interactions with key model parameters
    shiny$observeEvent(
      input$model_choice,
      {
        if (!is.null(input$model_choice)) {
          session_logger$user_activity(
            action = "Model type selected",
            input_id = "model_choice",
            input_value = input$model_choice,
            module = "mixed_effects_demand"
          )
        }
      },
      ignoreInit = TRUE
    )

    shiny$observeEvent(
      input$run_mixed_model,
      {
        session_logger$user_activity(
          action = "Mixed effects model run initiated",
          input_id = "run_mixed_model",
          module = "mixed_effects_demand"
        )

        # Log model configuration
        session_logger$model_fitting(
          model_type = "mixed_effects_demand",
          parameters = list(
            model_choice = input$model_choice,
            id_var = input$id_variable_choice,
            x_var = input$x_variable_choice,
            y_var = input$y_variable_choice,
            factors = selected_factors_reactive(),
            random_effects = input$random_effects_spec,
            covariance_structure = input$covariance_structure
          ),
          status = "started"
        )
      },
      ignoreInit = TRUE
    )

    return(
      list(
        run_trigger = shiny$reactive(input$run_mixed_model),
        data_to_analyze_trigger = current_data,
        id_var = shiny$reactive(input$id_variable_choice),
        x_var = shiny$reactive(input$x_variable_choice),
        y_var = shiny$reactive(input$y_variable_choice),
        selected_factors = selected_factors_reactive,
        factor_interaction = shiny$reactive(input$factor_interaction),
        random_effects_spec = shiny$reactive(input$random_effects_spec),
        covariance_structure = shiny$reactive(input$covariance_structure),
        equation_form = shiny$reactive(input$model_choice),
        collapse_levels_reactive = parsed_collapse_levels_reactive,
        nlme_controls = nlme_controls_reactive,
        # Covariate controls (Phase 1 only; not used in fitting yet)
        covariate = shiny$reactive(input$covariate_choice),
        cov_center = shiny$reactive(isTRUE(input$cov_center)),
        cov_scale = shiny$reactive(isTRUE(input$cov_scale)),
        cov_at_natural = shiny$reactive(input$cov_at),
        cov_stats = covariate_stats
      )
    )
  })
}
