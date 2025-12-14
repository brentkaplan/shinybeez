# # app/view/mixed_effects_demand.R

box::use(
  bsicons,
  bslib,
  shiny,
  DT,
  esquisse,
  htmltools,
  ggplot2, # For plot customization if needed beyond esquisse
  ggprism, # For GraphPad Prism themes
  openxlsx, # For Excel export
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
  app / logic / validate, # For data validation and transformations
  app / logic / mixed_effects_demand_utils,
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
    # shiny$selectInput(
    #   ns("y_trans_choice"),
    #   label = "Select Y transformation:",
    #   choices = list(
    #     "None" = "none",
    #     "LL4" = "ll4"
    #   ),
    #   selected = "none"
    # ),
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
        "Sets the value at which the continuous covariate is held constant when calculating Estimated Marginal Means (EMMs), pairwise comparisons, and drawing prediction lines in the plot.\n\nEnter this on the natural scale = the raw units of your data column (before any centering/scaling). If 'Center' and/or 'Scale' are checked, this natural-scale value is internally transformed using the column's global mean/SD to condition the model."
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
          "Tip: Start with Balanced. If PNLS stalls, raise pnlsMaxIter to 20–30 and tighten pnlsTol to 1e-4; if the outer loop stops early, try maxIter 150–200."
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
            "Minimum step shrink for PNLS backtracking. Lower (1e-4) for finer steps if overshooting; higher (1e-2) for speed."
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

    # Helper for robust, case-insensitive guessing that returns original names
    guess_first_match <- function(candidates, cols) {
      cols_lower <- tolower(cols)
      for (cand in candidates) {
        idx <- which(cols_lower == cand)
        if (length(idx) > 0) {
          return(cols[idx[1]])
        }
      }
      return(NA_character_)
    }

    # A. Update ID/X/Y choices ONLY when data changes
    shiny$observeEvent(
      current_data(),
      {
        df <- current_data()
        shiny$req(df)
        col_names <- names(df)
        cols_lower <- tolower(col_names)

        likely_id <- guess_first_match(
          c(
            "monkey",
            "id",
            "subject",
            "participant",
            "subjid",
            "subj_id",
            "responseid"
          ),
          col_names
        )
        if (is.na(likely_id)) {
          likely_id <- col_names[1]
        }

        likely_x <- guess_first_match(
          c("x", "price", "ratio"),
          col_names
        )
        if (is.na(likely_x) && length(col_names) >= 2) {
          likely_x <- col_names[2]
        }

        likely_y <- guess_first_match(
          c("y", "y_ll4", "consumption", "response"),
          col_names
        )
        if (is.na(likely_y) && length(col_names) >= 3) {
          likely_y <- col_names[3]
        }

        # On data change, initialize sensible defaults (do not preserve prior selections)
        id_selected <- likely_id
        x_selected <- likely_x
        y_selected <- likely_y

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
      # Helper: Build collapse mapping from group name/level inputs
      # -----------------------------------------------------------------------
      build_collapse_list <- function(g1_name, g1_levels, g2_name, g2_levels) {
        collapse_list <- list()
        if (!is.null(g1_name) && nzchar(g1_name) && length(g1_levels) > 0) {
          collapse_list[[g1_name]] <- g1_levels
        }
        if (!is.null(g2_name) && nzchar(g2_name) && length(g2_levels) > 0) {
          collapse_list[[g2_name]] <- g2_levels
        }
        if (length(collapse_list) == 0) {
          return(NULL)
        }
        collapse_list
      }

      # -----------------------------------------------------------------------
      # Helper: Validate no overlap between group levels
      # -----------------------------------------------------------------------
      validate_no_overlap <- function(
        g1_levels,
        g2_levels,
        factor_name,
        param_label
      ) {
        if (length(g1_levels) > 0 && length(g2_levels) > 0) {
          overlap <- intersect(g1_levels, g2_levels)
          if (length(overlap) > 0) {
            shiny$showNotification(
              paste0(
                "Overlap in ",
                param_label,
                " collapse for '",
                factor_name,
                "': ",
                paste(overlap, collapse = ", ")
              ),
              type = "error",
              duration = 7
            )
            return("ERROR_OVERLAP")
          }
        }
        NULL
      }

      # -----------------------------------------------------------------------
      # Helper: Process collapse for one parameter (Q0 or Alpha) of one factor
      # -----------------------------------------------------------------------
      # Returns: collapse mapping list, or NULL if not enabled/empty
      process_param_collapse <- function(
        collapse_enabled,
        factor_name,
        g1_name,
        g1_levels,
        g2_name,
        g2_levels,
        param_label
      ) {
        # Guard: Only process if explicitly enabled via checkbox
        if (!isTRUE(collapse_enabled)) {
          return(NULL)
        }

        # Validate no overlap
        err <- validate_no_overlap(
          g1_levels,
          g2_levels,
          factor_name,
          param_label
        )
        if (!is.null(err)) {
          return(err)
        }

        # Build and return the collapse mapping
        build_collapse_list(g1_name, g1_levels, g2_name, g2_levels)
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
        # Process Q0 collapse for Factor 1
        f1_q0 <- process_param_collapse(
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

        # Process Alpha collapse for Factor 1
        f1_alpha <- process_param_collapse(
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
        # Process Q0 collapse for Factor 2
        f2_q0 <- process_param_collapse(
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

        # Process Alpha collapse for Factor 2
        f2_alpha <- process_param_collapse(
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
    balanced_defaults <- list(
      maxIter = 100L,
      pnlsMaxIter = 7L,
      msMaxIter = 100L,
      tolerance = 1e-3,
      pnlsTol = 1e-3,
      minScale = 1e-3,
      niterEM = 50L
    )

    apply_preset <- function(preset) {
      vals <- switch(
        preset,
        "Faster" = list(
          maxIter = 60L,
          pnlsMaxIter = 10L,
          msMaxIter = 80L,
          tolerance = 3e-3,
          pnlsTol = 3e-3,
          minScale = 5e-3,
          niterEM = 30L
        ),
        "Stricter" = list(
          maxIter = 200L,
          pnlsMaxIter = 30L,
          msMaxIter = 200L,
          tolerance = 1e-5,
          pnlsTol = 1e-4,
          minScale = 1e-4,
          niterEM = 80L
        ),
        balanced_defaults
      )
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

    # Helper: build transformed covariate column and compute 'at' list for downstream APIs
    build_covariate_modeling_info <- function(df_in) {
      df <- df_in
      covar <- input$covariate_choice
      if (is.null(covar) || !nzchar(covar) || !(covar %in% names(df))) {
        return(list(df = df, model_covariate_name = NULL, at_list = NULL))
      }

      # Ensure numeric base column
      base_name <- covar
      base_vec <- df[[covar]]
      if (is.character(base_vec)) {
        base_name_num <- paste0(covar, "_num")
        df[[base_name_num]] <- suppressWarnings(as.numeric(base_vec))
        base_name <- base_name_num
      }

      center <- isTRUE(input$cov_center)
      scale <- isTRUE(input$cov_scale)
      x <- df[[base_name]]
      mu <- suppressWarnings(mean(x, na.rm = TRUE))
      sigma <- suppressWarnings(stats$sd(x, na.rm = TRUE))

      effective_name <- base_name
      if (center || scale) {
        if (
          isTRUE(scale) && (is.na(sigma) || !is.finite(sigma) || sigma <= 0)
        ) {
          shiny$showNotification(
            "Scale requested but SD is not positive; applying centering only.",
            type = "warning"
          )
          scale <- FALSE
        }
        if (isTRUE(scale)) {
          effective_name <- paste0(base_name, "_cs")
          df[[effective_name]] <- (x - mu) / sigma
        } else if (isTRUE(center)) {
          effective_name <- paste0(base_name, "_c")
          df[[effective_name]] <- (x - mu)
        }
      } else {
        # If not transforming and base is numeric-like character, column already created
        if (!(base_name %in% names(df))) df[[base_name]] <- x
      }

      # Build 'at' list using natural-scale input, transformed consistently
      at_val_nat <- suppressWarnings(as.numeric(input$cov_at))
      at_list <- NULL
      if (!is.null(at_val_nat) && is.finite(at_val_nat)) {
        at_val <- at_val_nat
        if (center || scale) {
          if (isTRUE(scale) && is.finite(sigma) && sigma > 0) {
            at_val <- (at_val_nat - mu) / sigma
          } else if (isTRUE(center)) {
            at_val <- (at_val_nat - mu)
          }
        }
        at_list <- list(at_val)
        names(at_list) <- effective_name
      }

      list(df = df, model_covariate_name = effective_name, at_list = at_list)
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
        # y_transform = shiny$reactive(input$y_trans_choice),
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
                "Show ShinyBeez Watermark",
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

    # Helper: build transformed covariate column and compute 'at' list
    # Uses covariate controls from sidebar_reactives so the function is local to this module
    build_covariate_modeling_info <- function(df_in) {
      df <- df_in
      covar <- sidebar_reactives$covariate()
      if (is.null(covar) || !nzchar(covar) || !(covar %in% names(df))) {
        return(list(df = df, model_covariate_name = NULL, at_list = NULL))
      }

      # Ensure numeric base column (coerce character numeric-like to numeric)
      base_name <- covar
      base_vec <- df[[covar]]
      if (is.character(base_vec)) {
        base_name_num <- paste0(covar, "_num")
        df[[base_name_num]] <- suppressWarnings(as.numeric(base_vec))
        base_name <- base_name_num
      }

      center <- isTRUE(sidebar_reactives$cov_center())
      scale <- isTRUE(sidebar_reactives$cov_scale())
      x <- df[[base_name]]
      mu <- suppressWarnings(mean(x, na.rm = TRUE))
      sigma <- suppressWarnings(stats$sd(x, na.rm = TRUE))

      effective_name <- base_name
      if (center || scale) {
        if (
          isTRUE(scale) && (is.na(sigma) || !is.finite(sigma) || sigma <= 0)
        ) {
          shiny$showNotification(
            "Scale requested but SD is not positive; applying centering only.",
            type = "warning"
          )
          scale <- FALSE
        }
        if (isTRUE(scale)) {
          effective_name <- paste0(base_name, "_cs")
          df[[effective_name]] <- (x - mu) / sigma
        } else if (isTRUE(center)) {
          effective_name <- paste0(base_name, "_c")
          df[[effective_name]] <- (x - mu)
        }
      } else {
        if (!(base_name %in% names(df))) df[[base_name]] <- x
      }

      # Build 'at' list using the natural-scale "at" value transformed consistently
      at_val_nat <- suppressWarnings(as.numeric(sidebar_reactives$cov_at_natural()))
      at_list <- NULL
      if (!is.null(at_val_nat) && is.finite(at_val_nat)) {
        at_val <- at_val_nat
        if (center || scale) {
          if (isTRUE(scale) && is.finite(sigma) && sigma > 0) {
            at_val <- (at_val_nat - mu) / sigma
          } else if (isTRUE(center)) {
            at_val <- (at_val_nat - mu)
          }
        }
        at_list <- list(at_val)
        names(at_list) <- effective_name
      }

      list(df = df, model_covariate_name = effective_name, at_list = at_list)
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

      # y_var_col_name <- sidebar_reactives$y_var()
      # y_transform_method <- sidebar_reactives$y_transform()
      # shiny$req(y_var_col_name, y_transform_method)
      # if (y_transform_method == "ll4") {
      #   if (y_var_col_name %in% names(df)) {
      #     # Create a new column for the transformed Y, or use y_ll4 if it's already the target
      #     # and the selected y_var IS y_ll4 (avoiding re-transforming y_ll4)
      #     if (y_var_col_name == "y_ll4" && "y_ll4" %in% names(df)) {
      #       df[["y_for_model"]] <- df[[y_var_col_name]]
      #       shiny$showNotification(
      #         paste(
      #           "Using existing column '",
      #           y_var_col_name,
      #           "' for y_for_model (LL4)."
      #         ),
      #         type = "message",
      #         duration = 3
      #       )
      #     } else {
      #       shiny$showNotification(
      #         paste(
      #           "Applying LL4 transformation to '",
      #           y_var_col_name,
      #           "' and using as y_for_model."
      #         ),
      #         type = "message",
      #         duration = 3
      #       )
      #       df[["y_for_model"]] <- beezdemand$ll4(df[[y_var_col_name]])
      #     }
      #   } else {
      #     shiny$showNotification(
      #       paste(
      #         "Selected Y variable '",
      #         y_var_col_name,
      #         "' not found for transformation."
      #       ),
      #       type = "error"
      #     )
      #     return(NULL)
      #   }
      # } else {
      #   # "none" transformation
      #   # If no transformation, y_for_model is just the selected y_variable_choice
      #   if (y_var_col_name %in% names(df)) {
      #     df[["y_for_model"]] <- df[[y_var_col_name]]
      #     shiny$showNotification(
      #       paste(
      #         "Using existing column '",
      #         y_var_col_name,
      #         "' and using as y_for_model."
      #       ),
      #       type = "message",
      #       duration = 3
      #     )
      #   } else {
      #     shiny$showNotification(
      #       paste("Selected Y variable '", y_var_col_name, "' not found."),
      #       type = "error"
      #     )
      #     return(NULL)
      #   }
      # }

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
          buttons = list(
            list(extend = "copy"),
            list(extend = "print"),
            list(
              extend = "csv",
              filename = "ShinyBeez_MixedEffects_Input_Data",
              title = NULL
            ),
            list(
              extend = "excel",
              filename = "ShinyBeez_MixedEffects_Input_Data",
              title = NULL
            ),
            list(
              extend = "pdf",
              filename = "ShinyBeez_MixedEffects_Input_Data",
              title = NULL
            )
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
          buttons = list(
            list(extend = "copy"),
            list(extend = "print"),
            list(
              extend = "csv",
              filename = "ShinyBeez_MixedEffects_Descriptives",
              title = NULL
            ),
            list(
              extend = "excel",
              filename = "ShinyBeez_MixedEffects_Descriptives",
              title = NULL
            ),
            list(
              extend = "pdf",
              filename = "ShinyBeez_MixedEffects_Descriptives",
              title = NULL
            )
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
            buttons = list(
              list(extend = "copy"),
              list(extend = "print"),
              list(
                extend = "csv",
                filename = "ShinyBeez_MixedEffects_Systematic_Criteria",
                title = NULL
              ),
              list(
                extend = "excel",
                filename = "ShinyBeez_MixedEffects_Systematic_Criteria",
                title = NULL
              ),
              list(
                extend = "pdf",
                filename = "ShinyBeez_MixedEffects_Systematic_Criteria",
                title = NULL
              )
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

        # y_var_actual <- if (
        #   sidebar_reactives$y_var() == "y_transform" &&
        #     !("y_ll4" %in% names(df))
        # ) {
        #   shiny$req("y_ll4" %in% names(df)) # ensure transformation happened
        #   "y_ll4"
        # } else {
        #   "y" # Defaulting to y_ll4 as per ZBEn
        # }

        # if (!(y_var_actual %in% names(df))) {
        #   shiny$showNotification(
        #     paste(
        #       "Required Y variable '",
        #       y_var_actual,
        #       "' not found in the data."
        #     ),
        #     type = "error"
        #   )
        #   return(NULL)
        # }

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
          buttons = list(
            list(extend = "copy"),
            list(extend = "print"),
            list(
              extend = "csv",
              filename = "ShinyBeez_MixedEffects_Fixed_Effects",
              title = NULL
            ),
            list(
              extend = "excel",
              filename = "ShinyBeez_MixedEffects_Fixed_Effects",
              title = NULL
            ),
            list(
              extend = "pdf",
              filename = "ShinyBeez_MixedEffects_Fixed_Effects",
              title = NULL
            )
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
            buttons = list(
              list(extend = "copy"),
              list(extend = "print"),
              list(
                extend = "csv",
                filename = "ShinyBeez_MixedEffects_Individual_Coefficients",
                title = NULL
              ),
              list(
                extend = "excel",
                filename = "ShinyBeez_MixedEffects_Individual_Coefficients",
                title = NULL
              ),
              list(
                extend = "pdf",
                filename = "ShinyBeez_MixedEffects_Individual_Coefficients",
                title = NULL
              )
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
          buttons = list(
            list(extend = "copy"),
            list(extend = "print"),
            list(
              extend = "csv",
              filename = "ShinyBeez_MixedEffects_Random_Effects",
              title = NULL
            ),
            list(
              extend = "excel",
              filename = "ShinyBeez_MixedEffects_Random_Effects",
              title = NULL
            ),
            list(
              extend = "pdf",
              filename = "ShinyBeez_MixedEffects_Random_Effects",
              title = NULL
            )
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
          buttons = list(
            list(extend = "copy"),
            list(extend = "print"),
            list(
              extend = "csv",
              filename = "ShinyBeez_MixedEffects_Q0_EMMs",
              title = NULL
            ),
            list(
              extend = "excel",
              filename = "ShinyBeez_MixedEffects_Q0_EMMs",
              title = NULL
            ),
            list(
              extend = "pdf",
              filename = "ShinyBeez_MixedEffects_Q0_EMMs",
              title = NULL
            )
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
          buttons = list(
            list(extend = "copy"),
            list(extend = "print"),
            list(
              extend = "csv",
              filename = "ShinyBeez_MixedEffects_Alpha_EMMs",
              title = NULL
            ),
            list(
              extend = "excel",
              filename = "ShinyBeez_MixedEffects_Alpha_EMMs",
              title = NULL
            ),
            list(
              extend = "pdf",
              filename = "ShinyBeez_MixedEffects_Alpha_EMMs",
              title = NULL
            )
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
          buttons = list(
            list(extend = "copy"),
            list(extend = "print"),
            list(
              extend = "csv",
              filename = "ShinyBeez_MixedEffects_EV",
              title = NULL
            ),
            list(
              extend = "excel",
              filename = "ShinyBeez_MixedEffects_EV",
              title = NULL
            ),
            list(
              extend = "pdf",
              filename = "ShinyBeez_MixedEffects_EV",
              title = NULL
            )
          )
        )
      )
    })

    # Populate factor selectors for comparisons
    # Use the model's actual factors (handles asymmetric collapse)
    shiny$observe({
      model_fit <- fitted_model_reactive()
      shiny$req(model_fit, model_fit$model)

      # Get factors from the fitted model - source of truth for comparisons
      model_factors <- model_fit$param_info$factors
      if (is.null(model_factors)) {
        model_factors <- character(0)
      }

      output$comparison_factor_selector_ui <- shiny$renderUI({
        if (length(model_factors) == 0) {
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
        if (length(model_factors) <= 1) {
          return(NULL)
        }
        main_comparison_factor <- input$comparison_factor
        shiny$req(main_comparison_factor)

        other_factors <- setdiff(model_factors, main_comparison_factor)
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
      if (is.null(model_factors) || length(model_factors) == 0) {
        # Intercept-only model - return NULL (no comparisons possible)
        return(NULL)
      }

      # Require a valid comparison factor selection
      main_factor <- input$comparison_factor
      if (
        is.null(main_factor) ||
          !nzchar(main_factor) ||
          !(main_factor %in% model_factors)
      ) {
        return(NULL)
      }

      by_factor <- input$contrast_by_factor

      specs_str <- main_factor
      if (
        !is.null(by_factor) && nzchar(by_factor) && by_factor %in% model_factors
      ) {
        specs_str <- paste(main_factor, "*", by_factor)
      }

      # The factor to compare `by` should be the `contrast_by` argument
      # The primary factor for pairwise comparison is implicitly handled by `emmeans`
      # when `specs` defines the interaction.

      contrast_by_arg <- if (
        !is.null(by_factor) && nzchar(by_factor) && by_factor %in% model_factors
      ) {
        by_factor
      } else {
        NULL
      }

      # Build 'at' for covariate conditioning
      df_now <- data_to_analyze()
      cov_info <- build_covariate_modeling_info(df_now)

      comps <- tryCatch(
        beezdemand$get_demand_comparisons(
          fit_obj = model_fit,
          params_to_compare = c("Q0", "alpha"),
          compare_specs = stats$as.formula(paste("~", specs_str)),
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
      raw_data <- if (input$comparison_display_type == "ratio") {
        comps$Q0$contrasts_ratio
      } else {
        comps$Q0$contrasts_log10
      }

      # Handle empty or NULL data
      if (is.null(raw_data) || nrow(raw_data) == 0) {
        return(NULL)
      }

      display_data <- dplyr$mutate(
        raw_data,
        dplyr$across(where(is.numeric), ~ round(., 4))
      )

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
        extensions = c("Buttons"),
        options = list(
          scrollX = TRUE,
          pageLength = 5,
          autoWidth = TRUE,
          dom = "Btip",
          buttons = list(
            list(extend = "copy"),
            list(extend = "print"),
            list(
              extend = "csv",
              filename = "ShinyBeez_MixedEffects_Q0_Comparisons",
              title = NULL
            ),
            list(
              extend = "excel",
              filename = "ShinyBeez_MixedEffects_Q0_Comparisons",
              title = NULL
            ),
            list(
              extend = "pdf",
              filename = "ShinyBeez_MixedEffects_Q0_Comparisons",
              title = NULL
            )
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

      display_data <- if (input$comparison_display_type == "ratio") {
        comps$Q0$contrasts_ratio
      } else {
        comps$Q0$contrasts_log10
      }

      # Handle empty comparisons (intercept-only for Q0 due to collapse)
      if (is.null(display_data) || nrow(display_data) == 0) {
        return(shiny$tagList(
          shiny$h4("Q0 Comparisons"),
          shiny$helpText(
            shiny$em(
              "No Q0 comparisons available (Q0 may be intercept-only due to collapsed levels)."
            )
          )
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

      display_data <- if (input$comparison_display_type == "ratio") {
        comps$alpha$contrasts_ratio
      } else {
        comps$alpha$contrasts_log10
      }

      # Handle empty comparisons (intercept-only for alpha due to collapse)
      if (is.null(display_data) || nrow(display_data) == 0) {
        return(shiny$tagList(
          shiny$h4("Alpha Comparisons"),
          shiny$helpText(
            shiny$em(
              "No alpha comparisons available (alpha may be intercept-only due to collapsed levels)."
            )
          )
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
      raw_data <- if (input$comparison_display_type == "ratio") {
        comps$alpha$contrasts_ratio
      } else {
        comps$alpha$contrasts_log10
      }

      # Handle empty or NULL data
      if (is.null(raw_data) || nrow(raw_data) == 0) {
        return(NULL)
      }

      display_data <- dplyr$mutate(
        raw_data,
        dplyr$across(where(is.numeric), ~ round(., 4))
      )

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
        extensions = c("Buttons"),
        options = list(
          scrollX = TRUE,
          pageLength = 5,
          autoWidth = TRUE,
          dom = "Btip",
          buttons = list(
            list(extend = "copy"),
            list(extend = "print"),
            list(
              extend = "csv",
              filename = "ShinyBeez_MixedEffects_Alpha_Comparisons",
              title = NULL
            ),
            list(
              extend = "excel",
              filename = "ShinyBeez_MixedEffects_Alpha_Comparisons",
              title = NULL
            ),
            list(
              extend = "pdf",
              filename = "ShinyBeez_MixedEffects_Alpha_Comparisons",
              title = NULL
            )
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
      selected_color <- shiny$isolate(input$plot_color_by)
      if (!selected_color %in% factors_in_model) {
        selected_color <- ""
      }

      selected_linetype <- shiny$isolate(input$plot_linetype_by)
      if (!selected_linetype %in% factors_in_model) {
        selected_linetype <- ""
      }

      selected_facet <- shiny$isolate(input$plot_facet_by)
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
      # inv_transform_fun <- beezdemand$ll4_inv # Assuming ll4_inv is 10^x or
      # similar

      y_is_ll4 <- isTRUE(model_fit$param_info$y_is_ll4)
      inv_transform_fun <- if (y_is_ll4) beezdemand$ll4_inv else identity

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

      # Build 'at' for covariate conditioning in plot
      df_now <- data_to_analyze()
      cov_info <- build_covariate_modeling_info(df_now)

      p <- plot(
        model_fit,
        inv_fun = inv_transform_fun, # To get natural scale for predictions
        y_trans = current_y_trans, # Visual scale for y-axis
        x_trans = current_x_trans, # Visual scale for x-axis
        at = cov_info$at_list,
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
      )

      # Apply selected theme with user-specified font size
      font_size <- if (is.null(input$plot_font_size)) {
        14
      } else {
        input$plot_font_size
      }
      p <- switch(
        input$plot_theme,
        "prism" = p + ggprism$theme_prism(base_size = font_size),
        "classic" = p + ggplot2$theme_classic(base_size = font_size),
        "minimal" = p + ggplot2$theme_minimal(base_size = font_size),
        p # default: keep existing theme
      )

      # Apply legend position
      legend_pos <- if (is.null(input$plot_legend_position)) {
        "right"
      } else {
        input$plot_legend_position
      }
      p <- p + ggplot2$theme(legend.position = legend_pos)

      # Add watermark if enabled
      if (isTRUE(input$show_watermark)) {
        p <- p + utils$add_shiny_logo(utils$watermark_tr)
      }

      # Apply palette if coloring by a discrete factor
      if (!is.null(plot_color)) {
        # Retrieve the factor levels present in the fitted data for the color factor
        fit_data <- model_fit$data
        if (!is.null(fit_data) && plot_color %in% names(fit_data)) {
          n_levels <- length(unique(stats$na.omit(fit_data[[plot_color]])))
          p <- p +
            ggplot2$scale_color_manual(
              values = utils$get_palette_colors(input$plot_palette, n_levels)
            )
        }
      }

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

        # --- Sheet 1: Summary ---
        openxlsx$addWorksheet(wb, "Summary")

        # Build summary values first
        eq_val <- sidebar_reactives$equation_form()
        eq_str <- if (is.null(eq_val)) "N/A" else as.character(eq_val)

        factors_val <- sidebar_reactives$selected_factors()
        factors_str <- if (
          is.null(factors_val) ||
            length(factors_val) == 0 ||
            identical(factors_val, "None")
        ) {
          "None"
        } else {
          paste(factors_val, collapse = ", ")
        }

        interaction_str <- if (isTRUE(sidebar_reactives$factor_interaction())) {
          "Yes"
        } else {
          "No"
        }

        id_val <- sidebar_reactives$id_var()
        id_str <- if (is.null(id_val)) "N/A" else as.character(id_val)

        x_val <- sidebar_reactives$x_var()
        x_str <- if (is.null(x_val)) "N/A" else as.character(x_val)

        y_val <- sidebar_reactives$y_var()
        y_str <- if (is.null(y_val)) "N/A" else as.character(y_val)

        # Y transformation is determined by equation_form (zben = LL4 transform)
        ytrans_str <- if (identical(eq_val, "zben")) {
          "Log-Log 4 (LL4)"
        } else {
          "None"
        }

        re_val <- sidebar_reactives$random_effects_spec()
        re_str <- if (is.null(re_val) || length(re_val) == 0) {
          "N/A"
        } else {
          paste(re_val, collapse = ", ")
        }

        cov_val <- sidebar_reactives$covariance_structure()
        cov_str <- if (is.null(cov_val)) "N/A" else as.character(cov_val)

        # Build summary data frame
        summary_items <- c(
          "shinybeez Mixed-Effects Demand Analysis",
          "",
          "Export Date",
          "shinybeez Version",
          "",
          "--- Analysis Settings ---",
          "Equation",
          "Factor(s)",
          "Factor Interaction",
          "ID Variable",
          "X Variable",
          "Y Variable",
          "Y Transformation",
          "Random Effects",
          "Covariance Structure"
        )

        summary_values <- c(
          "",
          "",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
          "1.0.0",
          "",
          "",
          eq_str,
          factors_str,
          interaction_str,
          id_str,
          x_str,
          y_str,
          ytrans_str,
          re_str,
          cov_str
        )

        summary_data <- data.frame(
          Item = summary_items,
          Value = summary_values,
          stringsAsFactors = FALSE
        )

        # Collapse levels info
        collapse_info <- tryCatch(
          sidebar_reactives$collapse_levels_reactive(),
          error = function(e) NULL
        )
        if (
          !is.null(collapse_info) &&
            !identical(collapse_info, "ERROR_OVERLAP") &&
            !identical(collapse_info, "ERROR_SINGLE_LEVEL")
        ) {
          collapse_rows <- data.frame(
            Item = c("", "--- Collapse Levels ---"),
            Value = c("", ""),
            stringsAsFactors = FALSE
          )
          summary_data <- rbind(summary_data, collapse_rows)

          if (!is.null(collapse_info$Q0)) {
            q0_str <- paste(names(collapse_info$Q0), collapse = ", ")
            q0_val <- if (nzchar(q0_str)) q0_str else "None"
            summary_data <- rbind(
              summary_data,
              data.frame(
                Item = "Q0 Collapse",
                Value = q0_val,
                stringsAsFactors = FALSE
              )
            )
          }
          if (!is.null(collapse_info$alpha)) {
            alpha_str <- paste(names(collapse_info$alpha), collapse = ", ")
            alpha_val <- if (nzchar(alpha_str)) alpha_str else "None"
            summary_data <- rbind(
              summary_data,
              data.frame(
                Item = "Alpha Collapse",
                Value = alpha_val,
                stringsAsFactors = FALSE
              )
            )
          }
        }

        # Add Fitting Settings section
        nlme_ctrl <- tryCatch(
          sidebar_reactives$nlme_controls(),
          error = function(e) NULL
        )
        if (!is.null(nlme_ctrl)) {
          fitting_rows <- data.frame(
            Item = c(
              "",
              "--- Fitting Settings ---",
              "maxIter",
              "pnlsMaxIter",
              "msMaxIter",
              "tolerance",
              "pnlsTol",
              "minScale",
              "niterEM"
            ),
            Value = c(
              "",
              "",
              as.character(nlme_ctrl$maxIter),
              as.character(nlme_ctrl$pnlsMaxIter),
              as.character(nlme_ctrl$msMaxIter),
              as.character(nlme_ctrl$tolerance),
              as.character(nlme_ctrl$pnlsTol),
              as.character(nlme_ctrl$minScale),
              as.character(nlme_ctrl$niterEM)
            ),
            stringsAsFactors = FALSE
          )
          summary_data <- rbind(summary_data, fitting_rows)
        }

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

        # --- Sheet 3: Descriptives ---
        openxlsx$addWorksheet(wb, "Descriptives")
        if (!is.null(raw_data) && "y_for_model" %in% names(raw_data)) {
          factors <- sidebar_reactives$selected_factors()
          x_var_sel <- sidebar_reactives$x_var()
          grouping_vars <- c(factors, x_var_sel)
          grouping_vars <- grouping_vars[!grouping_vars %in% c("None", "")]
          grouping_vars_present <- intersect(grouping_vars, names(raw_data))

          if (length(grouping_vars_present) == 0) {
            desc_data <- raw_data |>
              dplyr$summarise(
                N = dplyr$n(),
                Mean_Y_Model = mean(y_for_model, na.rm = TRUE),
                SD_Y_Model = stats$sd(y_for_model, na.rm = TRUE),
                Median_Y_Model = stats$median(y_for_model, na.rm = TRUE),
                .groups = "drop"
              )
          } else {
            desc_data <- raw_data |>
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
          desc_data <- dplyr$mutate(
            desc_data,
            dplyr$across(where(is.numeric), ~ round(., 3))
          )
          openxlsx$writeData(wb, "Descriptives", desc_data)
          openxlsx$setColWidths(
            wb,
            "Descriptives",
            cols = 1:ncol(desc_data),
            widths = "auto"
          )
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
