# # app/view/mixed_effects_demand.R

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
  app / logic / validate, # For data validation and transformations
  app / logic / mixed_effects_demand_utils
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
    file_input$server("upload_mixed_effects_demand", type = "mixed_effects_demand")
    # Reactive to get the current data (uploaded or default 'ko')
    current_data <- shiny$reactive({
      data_uploaded <- session$userData$data$mixed_effects_demand
      if (!is.null(data_uploaded)) {
        return(data_uploaded)
      } else {
        # Load example 'ko' data from module cache if no file is uploaded yet
        ko <- get_default_ko()
        return(ko)
      }
    })

    # Helper for robust, case-insensitive guessing that returns original names
    guess_first_match <- function(candidates, cols) {
      cols_lower <- tolower(cols)
      for (cand in candidates) {
        idx <- which(cols_lower == cand)
        if (length(idx) > 0) return(cols[idx[1]])
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
          stats::setNames(factor_choices, factor_choices)
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
          choices = c("None" = "", stats::setNames(ch2, ch2)),
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
          choices = c("None" = "", stats::setNames(ch2, ch2)),
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
          choices = c("None" = "", stats::setNames(ch1, ch1)),
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

    # --- Factor 2 collapse UI and logic (mirrors Factor 1) ---
    output$collapse_factor2_ui <- shiny$renderUI({
      ns <- session$ns
      shiny$req(
        input$enable_collapse_factor2,
        input$factor2_choice != "None",
        input$factor2_choice != ""
      )
      all_levels <- factor2_levels_reactive()
      shiny$req(all_levels)

      shiny$tagList(
        shiny$p(paste("Collapsing levels for Factor:", input$factor2_choice)),
        shiny$textInput(
          ns("f2_group1_name"),
          "Name for New Group 1:",
          placeholder = "e.g., LowDose"
        ),
        shiny$selectizeInput(
          ns("f2_group1_levels"),
          "Selected levels to collapse into Group 1:",
          choices = all_levels,
          multiple = TRUE
        ),
        shiny$hr(style = "margin-top: 10px; margin-bottom: 10px;"),
        shiny$textInput(
          ns("f2_group2_name"),
          "Name for New Group 2:",
          placeholder = "e.g., HighDose"
        ),
        shiny$selectizeInput(
          ns("f2_group2_levels"),
          "Selected levels to collapse into Group 2:",
          choices = all_levels,
          multiple = TRUE
        )
      )
    })

    shiny$observeEvent(
      input$f2_group1_levels,
      {
        all_levels <- factor2_levels_reactive()
        selected_g1 <- input$f2_group1_levels %||% character(0)
        shiny$req(input$enable_collapse_factor2, all_levels)

        current_selected_g2 <- shiny::isolate(
          input$f2_group2_levels %||% character(0)
        )
        available_for_g2 <- setdiff(all_levels, selected_g1)
        new_selected_g2 <- intersect(current_selected_g2, available_for_g2)

        shiny$updateSelectizeInput(
          session,
          "f2_group2_levels",
          choices = available_for_g2,
          selected = new_selected_g2
        )
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    shiny$observeEvent(
      input$f2_group2_levels,
      {
        all_levels <- factor2_levels_reactive()
        selected_g2 <- input$f2_group2_levels %||% character(0)
        shiny$req(input$enable_collapse_factor2, all_levels)

        current_selected_g1 <- shiny::isolate(
          input$f2_group1_levels %||% character(0)
        )

        available_for_g1 <- setdiff(all_levels, selected_g2)
        new_selected_g1 <- intersect(current_selected_g1, available_for_g1)

        shiny$updateSelectizeInput(
          session,
          "f2_group1_levels",
          choices = available_for_g1,
          selected = new_selected_g1
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
      final_list <- list()

      # Helper to validate and append for one factor
      handle_factor <- function(
        enable_flag,
        factor_name,
        g1_name,
        g1_levels,
        g2_name,
        g2_levels,
        all_levels
      ) {
        if (
          !enable_flag || is.null(factor_name) || factor_name %in% c("None", "")
        ) {
          return(NULL)
        }

        # If current levels are insufficient for collapsing UI (threshold 3), ignore
        if (is.null(all_levels) || length(all_levels) < 3) {
          return(NULL)
        }

        # Overlap validation
        if (
          length(g1_levels) > 0 &&
            length(g2_levels) > 0 &&
            length(intersect(g1_levels, g2_levels)) > 0
        ) {
          shiny$showNotification(
            paste0(
              "Overlap detected in selected levels for new groups for factor '",
              factor_name,
              "'. Please ensure unique selections."
            ),
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

        if (length(collapse_list_for_factor) == 0) {
          return(NULL)
        }

        # Enforce at least two resulting levels after collapsing
        assigned <- unique(c(
          g1_levels %||% character(0),
          g2_levels %||% character(0)
        ))
        remaining <- setdiff(all_levels %||% character(0), assigned)
        num_new_groups <- length(collapse_list_for_factor)
        num_result_levels <- num_new_groups + length(remaining)
        if (num_result_levels < 2) {
          shiny$showNotification(
            paste0(
              "Collapsing for factor '",
              factor_name,
              "' would result in a single level, which is not allowed."
            ),
            type = "error",
            duration = 7
          )
          return("ERROR_SINGLE_LEVEL")
        }

        return(collapse_list_for_factor)
      }

      # Factor 1
      f1_res <- handle_factor(
        enable_flag = isTRUE(input$enable_collapse_factor1),
        factor_name = input$factor1_choice,
        g1_name = trimws(input$f1_group1_name),
        g1_levels = input$f1_group1_levels,
        g2_name = trimws(input$f1_group2_name),
        g2_levels = input$f1_group2_levels,
        all_levels = tryCatch(factor1_levels_reactive(), error = function(...) {
          NULL
        })
      )
      if (is.character(f1_res)) {
        return(f1_res)
      }
      if (is.list(f1_res)) {
        final_list[[input$factor1_choice]] <- f1_res
      }

      # Factor 2
      f2_res <- handle_factor(
        enable_flag = isTRUE(input$enable_collapse_factor2),
        factor_name = input$factor2_choice,
        g1_name = trimws(input$f2_group1_name),
        g1_levels = input$f2_group1_levels,
        g2_name = trimws(input$f2_group2_name),
        g2_levels = input$f2_group2_levels,
        all_levels = tryCatch(factor2_levels_reactive(), error = function(...) {
          NULL
        })
      )
      if (is.character(f2_res)) {
        return(f2_res)
      }
      if (is.list(f2_res)) {
        final_list[[input$factor2_choice]] <- f2_res
      }

      if (length(final_list) > 0) final_list else NULL
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
      sigma <- suppressWarnings(stats::sd(x, na.rm = TRUE))
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
      sigma <- suppressWarnings(stats::sd(x, na.rm = TRUE))

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
    # Debug flag: read from R option or environment variable
    is_debug <- isTRUE(getOption(
      "shinybeez.debug",
      as.logical(Sys.getenv("SHINYBEEZ_DEBUG", "0"))
    ))

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
      sigma <- suppressWarnings(stats::sd(x, na.rm = TRUE))

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
            list(extend = "csv", filename = "ShinyBeez_MixedEffects_Input_Data", title = NULL),
            list(extend = "excel", filename = "ShinyBeez_MixedEffects_Input_Data", title = NULL),
            list(extend = "pdf", filename = "ShinyBeez_MixedEffects_Input_Data", title = NULL)
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
            list(extend = "csv", filename = "ShinyBeez_MixedEffects_Descriptives", title = NULL),
            list(extend = "excel", filename = "ShinyBeez_MixedEffects_Descriptives", title = NULL),
            list(extend = "pdf", filename = "ShinyBeez_MixedEffects_Descriptives", title = NULL)
          )
        ),
        filter = "top",
        class = "compact hover"
      )
    })

    # Systematic Criteria: optional grouping selector (multi-select)
    output$systematic_group_ui <- shiny$renderUI({
      facs <- sidebar_reactives$selected_factors()
      if (is.null(facs) || length(facs) == 0) return(NULL)
      shiny$selectizeInput(
        ns("systematic_group_by"),
        "Group results by (optional):",
        choices = stats$setNames(facs, facs),
        selected = facs, # default to all selected factors; empty selection means no grouping
        multiple = TRUE,
        options = list(placeholder = "Select factors to group by (leave empty for none)")
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
          dplyr$group_modify(~ beezdemand$CheckUnsystematic(
            dat = .x[, c("id", "x", "y")],
            deltaq = input$deltaq,
            bounce = input$bounce,
            reversals = input$reversals,
            ncons0 = input$ncons0
          ))
      } else {
        df_sys <- mixed_effects_demand_utils$prepare_systematic_input(
          df = df_raw, id_col = id_col, x_col = x_col, y_col = y_col
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
              list(extend = "csv", filename = "ShinyBeez_MixedEffects_Systematic_Criteria", title = NULL),
              list(extend = "excel", filename = "ShinyBeez_MixedEffects_Systematic_Criteria", title = NULL),
              list(extend = "pdf", filename = "ShinyBeez_MixedEffects_Systematic_Criteria", title = NULL)
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
        random_effects_formula_to_pass <- stats::as.formula(paste0(
          random_formula_str,
          " ~ 1"
        ))

        # browser()
        # ## TODO: FIXME this logic so that it works when transforming
        # Read the y-scale flag prepared in data_to_analyze()
        y_is_ll4 <- isTRUE(attr(df, "y_is_ll4"))

        # Ensure the authoritative column exists
        if (!("y_for_model" %in% names(df))) {
          shiny$showNotification(
            "Internal error: 'y_for_model' not found.",
            type = "error"
          )
          return(NULL)
        }

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
        # browser()
        if (is_debug) {
          print(random_effects_formula_to_pass)
        }
        # Prepare covariate info for modeling (ensures transformed column exists)
        cov_info <- build_covariate_modeling_info(df)
        cont_covars_to_pass <- cov_info$model_covariate_name

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
            NULL
          }
        )

        if (is.null(model_fit) || is.null(model_fit$model)) {
          shiny$removeNotification(notif_id)
          shiny$showNotification(
            "Model fitting failed or did not converge.",
            type = "error"
          )
          return(NULL)
        }

        # Persist the scale info for plotting
        model_fit$param_info$y_is_ll4 <- y_is_ll4

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
            list(extend = "csv", filename = "ShinyBeez_MixedEffects_Fixed_Effects", title = NULL),
            list(extend = "excel", filename = "ShinyBeez_MixedEffects_Fixed_Effects", title = NULL),
            list(extend = "pdf", filename = "ShinyBeez_MixedEffects_Fixed_Effects", title = NULL)
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
              list(extend = "csv", filename = "ShinyBeez_MixedEffects_Individual_Coefficients", title = NULL),
              list(extend = "excel", filename = "ShinyBeez_MixedEffects_Individual_Coefficients", title = NULL),
              list(extend = "pdf", filename = "ShinyBeez_MixedEffects_Individual_Coefficients", title = NULL)
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
            list(extend = "csv", filename = "ShinyBeez_MixedEffects_Random_Effects", title = NULL),
            list(extend = "excel", filename = "ShinyBeez_MixedEffects_Random_Effects", title = NULL),
            list(extend = "pdf", filename = "ShinyBeez_MixedEffects_Random_Effects", title = NULL)
          )
        ),
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
      # Build 'at' for covariate conditioning
      df_now <- data_to_analyze()
      cov_info <- build_covariate_modeling_info(df_now)
      emms_data <- tryCatch(
        beezdemand$get_observed_demand_param_emms(
          fit_obj = model_fit,
          factors_in_emm = sel_factors, # Use selected factors
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
        extensions = c("Buttons"),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = "Btip",
          buttons = list(
            list(extend = "copy"),
            list(extend = "print"),
            list(extend = "csv", filename = "ShinyBeez_MixedEffects_EMMs_EV", title = NULL),
            list(extend = "excel", filename = "ShinyBeez_MixedEffects_EMMs_EV", title = NULL),
            list(extend = "pdf", filename = "ShinyBeez_MixedEffects_EMMs_EV", title = NULL)
          )
        )
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

      # Build 'at' for covariate conditioning
      df_now <- data_to_analyze()
      cov_info <- build_covariate_modeling_info(df_now)

      comps <- tryCatch(
        beezdemand$get_demand_comparisons(
          fit_obj = model_fit,
          params_to_compare = c("Q0", "alpha"),
          compare_specs = stats$as.formula(paste("~", specs_str)),
          contrast_by = contrast_by_arg, # Compare `main_factor` within levels of `by_factor`
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
        extensions = c("Buttons"),
        options = list(
          scrollX = TRUE,
          pageLength = 5,
          autoWidth = TRUE,
          dom = "Btip",
          buttons = list(
            list(extend = "copy"),
            list(extend = "print"),
            list(extend = "csv", filename = "ShinyBeez_MixedEffects_Q0_Comparisons", title = NULL),
            list(extend = "excel", filename = "ShinyBeez_MixedEffects_Q0_Comparisons", title = NULL),
            list(extend = "pdf", filename = "ShinyBeez_MixedEffects_Q0_Comparisons", title = NULL)
          )
        ),
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
        extensions = c("Buttons"),
        options = list(
          scrollX = TRUE,
          pageLength = 5,
          autoWidth = TRUE,
          dom = "Btip",
          buttons = list(
            list(extend = "copy"),
            list(extend = "print"),
            list(extend = "csv", filename = "ShinyBeez_MixedEffects_Alpha_Comparisons", title = NULL),
            list(extend = "excel", filename = "ShinyBeez_MixedEffects_Alpha_Comparisons", title = NULL),
            list(extend = "pdf", filename = "ShinyBeez_MixedEffects_Alpha_Comparisons", title = NULL)
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
      # browser()
      if (is_debug) {
        print(
          list(
            plot_color = plot_color,
            plot_linetype = plot_linetype,
            plot_facet_str = plot_facet_str,
            show_lines_arg = show_lines_arg,
            current_y_trans = current_y_trans,
            current_x_trans = current_x_trans
          )
        )
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
      ) +
        utils$add_shiny_logo(utils$watermark_tr)

      # Apply palette if coloring by a discrete factor
      if (!is.null(plot_color)) {
        # Retrieve the factor levels present in the fitted data for the color factor
        fit_data <- model_fit$data
        if (!is.null(fit_data) && plot_color %in% names(fit_data)) {
          n_levels <- length(unique(stats::na.omit(fit_data[[plot_color]])))
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
        input$plot_palette,
        input$plot_x_trans_log,
        input$plot_y_trans_log,
        input$show_population_lines,
        input$show_individual_lines,
        input$show_observed_points_plot,
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
  })
}
