# Shinybeez Modular Architecture

This document describes the modular architecture implemented for the shinybeez application.

## Overview

The application follows the [rhino](https://appsilon.github.io/rhino/) framework pattern with `box` modules for clean separation of concerns.

## Directory Structure

```
app/
├── logic/                          # Business logic (pure functions)
│   ├── mixed_effects/              # Mixed effects demand logic
│   │   ├── __init__.R              # Module exports
│   │   ├── collapse_levels.R       # Factor level collapsing
│   │   ├── data_prep.R             # Column guessing, data preparation
│   │   ├── model_fitting.R         # Covariate transforms, nlme controls
│   │   ├── emms_utils.R            # EMM data processing
│   │   ├── export_utils.R          # Excel export helpers
│   │   └── validation_utils.R      # Data validation helpers
│   ├── demand/                     # Demand-specific logic (future)
│   ├── discounting/                # Discounting-specific logic (future)
│   ├── logging_utils.R             # Centralized logging
│   ├── telemetry_utils.R           # Telemetry/analytics
│   ├── utils.R                     # Shared utilities
│   ├── validate.R                  # Data validation
│   └── mixed_effects_demand_utils.R
├── view/                           # UI components
│   ├── shared/                     # Reusable components
│   │   ├── __init__.R              # Component exports
│   │   ├── data_table.R            # DT-based data table
│   │   ├── plot_settings.R         # Plot settings sidebar
│   │   └── systematic_criteria.R   # Systematic criteria panel
│   ├── mixed_effects_demand_coordinator.R  # Coordinator (25 lines)
│   ├── mixed_effects_demand_sidebar.R      # Sidebar UI/server (1388 lines)
│   ├── mixed_effects_demand_navpanel.R     # Navpanel UI/server (2488 lines)
│   ├── demand.R                    # Demand tab
│   ├── discounting.R               # Discounting tab
│   └── ...
└── main.R                          # App entry point
```

## Logic Modules

### `app/logic/mixed_effects/collapse_levels.R`

Pure functions for building and validating collapse level specifications.

**Exports:**
- `build_collapse_list()` - Build collapse mapping from group names/levels
- `find_overlap()` - Check for overlapping levels
- `validate_no_overlap()` - Validate no overlap between groups
- `process_param_collapse()` - Process collapse for one parameter
- `build_collapse_structure()` - Build complete collapse structure

### `app/logic/mixed_effects/data_prep.R`

Column guessing and data preparation helpers.

**Exports:**
- `guess_first_match()` - Case-insensitive column matching
- `guess_id_column()`, `guess_x_column()`, `guess_y_column()` - Column guessing
- `guess_variable_columns()` - Guess all variable columns
- `identify_factor_columns()` - Find potential factor columns
- `identify_covariate_columns()` - Find numeric covariate columns
- `as_ordered_factor()` - Convert to factor with level order

### `app/logic/mixed_effects/model_fitting.R`

Covariate transformation and nlme control helpers.

**Exports:**
- `transform_covariate()` - Center and/or scale covariate
- `transform_at_value()` - Transform conditioning value
- `build_covariate_column_name()` - Build column name with suffix
- `process_covariate()` - Full covariate processing pipeline
- `build_nlme_control()` - Build nlme control settings (presets available)
- `validate_factors()` - Validate factor columns

### `app/logic/mixed_effects/emms_utils.R`

EMM (Estimated Marginal Means) data processing.

**Exports:**
- `extract_q0_columns()`, `extract_alpha_columns()`, `extract_ev_columns()`
- `round_numeric_columns()` - Round numeric columns in data frame
- `format_comparison_results()` - Format comparison results for display
- `has_emm_content()` - Check if EMM data has content

### `app/logic/mixed_effects/export_utils.R`

Excel export helpers.

**Exports:**
- `build_summary_sheet()` - Build summary data for Excel export
- `add_collapse_info()`, `add_fitting_settings()` - Add sections to summary
- `build_descriptives()` - Build descriptives summary
- `generate_export_timestamp()`, `build_export_filename()` - Filename helpers

## Shared View Components

### `app/view/shared/data_table.R`

Reusable DT-based data table with export buttons.

```r
box::use(app/view/shared/data_table)

# UI
data_table$ui(ns("my_table"))

# Server
data_table$server(
  "my_table",
  data_reactive = my_data,
  filename_prefix = "shinybeez_Export"
)
```

### `app/view/shared/plot_settings.R`

Reusable plot settings sidebar.

```r
box::use(app/view/shared/plot_settings)

# UI
plot_settings$sidebar_ui(ns("plot_opts"))

# Server - returns reactive list of settings
opts <- plot_settings$server("plot_opts")
opts$settings()  # Get current settings
opts$update_trigger()  # Trigger for update button
```

### `app/view/shared/systematic_criteria.R`

Systematic criteria panel for demand/discounting analysis.

```r
box::use(app/view/shared/systematic_criteria)

# UI - panel with sidebar
systematic_criteria$panel_ui(ns("sys"), type = "demand")

# Server - returns reactive with criteria values
criteria <- systematic_criteria$server("sys", type = "demand")
criteria()  # list(deltaq, bounce, reversals, ncons0)
```

## Testing

Tests are located in `tests/testthat/`. Run with:

```r
testthat::test_dir("tests/testthat")
```

**Test Files:**
- `test-collapse_levels.R` - 48 tests
- `test-data_prep.R` - 45 tests
- `test-model_fitting.R` - 55 tests
- `test-emms_utils.R` - 27 tests
- `test-export_utils.R` - 25 tests
- `test-validate.R` - 24 tests
- `test-mixed_effects_demand_utils.R` - 12 tests
- `test-module_imports.R` - 25 tests (integration)

Total: **261 tests**

## Usage Example

```r
box::use(
  app/logic/mixed_effects/data_prep,
  app/logic/mixed_effects/model_fitting,
  app/logic/mixed_effects/collapse_levels
)

# Guess columns from data
guessed <- data_prep$guess_variable_columns(my_data)

# Process covariate with centering
cov_result <- model_fitting$process_covariate(
  df = my_data,
  covariate_col = "age",
  center = TRUE,
  scale = FALSE
)

# Build nlme controls with preset (Balanced, Faster, Stricter available)
ctrl <- model_fitting$build_nlme_control(preset = "Balanced")

# Process collapse levels
result <- collapse_levels$process_param_collapse(
  collapse_enabled = TRUE,
  g1_name = "Low",
  g1_levels = c("A", "B"),
  g2_name = "High",
  g2_levels = c("C", "D")
)
```

## Inline Logic Replacements in mixed_effects_demand.R

The following inline logic has been replaced with module calls:

| Original Inline Code | Module Replacement |
|---------------------|--------------------|
| Column guessing helpers | `data_prep$guess_variable_columns()` |
| Nlme preset definitions | `model_fitting$build_nlme_control(preset)` |
| Collapse helper functions | `collapse_levels$process_param_collapse()` |
| Covariate processing (2 instances) | `model_fitting$process_covariate()` |
| Excel summary sheet building | `export_utils$build_summary_sheet()` |
| Excel collapse info section | `export_utils$add_collapse_info()` |
| Excel fitting settings section | `export_utils$add_fitting_settings()` |
| Excel descriptives building | `export_utils$build_descriptives()` |

**Result**: `mixed_effects_demand.R` reduced from 4200 → 3853 lines (347 lines, 8.3% reduction)

## Migration Notes

When refactoring code to use these modules:

1. **Import the module** using `box::use()`
2. **Replace inline functions** with module function calls
3. **Handle Shiny side effects** (notifications) in the view layer, not logic layer
4. **Add tests** for any new logic functions
5. **Run tests** after each change to catch regressions

## Branding

All export filenames and UI labels use lowercase `shinybeez` branding (e.g., `shinybeez_MixedEffects_Data.xlsx`).
