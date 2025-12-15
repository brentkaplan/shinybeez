# Shinybeez Modular Architecture

This document describes the modular architecture implemented for the shinybeez application.

**Branch:** `refactor/modular-architecture`
**Last Updated:** December 14, 2024
**Tests:** 437 passing
**See Also:** [REFACTORING_ROADMAP.md](./REFACTORING_ROADMAP.md) for next steps

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
│   │   ├── model_output_utils.R    # Model output extraction helpers
│   │   ├── systematic_utils.R      # Systematic criteria computation
│   │   ├── comparisons.R           # Pairwise comparison helpers
│   │   ├── plotting.R              # Plot configuration helpers
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
│   ├── mixed_effects_demand_sidebar.R      # Sidebar UI/server (1,379 lines)
│   ├── mixed_effects_demand_navpanel.R     # Navpanel UI/server (2,133 lines)
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
- `has_emm_content()` - Check if EMM data has content
- `prepare_q0_display_data()`, `prepare_alpha_display_data()`, `prepare_ev_display_data()` - Prepare EMM data for display
- `get_q0_factor_columns()`, `get_alpha_factor_columns()` - Get factor columns for asymmetric collapse
- `build_emm_dt_options()` - Build DT options for EMM tables
- `build_empty_emm_message()` - Build placeholder message for empty EMMs

### `app/logic/mixed_effects/model_output_utils.R`

Model output extraction and formatting helpers.

**Exports:**
- `get_fixed_effects_df()` - Extract fixed effects as data frame
- `get_random_effects_df()` - Extract random effects as data frame
- `get_individual_coefficients_df()` - Extract individual coefficients
- `has_valid_model()` - Check if model has valid output

### `app/logic/mixed_effects/systematic_utils.R`

Systematic criteria computation helpers.

**Exports:**
- `compute_systematic_criteria()` - Compute systematic criteria with optional grouping
- `validate_group_vars()` - Validate grouping variables exist in data

### `app/logic/mixed_effects/comparisons.R`

Pairwise comparison helpers.

**Exports:**
- `build_specs_string()`, `build_specs_formula()` - Build comparison specs
- `get_other_factors()`, `get_contrast_by_arg()` - Factor selection helpers
- `get_comparison_data()` - Extract comparison data by display type
- `is_empty_comparison()`, `can_compare()` - Validation helpers
- `build_comparison_caption()`, `build_empty_comparison_message()` - UI text helpers
- `prepare_comparison_display()` - Prepare comparison data for display
- `get_comparison_ui_state()` - Determine UI state (hide/show_empty/show_table)
- `round_comparison_data()` - Round numeric columns in comparisons

### `app/logic/mixed_effects/plotting.R`

Plot configuration and aesthetic helpers.

**Exports:**
- `validate_aesthetic()` - Validate aesthetic selection against factors
- `compute_aesthetic_defaults()` - Compute smart defaults for aesthetics
- `build_validated_aesthetics()` - Build validated aesthetic mappings
- `has_plot_content()` - Check if there's content to plot
- `build_pred_lines_arg()` - Build prediction lines argument
- `apply_plot_theme()`, `apply_legend_position()`, `apply_color_palette()` - Theme helpers
- `get_axis_transform()` - Get axis transformation function

### `app/logic/mixed_effects/export_utils.R`

Excel export and DT table helpers.

**Exports:**
- `build_summary_sheet()` - Build summary data for Excel export
- `add_collapse_info()`, `add_fitting_settings()` - Add sections to summary
- `build_descriptives()` - Build descriptives summary
- `generate_export_timestamp()`, `build_export_filename()` - Filename helpers
- `build_dt_buttons()` - Standard DT export button configuration
- `write_data_sheet()` - Write data frame to Excel sheet with auto-width
- `write_comparison_sheet()` - Write comparison data with rounding

### `app/logic/mixed_effects/validation_utils.R`

Data validation helpers for reactive contexts.

**Exports:**
- `is_valid_dataframe()` - Check for valid non-empty data frame
- `is_valid_comparison_data()` - Validate comparison output
- `is_valid_emms_data()` - Validate EMMs output
- `is_valid_model_fit()` - Validate model fit object
- `is_ready_for_analysis()` - Check data has required columns
- `safe_nrow()` - Get row count without errors on NULL/invalid data

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
- `test-model_fitting.R` - 57 tests
- `test-emms_utils.R` - 67 tests (expanded for display helpers)
- `test-export_utils.R` - 33 tests
- `test-validation_utils.R` - 31 tests
- `test-validate.R` - 24 tests
- `test-mixed_effects_demand_utils.R` - 21 tests
- `test-module_imports.R` - 25 tests (integration)
- `test-comparisons.R` - 32 tests
- `test-plotting.R` - 53 tests

Total: **437 tests**

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
| DT button configurations (11x) | `export_utils$build_dt_buttons()` |
| Comparison data validation | `validation_utils$is_valid_comparison_data()` |

**Result**: Original `mixed_effects_demand.R` (4,200 lines) split into:
- `mixed_effects_demand_sidebar.R` (1,379 lines)
- `mixed_effects_demand_navpanel.R` (1,621 lines) - reduced from 2,133
- `mixed_effects_demand_coordinator.R` (25 lines)

**Total reduction:** 1,175 lines (28% from original)

## Migration Notes

When refactoring code to use these modules:

1. **Import the module** using `box::use()`
2. **Replace inline functions** with module function calls
3. **Handle Shiny side effects** (notifications) in the view layer, not logic layer
4. **Add tests** for any new logic functions
5. **Run tests** after each change to catch regressions

## Branding

All export filenames and UI labels use lowercase `shinybeez` branding (e.g., `shinybeez_MixedEffects_Data.xlsx`).
