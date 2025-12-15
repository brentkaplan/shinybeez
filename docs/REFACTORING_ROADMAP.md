# Shinybeez Refactoring Roadmap

This document outlines the next steps for refactoring, performance optimization, and testing improvements for the shinybeez application.

**Branch:** `refactor/modular-architecture`
**Last Updated:** December 14, 2024

---

## Current State Summary

### Progress Completed

| Metric | Before | After | Change |
|--------|--------|-------|--------|
| `mixed_effects_demand.R` | 4,200 lines | Split into 3 files | -100% (deleted) |
| Sidebar module | - | 1,379 lines | New |
| Navpanel module | - | 1,621 lines | New (reduced from 2,133) |
| Coordinator module | - | 25 lines | New |
| **Total view code** | 4,200 lines | 3,025 lines | -1,175 lines (28%) |
| Unit tests | 0 | 437 | +437 |
| Logic modules extracted | 0 | 10 | +10 |

### Extracted Logic Modules

| Module | Lines | Purpose |
|--------|-------|---------|
| `collapse_levels.R` | 186 | Factor level collapsing |
| `data_prep.R` | 184 | Column guessing, data prep |
| `model_fitting.R` | 304 | Covariate transforms, nlme controls |
| `emms_utils.R` | 350 | EMM data processing + display helpers |
| `export_utils.R` | 345 | Excel export + DT button helpers |
| `validation_utils.R` | 76 | Data validation helpers |
| `comparisons.R` | 218 | Pairwise comparison helpers |
| `plotting.R` | 223 | Plot configuration + aesthetics |
| `model_output_utils.R` | 120 | Model output extraction |
| `systematic_utils.R` | 115 | Systematic criteria computation |

---

## Phase 1: Continue View Layer Decomposition

### 1.1 Extract Plotting Logic (~300 lines)

**Files:** `mixed_effects_demand_navpanel.R` lines 1437-1750
**Priority:** High
**Effort:** Medium

The plotting section contains:
- Plot aesthetic population logic
- `plot_object_reactive` with complex ggplot construction
- Download handlers for plot export

**Action:**
```
app/view/mixed_effects/
├── plotting.R          # Plot reactive and handlers
└── plot_download.R     # Download handlers (optional)
```

### 1.2 Extract Comparisons Logic (~250 lines) ✅ DONE

**Status:** Completed December 2024

Created `app/logic/mixed_effects/comparisons.R` with:
- `build_specs_string()`, `build_specs_formula()` - Build comparison specs
- `get_comparison_data()`, `prepare_comparison_display()` - Data extraction
- `get_comparison_ui_state()` - UI state management (hide/show_empty/show_table)
- `round_comparison_data()`, validation helpers, caption builders

### 1.3 Extract EMMs Display Logic (~150 lines) ✅ DONE

**Status:** Completed December 2024

Added to `app/logic/mixed_effects/emms_utils.R`:
- `prepare_q0_display_data()`, `prepare_alpha_display_data()`, `prepare_ev_display_data()`
- `get_q0_factor_columns()`, `get_alpha_factor_columns()` - Asymmetric collapse support
- `build_emm_dt_options()` - DT table options helper
- `build_empty_emm_message()` - Empty state messages

### 1.4 Extract Systematic Criteria Logic (~100 lines) ✅ DONE

**Status:** Completed December 2024

Created `app/logic/mixed_effects/systematic_utils.R` with:
- `compute_systematic_criteria()` - Compute criteria with optional grouping
- `validate_group_vars()` - Validate grouping variables

### 1.5 Extract Model Output Logic ✅ DONE

**Status:** Completed December 2024

Created `app/logic/mixed_effects/model_output_utils.R` with:
- `get_fixed_effects_df()` - Extract fixed effects as data frame
- `get_random_effects_df()` - Extract random effects as data frame
- `get_individual_coefficients_df()` - Extract individual coefficients
- `has_valid_model()` - Model validation helper

---

## Phase 2: Wire Shared Components

### 2.1 Use Shared `data_table` in Mixed Effects

**Files:** `mixed_effects_demand_navpanel.R`
**Priority:** Medium
**Effort:** High (reactive pattern changes)

Currently 11 DT tables use custom rendering. The shared `data_table` component exists but isn't wired in due to different reactive patterns.

**Challenge:** Current tables use inline `DT$renderDT` inside `shiny$observe` blocks, while shared component expects a reactive data source.

**Options:**
1. Refactor table sections to use reactive pattern compatible with shared component
2. Create variant of shared component that works with observe pattern
3. Leave as-is (already using `build_dt_buttons` helper)

**Recommendation:** Option 3 - the `build_dt_buttons` helper already reduces duplication. Full component wiring would require significant reactive restructuring for marginal benefit.

### 2.2 Wire Shared Components into Demand/Discounting

**Files:** `demand_data_table.R` (382 lines), `discounting_data_table.R` (259 lines)
**Priority:** Low
**Effort:** Medium

These modules could potentially use shared components, but have their own specific logic tied to their data structures.

---

## Phase 3: Sidebar Refactoring

### 3.1 Extract Collapse UI Section (~400 lines)

**Files:** `mixed_effects_demand_sidebar.R` lines 700-1100
**Priority:** Medium
**Effort:** Medium

The factor collapse UI is complex with:
- Dynamic UI generation based on factor levels
- Overlap prevention observers
- Clear/reset helpers

**Action:**
```
app/view/mixed_effects/
├── collapse_ui.R       # Collapse level UI module
```

### 3.2 Extract Advanced Controls Section (~200 lines)

**Files:** `mixed_effects_demand_sidebar.R` lines 200-400
**Priority:** Low
**Effort:** Low

nlme control inputs and preset handling.

---

## Phase 4: Performance Optimization

### 4.1 Lazy Loading of Example Data

**Current:** Example data loaded synchronously on module init
**Improvement:** Load on first access only

**Files:** `mixed_effects_demand_sidebar.R` line 25-35

```r
# Current (eager)
.local_cache$ko <- readr$read_csv(...)

# Improved (lazy)
get_default_ko <- function() {
  if (is.null(.local_cache$ko)) {
    .local_cache$ko <- readr$read_csv(...)
  }
  .local_cache$ko
}
```

**Status:** Already implemented ✓

### 4.2 Debounce Expensive Reactives

**Priority:** Medium
**Effort:** Low

Add `shiny$debounce()` to expensive reactives that fire on rapid input changes:

```r
# Example
data_to_analyze <- shiny$reactive({...}) |> shiny$debounce(300)
```

**Candidates:**
- `data_to_analyze()` reactive
- Factor choice update observers

### 4.3 Conditional EMMs Calculation

**Priority:** Medium
**Effort:** Low

Currently `emms_data_reactive` calculates all EMMs even if user only views one tab. Consider splitting into separate reactives or using `bindEvent` to trigger only when tab is active.

### 4.4 Server-Side DT for Large Datasets

**Status:** Already using `server = TRUE` for most tables ✓

One exception: `systematic_table` uses `server = FALSE` which is appropriate for its small output size.

### 4.5 Profile and Optimize Model Fitting

**Priority:** Low
**Effort:** High

Model fitting is inherently expensive. Options:
- **Progress indicators:** Already showing notifications ✓
- **Background processing:** Use `promises`/`future` for async fitting (complex)
- **Caching:** Not recommended - users expect fresh fits when clicking "Run"

---

## Phase 5: Testing Improvements

### 5.1 Current Test Coverage

| Test File | Tests | Coverage Area |
|-----------|-------|---------------|
| `test-collapse_levels.R` | 48 | Factor collapsing |
| `test-data_prep.R` | 45 | Column guessing |
| `test-model_fitting.R` | 57 | Covariate transforms |
| `test-emms_utils.R` | 27 | EMM formatting |
| `test-export_utils.R` | 33 | Excel export helpers |
| `test-validation_utils.R` | 31 | Validation helpers |
| `test-validate.R` | 24 | Data validation |
| `test-mixed_effects_demand_utils.R` | 21 | Utils functions |
| `test-module_imports.R` | 25 | Import integration |
| **Total** | **296** | |

### 5.2 Add Integration Tests with shinytest2

**Priority:** High
**Effort:** High

Currently no end-to-end UI tests. Add `shinytest2` tests for critical workflows:

```r
# tests/testthat/test-app-mixed_effects.R
test_that("Mixed effects workflow completes", {
  app <- shinytest2::AppDriver$new(app_dir = ".")
  app$set_inputs(`app-nav` = "MixedEffectsDemand")
  app$upload_file(`app-mixed_effects_demand-file_input-file` = test_file)
  app$click("app-mixed_effects_demand-run_button")
  app$wait_for_idle()
  expect_true(app$get_value(output = "app-mixed_effects_demand-model_summary_output") != "")
})
```

**Test Scenarios:**
1. Load example data → Fit model → View results
2. Upload custom data → Configure factors → Fit model
3. Collapse levels → Verify asymmetric collapse works
4. Export Excel → Verify file downloads

### 5.3 Add Snapshot Tests for UI

**Priority:** Medium
**Effort:** Medium

Use `shinytest2` snapshots to catch unintended UI changes:

```r
test_that("Sidebar UI renders correctly", {
  app <- shinytest2::AppDriver$new(...)
  expect_snapshot(app$get_html(".sidebar"))
})
```

### 5.4 Add Tests for Demand/Discounting Modules

**Priority:** Medium
**Effort:** Medium

Current tests focus on mixed effects. Extend to:
- `demand.R` / `demand_data_table.R` / `demand_results_table.R`
- `discounting.R` / `discounting_data_table.R` / `discounting_results_table.R`

### 5.5 Add Performance Benchmarks

**Priority:** Low
**Effort:** Medium

Track performance regressions:

```r
# tests/testthat/test-performance.R
test_that("Model fitting completes in reasonable time", {
  skip_on_cran()
  data <- load_test_data()
  time <- system.time({
    result <- beezdemand::fit_demand_mixed(...)
  })
  expect_lt(time["elapsed"], 30)  # Should complete in < 30 seconds
})
```

---

## Phase 6: Code Quality

### 6.1 Address Lint Warnings

**Priority:** Low
**Effort:** Low

Current lint warnings (non-blocking):
- `seq_len(ncol(...))` vs `1:ncol(...)` - 12 instances in navpanel

### 6.2 Consolidate Duplicate Validation Logic

**Priority:** Low
**Effort:** Low

`validation_utils.R` and `validate.R` have some overlap. Consider:
- Merge into single validation module
- Or clearly separate: `validate.R` for user data, `validation_utils.R` for internal checks

### 6.3 Standardize Logging

**Priority:** Low
**Effort:** Medium

Current logging uses mix of:
- `rhino$log$info()` in some modules
- `session_logger$*()` in others
- `logging_utils$*()` custom functions

Standardize on one approach.

---

## Recommended Priority Order

### Immediate (Next Session)

1. ~~**1.1** Extract plotting logic to separate module~~ ✅ DONE (Dec 2024)
2. ~~**1.2** Extract comparisons logic to separate module~~ ✅ DONE (Dec 2024)
3. ~~**1.3** Extract EMMs display logic~~ ✅ DONE (Dec 2024)
4. ~~**1.4** Extract systematic criteria logic~~ ✅ DONE (Dec 2024)
5. **5.2** Add shinytest2 integration tests for critical workflows

### Short-Term (1-2 Sessions)

6. **3.1** Extract collapse UI section
7. **4.2** Add debounce to expensive reactives
8. **5.4** Add tests for demand/discounting modules
9. Continue navpanel reduction (1,621 → <1,500 lines)

### Medium-Term (Future)

10. **6.2** Consolidate validation logic
11. **6.3** Standardize logging

### Low Priority / As-Needed

- **2.1** Wire shared data_table (marginal benefit)
- **4.5** Async model fitting (high complexity)
- **6.1** Lint warnings (cosmetic)

### Future Enhancement: Plotting Options for Demand/Discounting Tabs

Adopt the same plotting configuration options from mixed effects to the demand and discounting tabs:
- Color/linetype/facet aesthetic selectors
- Theme selection (prism, classic, minimal)
- Legend position
- Color palette selection
- Axis transformations (log scale)
- Reuse `app/logic/mixed_effects/plotting.R` helpers where applicable

---

## Architecture Decisions

### Keep Separate
- **Sidebar vs Navpanel:** Maintain clear separation for maintainability
- **Logic vs View:** Pure functions in logic/, Shiny reactives in view/

### Consider Merging
- `mixed_effects_demand_utils.R` → Absorb into specific logic modules
- `validate.R` + `validation_utils.R` → Single validation module

### Don't Over-Engineer
- Shared components are useful but not mandatory
- Some duplication is acceptable if it improves clarity
- Avoid premature optimization of model fitting

---

## Metrics to Track

| Metric | Current | Target |
|--------|---------|--------|
| Unit tests | 437 | 450+ |
| Integration tests | 0 | 10+ |
| Largest view file | 1,621 lines | <1,500 lines |
| Test coverage (estimated) | ~70% logic | 80%+ logic |
| CI/CD passing | Yes | Maintain |

---

## Quick Reference: Module Locations

```
app/logic/mixed_effects/
├── __init__.R              # Exports all modules
├── collapse_levels.R       # Factor collapsing
├── comparisons.R           # Pairwise comparison helpers
├── data_prep.R             # Column guessing
├── emms_utils.R            # EMM formatting + display
├── export_utils.R          # Excel + DT buttons
├── model_fitting.R         # Covariate + nlme
├── model_output_utils.R    # Model output extraction
├── plotting.R              # Plot aesthetics + theming
├── systematic_utils.R      # Systematic criteria computation
└── validation_utils.R      # Validation helpers

app/view/
├── mixed_effects_demand_coordinator.R  # Entry point (25 lines)
├── mixed_effects_demand_sidebar.R      # Sidebar UI/server (1,379 lines)
├── mixed_effects_demand_navpanel.R     # Main content UI/server (1,621 lines)
└── shared/
    ├── data_table.R        # Reusable DT table
    ├── plot_settings.R     # Plot settings sidebar
    └── systematic_criteria.R  # Criteria panel
```
