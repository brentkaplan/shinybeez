# Integration tests to verify module imports work correctly

box::use(
  testthat[...],
)

# ------------------------------------------------------------------------------
# Test that all logic modules can be imported
# ------------------------------------------------------------------------------

describe("module imports", {
  it("can import collapse_levels module", {
    box::use(app / logic / mixed_effects / collapse_levels)
    expect_true(is.function(collapse_levels$build_collapse_list))
    expect_true(is.function(collapse_levels$validate_no_overlap))
    expect_true(is.function(collapse_levels$process_param_collapse))
  })

  it("can import data_prep module", {
    box::use(app / logic / mixed_effects / data_prep)
    expect_true(is.function(data_prep$guess_first_match))
    expect_true(is.function(data_prep$guess_variable_columns))
    expect_true(is.function(data_prep$identify_factor_columns))
  })

  it("can import model_fitting module", {
    box::use(app / logic / mixed_effects / model_fitting)
    expect_true(is.function(model_fitting$transform_covariate))
    expect_true(is.function(model_fitting$process_covariate))
    expect_true(is.function(model_fitting$build_nlme_control))
  })

  it("can import emms_utils module", {
    box::use(app / logic / mixed_effects / emms_utils)
    expect_true(is.function(emms_utils$extract_q0_columns))
    expect_true(is.function(emms_utils$extract_alpha_columns))
    expect_true(is.function(emms_utils$has_emm_content))
  })

  it("can import export_utils module", {
    box::use(app / logic / mixed_effects / export_utils)
    expect_true(is.function(export_utils$build_summary_sheet))
    expect_true(is.function(export_utils$build_descriptives))
    expect_true(is.function(export_utils$build_export_filename))
  })

  it("can import validate module", {
    box::use(app / logic / validate)
    expect_true(is.function(validate$check_data))
    expect_true(is.function(validate$reshape_data))
  })

  it("can import mixed_effects_demand_utils module", {
    box::use(app / logic / mixed_effects_demand_utils)
    expect_true(is.function(
      mixed_effects_demand_utils$prepare_systematic_input
    ))
  })
})

# ------------------------------------------------------------------------------
# Test that shared view components can be imported
# ------------------------------------------------------------------------------

describe("shared view components", {
  it("can import data_table component", {
    box::use(app / view / shared / data_table)
    expect_true(is.function(data_table$ui))
    expect_true(is.function(data_table$server))
  })

  it("can import plot_settings component", {
    box::use(app / view / shared / plot_settings)
    expect_true(is.function(plot_settings$sidebar_ui))
    expect_true(is.function(plot_settings$server))
  })

  it("can import systematic_criteria component", {
    box::use(app / view / shared / systematic_criteria)
    expect_true(is.function(systematic_criteria$sidebar_ui))
    expect_true(is.function(systematic_criteria$panel_ui))
    expect_true(is.function(systematic_criteria$server))
  })
})
