# Tests for the run_demand_comparisons() wrapper added to the mixed_effects
# comparisons logic module.
#
# Exercises the real beezdemand 0.3.0 API through a small NLME fixture (no
# mocks) to prove the wrapper drives the canonical `param=` path and never
# trips the deprecated `params_to_compare=` argument. The fitted model is
# provided by fit_mixed_fixture() in helper-mixed.R.

box::use(
  testthat[...],
)

box::use(
  app / logic / mixed_effects / comparisons,
)

describe("run_demand_comparisons", {
  it("returns Q0 and alpha results with ratio and log10 contrast frames", {
    fit <- fit_mixed_fixture()

    result <- comparisons$run_demand_comparisons(
      fit_obj = fit,
      param = c("Q0", "alpha"),
      compare_specs = comparisons$build_specs_formula("drug"),
      contrast_by = NULL,
      at = NULL,
      adjust = "holm",
      report_ratios = TRUE
    )

    expect_type(result, "list")
    expect_true(all(c("Q0", "alpha") %in% names(result)))

    for (param in c("Q0", "alpha")) {
      expect_s3_class(result[[param]]$contrasts_ratio, "data.frame")
      expect_s3_class(result[[param]]$contrasts_log10, "data.frame")
      expect_gt(nrow(result[[param]]$contrasts_ratio), 0)
      expect_gt(nrow(result[[param]]$contrasts_log10), 0)
    }
  })

  it("drives the param= path without tripping the deprecation warning", {
    fit <- fit_mixed_fixture()

    # lifecycle throttles deprecation warnings (~once/8h), so a bare
    # expect_no_condition can pass spuriously. Promoting deprecations to errors
    # makes any legacy params_to_compare= use fail deterministically here.
    withr::local_options(lifecycle_verbosity = "error")

    expect_no_error(
      comparisons$run_demand_comparisons(
        fit_obj = fit,
        param = c("Q0", "alpha"),
        compare_specs = comparisons$build_specs_formula("drug"),
        contrast_by = NULL,
        at = NULL,
        adjust = "holm",
        report_ratios = TRUE
      )
    )
  })

  it("stratifies contrasts within the levels of contrast_by", {
    fit <- fit_mixed_fixture_2factor()

    result <- comparisons$run_demand_comparisons(
      fit_obj = fit,
      param = c("Q0", "alpha"),
      compare_specs = comparisons$build_specs_formula("drug * site"),
      contrast_by = "site",
      at = NULL,
      adjust = "holm",
      report_ratios = TRUE
    )

    # contrast_by yields one drug contrast per site level, and the resulting
    # frame carries the stratifier column.
    expect_s3_class(result$Q0$contrasts_ratio, "data.frame")
    expect_gt(nrow(result$Q0$contrasts_ratio), 0)
    expect_true(
      any(grepl("site", names(result$Q0$contrasts_ratio), ignore.case = TRUE))
    )
  })

  it("forwards the at covariate conditioning through the wrapper", {
    fit <- fit_mixed_fixture_covariate()
    withr::local_options(lifecycle_verbosity = "error")

    result <- comparisons$run_demand_comparisons(
      fit_obj = fit,
      param = c("Q0", "alpha"),
      compare_specs = comparisons$build_specs_formula("drug"),
      contrast_by = NULL,
      at = list(bodyweight = 8.0),
      adjust = "holm",
      report_ratios = TRUE
    )

    expect_s3_class(result$Q0$contrasts_ratio, "data.frame")
    expect_gt(nrow(result$Q0$contrasts_ratio), 0)
  })
})
