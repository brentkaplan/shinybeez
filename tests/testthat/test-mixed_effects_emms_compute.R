# Tests for the run_observed_emms() wrapper in the new mixed_effects
# emms_compute logic module.
#
# Exercises the real beezdemand 0.3.0 API through a small NLME fixture (no
# mocks). Asserts the wrapper returns the Q0-, alpha-, and EV-bearing columns
# that the downstream emms_utils formatters extract by case-insensitive grep,
# so the existing display pipeline keeps working unchanged.

box::use(
  testthat[...],
)

box::use(
  app / logic / mixed_effects / emms_compute,
  app / logic / mixed_effects / emms_utils,
)

describe("run_observed_emms", {
  it("returns an EMM data frame with Q0, alpha, and EV bearing columns", {
    fit <- fit_mixed_fixture()

    result <- emms_compute$run_observed_emms(
      fit_obj = fit,
      factors_in_emm = "drug",
      at = NULL,
      include_ev = TRUE,
      ci_level = 0.95
    )

    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 0)

    # The factor requested in the EMM grid is preserved as a column.
    expect_true("drug" %in% names(result))

    # Column-name contracts that emms_utils relies on (case-insensitive grep).
    expect_true(any(grepl("Q0", names(result), ignore.case = TRUE)))
    expect_true(any(grepl("alpha", names(result), ignore.case = TRUE)))
    expect_true(any(grepl("\\bEV\\b", names(result))))
  })

  it("produces output the emms_utils extractors can consume", {
    fit <- fit_mixed_fixture()

    result <- emms_compute$run_observed_emms(
      fit_obj = fit,
      factors_in_emm = "drug",
      at = NULL,
      include_ev = TRUE,
      ci_level = 0.95
    )

    q0 <- emms_utils$extract_q0_columns(result)
    alpha <- emms_utils$extract_alpha_columns(result)
    ev <- emms_utils$extract_ev_columns(result)

    expect_s3_class(q0, "data.frame")
    expect_s3_class(alpha, "data.frame")
    expect_s3_class(ev, "data.frame")
    expect_gt(nrow(q0), 0)
    expect_gt(nrow(alpha), 0)
    expect_gt(nrow(ev), 0)
  })

  it("conditions EMMs on the at covariate without error", {
    fit <- fit_mixed_fixture_covariate()

    result <- emms_compute$run_observed_emms(
      fit_obj = fit,
      factors_in_emm = "drug",
      at = list(bodyweight = 8.0),
      include_ev = TRUE,
      ci_level = 0.95
    )

    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 0)
    expect_true(any(grepl("Q0", names(result), ignore.case = TRUE)))
  })
})
