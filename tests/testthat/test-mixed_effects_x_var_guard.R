box::use(
  app/logic/mixed_effects/data_prep,
)

# CLOSE-OUT for telemetry signature e84804c9 (2026-07-18 triage):
# "mixed_effects_model_fit :: Model fitting error: `x_var` column x must be numeric."
# A character-typed X column reached beezdemand::fit_demand_mixed untouched because
# the X dropdown offered every column and fitted_model_reactive never validated X.
# These helpers back the two guards: the sidebar filters X choices to numeric-like
# columns (100% clean), and the navpanel coerces/rejects X before the fit call.
# Pre-existing NA/Inf in an already-numeric column pass through unchanged - that is
# beezdemand's contract to enforce, not ours; the guard only closes the
# non-numeric-column hole that produced the production error.

describe("is_numeric_like", {
  it("accepts numeric and integer columns", {
    expect_true(data_prep$is_numeric_like(c(1.5, 2, 3)))
    expect_true(data_prep$is_numeric_like(1:5))
  })

  it("accepts character columns that coerce cleanly", {
    expect_true(data_prep$is_numeric_like(c("1", "2.5", "0.001")))
  })

  it("applies the coercion threshold to character columns", {
    # 19 clean + 1 junk = 95% clean: passes at the covariate threshold (0.95),
    # fails at the strict X threshold (1)
    v <- c(as.character(1:19), "junk")
    expect_true(data_prep$is_numeric_like(v, threshold = 0.95))
    expect_false(data_prep$is_numeric_like(v, threshold = 1))
    # 9 clean + 1 junk = 90% clean: fails both
    expect_false(data_prep$is_numeric_like(c(as.character(1:9), "junk")))
  })

  it("rejects ratio-label, all-NA, factor, and logical columns", {
    expect_false(data_prep$is_numeric_like(c("1:1", "2:1", "3:1")))
    expect_false(data_prep$is_numeric_like(as.character(c(NA, NA))))
    expect_false(data_prep$is_numeric_like(factor(c("1", "2"))))
    expect_false(data_prep$is_numeric_like(c(TRUE, FALSE)))
  })

  it("ignores NA entries when judging a character column", {
    expect_true(data_prep$is_numeric_like(c("1", NA, "3"), threshold = 1))
  })
})

describe("numeric_x_candidates", {
  df <- data.frame(
    subject = c("s1", "s2"),
    x = c("0.5", "1"),
    y = c(10, 8),
    grp = factor(c("a", "b")),
    notes = c("junk", "1"),
    stringsAsFactors = FALSE
  )

  it("returns only columns that coerce 100% cleanly", {
    expect_identical(data_prep$numeric_x_candidates(df), c("x", "y"))
  })

  it("honors exclusions", {
    expect_identical(data_prep$numeric_x_candidates(df, exclude = "y"), "x")
  })

  it("returns character(0) when nothing qualifies", {
    expect_identical(
      data_prep$numeric_x_candidates(df[, c("subject", "grp"), drop = FALSE]),
      character(0)
    )
  })
})

describe("choose_x_selection", {
  it("keeps a guessed selection that is a valid candidate", {
    expect_identical(
      data_prep$choose_x_selection("x", c("x", "y"), id = "subject", y = "y"),
      "x"
    )
  })

  it("falls back to the first candidate that is not the id or y column", {
    expect_identical(
      data_prep$choose_x_selection("notes", c("subject_num", "x", "y"),
        id = "subject_num", y = "y"
      ),
      "x"
    )
  })

  it("returns empty when no candidate remains", {
    expect_identical(
      data_prep$choose_x_selection("x", character(0), id = "subject", y = "y"),
      ""
    )
    expect_identical(
      data_prep$choose_x_selection("x", c("id_num", "y"), id = "id_num", y = "y"),
      ""
    )
  })
})

describe("coerce_x_numeric", {
  it("passes numeric and integer columns through", {
    res <- data_prep$coerce_x_numeric(c(0.5, 1, 2))
    expect_true(res$ok)
    expect_identical(res$values, c(0.5, 1, 2))
    res_int <- data_prep$coerce_x_numeric(1:3)
    expect_true(res_int$ok)
    expect_identical(res_int$values, c(1, 2, 3))
  })

  it("preserves pre-existing NA in a numeric column (beezdemand's contract, not ours)", {
    res <- data_prep$coerce_x_numeric(c(1, NA, 3))
    expect_true(res$ok)
    expect_identical(res$values, c(1, NA, 3))
  })

  it("cleanly coerces an all-numeric character column", {
    res <- data_prep$coerce_x_numeric(c("0.5", "1", "2"))
    expect_true(res$ok)
    expect_identical(res$values, c(0.5, 1, 2))
  })

  it("keeps NA entries in a character column without counting them as failures", {
    res <- data_prep$coerce_x_numeric(c("1", NA, "3"))
    expect_true(res$ok)
    expect_identical(res$values, c(1, NA, 3))
  })

  it("rejects a character column where coercion introduces new NAs", {
    res <- data_prep$coerce_x_numeric(c("1", "junk", "3", "also junk"))
    expect_false(res$ok)
    expect_identical(res$n_bad, 2L)
  })

  it("rejects factor and logical columns outright", {
    expect_false(data_prep$coerce_x_numeric(factor(c("1", "2")))$ok)
    expect_false(data_prep$coerce_x_numeric(c(TRUE, FALSE))$ok)
  })
})
