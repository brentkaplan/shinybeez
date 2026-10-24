# The consent tier table and the event schema are the two tables every telemetry row is
# checked against: the tier decides whether a row may be written at the session's consent
# level, the schema decides which keys and values the row may carry. Both live in one module
# so a new event type is declared in one place.

box::use(
  testthat[...],
)

box::use(
  app / logic / telemetry_schema,
)

describe("level_rank()", {
  it("orders none < diagnostic < usage", {
    expect_identical(telemetry_schema$level_rank("none"), 0L)
    expect_identical(telemetry_schema$level_rank("diagnostic"), 1L)
    expect_identical(telemetry_schema$level_rank("usage"), 2L)
  })

  it("ranks an unknown level as none", {
    expect_identical(telemetry_schema$level_rank("everything"), 0L)
    expect_identical(telemetry_schema$level_rank(NULL), 0L)
  })
})

describe("tier_of()", {
  it("knows the diagnostic and usage types", {
    expect_identical(telemetry_schema$tier_of("error"), "diagnostic")
    expect_identical(telemetry_schema$tier_of("session_start"), "diagnostic")
    expect_identical(telemetry_schema$tier_of("navigation"), "usage")
    expect_identical(telemetry_schema$tier_of("input"), "usage")
    expect_identical(telemetry_schema$tier_of("login"), "usage")
  })

  it("treats consent_change as writable at every level", {
    expect_identical(telemetry_schema$tier_of("consent_change"), "none")
  })

  it("treats an unknown type as usage", {
    expect_identical(telemetry_schema$tier_of("shopping"), "usage")
  })
})

describe("level_allows()", {
  it("allows consent_change even at none", {
    expect_true(telemetry_schema$level_allows("none", "consent_change"))
  })

  it("refuses everything else at none", {
    expect_false(telemetry_schema$level_allows("none", "error"))
    expect_false(telemetry_schema$level_allows("none", "navigation"))
  })

  it("allows diagnostic rows but not usage rows at diagnostic", {
    expect_true(telemetry_schema$level_allows("diagnostic", "error"))
    expect_true(telemetry_schema$level_allows("diagnostic", "performance"))
    expect_false(telemetry_schema$level_allows("diagnostic", "navigation"))
    expect_false(telemetry_schema$level_allows("diagnostic", "shopping"))
  })

  it("allows everything at usage", {
    expect_true(telemetry_schema$level_allows("usage", "navigation"))
    expect_true(telemetry_schema$level_allows("usage", "shopping"))
  })
})

describe("scrub_user_strings()", {
  it("replaces every double-quoted substring with an ellipsis", {
    out <- telemetry_schema$scrub_user_strings('Could not parse "age" and "Sex, 2".')
    expect_identical(out, 'Could not parse "…" and "…".')
  })

  it("truncates to the limit after scrubbing", {
    long <- paste(rep("x", 10000), collapse = "")
    expect_identical(nchar(telemetry_schema$scrub_user_strings(long)), 500L)
    expect_identical(nchar(telemetry_schema$scrub_user_strings(long, max_chars = 20)), 20L)
  })

  it("passes NULL and non-character values through", {
    expect_null(telemetry_schema$scrub_user_strings(NULL))
    expect_identical(telemetry_schema$scrub_user_strings(42), 42)
  })

  it("does not error on an unbalanced quote", {
    expect_identical(telemetry_schema$scrub_user_strings('a "b'), 'a "b')
  })
})

describe("sanitize_event()", {
  it("maps an unknown enum value to other and drops an unknown key", {
    out <- telemetry_schema$sanitize_event(
      "data_upload", list(file_type = "Rdata", rows = 3L, colour = "x")
    )
    expect_identical(out$data$file_type, "other")
    expect_identical(out$data$rows, 3L)
    expect_false("colour" %in% names(out$data))
    expect_identical(out$dropped, "colour")
  })

  it("keeps a known enum value and a flag", {
    out <- telemetry_schema$sanitize_event(
      "data_upload", list(file_type = "csv", reshaped = TRUE, file_size = 10)
    )
    expect_identical(out$data$file_type, "csv")
    expect_true(out$data$reshaped)
    expect_identical(out$dropped, character())
  })

  it("drops a number key holding a string", {
    out <- telemetry_schema$sanitize_event("data_upload", list(rows = "three"))
    expect_false("rows" %in% names(out$data))
    expect_identical(out$dropped, "rows")
  })

  it("applies the nested rule to model_fitting parameters", {
    out <- telemetry_schema$sanitize_event(
      "model_fitting",
      list(
        model_type = "mixed_effects", status = "failed",
        parameters = list(
          equation = "hs", k = 2, grouped = TRUE, n_ids = 12L,
          factors = "Age,Sex", error = 'fit failed on "Age"', spec = list(a = 1)
        )
      )
    )
    p <- out$data$parameters
    expect_identical(p$equation, "hs")
    expect_identical(p$k, 2)
    expect_true(p$grouped)
    expect_identical(p$n_ids, 12L)
    expect_false("factors" %in% names(p))
    expect_false("spec" %in% names(p))
    expect_identical(p$error, 'fit failed on "…"')
    expect_setequal(out$dropped, c("parameters.factors", "parameters.spec"))
  })

  it("accepts the cancelled and timeout fit statuses", {
    for (s in c("started", "completed", "failed", "cancelled", "timeout")) {
      out <- telemetry_schema$sanitize_event("model_fitting", list(model_type = "demand", status = s))
      expect_identical(out$data$status, s)
    }
  })

  it("applies the nested rule to every non-module key of configuration_snapshot", {
    out <- telemetry_schema$sanitize_event(
      "configuration_snapshot",
      list(module = "mixed_effects", factors = "Age,Sex", n_rows = 10L, equation_form = "koff")
    )
    expect_identical(out$data$module, "mixed_effects")
    expect_identical(out$data$n_rows, 10L)
    expect_identical(out$data$equation_form, "koff")
    expect_false("factors" %in% names(out$data))
    expect_identical(out$dropped, "factors")
  })

  it("scrubs and truncates text keys", {
    out <- telemetry_schema$sanitize_event(
      "error",
      list(error_message = 'bad column "Age"', context = paste(rep("c", 300), collapse = ""))
    )
    expect_identical(out$data$error_message, 'bad column "…"')
    expect_identical(nchar(out$data$context), 100L)
  })

  it("keeps the reshape layout and validation shape keys", {
    out <- telemetry_schema$sanitize_event(
      "reshape", list(target = "demand", outcome = "confirmed", layout = "wide", n_series = 2L)
    )
    expect_identical(out$data$layout, "wide")
    out <- telemetry_schema$sanitize_event(
      "validation_outcome",
      list(module = "demand", outcome = "failure", reason_code = "no_price_columns",
           n_cols = 3L, delim = "comma", first_col_id_like = TRUE)
    )
    expect_identical(out$data$reason_code, "no_price_columns")
    expect_identical(out$data$delim, "comma")
    expect_true(out$data$first_col_id_like)
  })

  it("keeps POSIXct timestamps", {
    now <- Sys.time()
    out <- telemetry_schema$sanitize_event("navigation", list(tab = "Demand", timestamp = now))
    expect_identical(out$data$timestamp, now)
  })

  it("returns an unknown type unchanged", {
    out <- telemetry_schema$sanitize_event("shopping", list(anything = "goes"))
    expect_identical(out$data, list(anything = "goes"))
    expect_identical(out$dropped, character())
  })

  it("handles an empty payload", {
    out <- telemetry_schema$sanitize_event("session_end", list())
    expect_identical(out$data, list())
    expect_identical(out$dropped, character())
  })
})

describe("code fields and the nested string allowlist", {
  it("turns free text in a code field into other", {
    out <- telemetry_schema$sanitize_event(
      "result_export", list(export_type = "csv", file_format = "my data.csv")
    )
    expect_identical(out$data$export_type, "csv")
    expect_identical(out$data$file_format, "other")
  })

  it("drops a nested string under a key that is not a known code or text key", {
    out <- telemetry_schema$sanitize_event(
      "error", list(error_message = "x", details = list(col = "age", n = 2L, equation = "hs"))
    )
    expect_false("col" %in% names(out$data$details))
    expect_identical(out$data$details$n, 2L)
    expect_identical(out$data$details$equation, "hs")
    expect_identical(out$dropped, "details.col")
  })

  it("turns a non-token value under a nested code key into other", {
    out <- telemetry_schema$sanitize_event(
      "model_fitting", list(model_type = "demand", parameters = list(equation = "my equation"))
    )
    expect_identical(out$data$parameters$equation, "other")
  })

  it("maps reshape x_source outside its vocabulary to other", {
    out <- telemetry_schema$sanitize_event("reshape", list(target = "demand", x_source = "age"))
    expect_identical(out$data$x_source, "other")
  })
})

describe("vocabularies the call sites actually use", {
  it("accepts the demand_fixed model type and the partial status", {
    out <- telemetry_schema$sanitize_event(
      "model_fitting", list(model_type = "demand_fixed", status = "partial")
    )
    expect_identical(out$data$model_type, "demand_fixed")
    expect_identical(out$data$status, "partial")
  })

  it("keeps the discounting configuration codes", {
    out <- telemetry_schema$sanitize_event(
      "configuration_snapshot",
      list(module = "discounting", type = "MCQ", imputation = "GGM", transformation = "log", method = "x")
    )
    expect_identical(out$data$type, "MCQ")
    expect_identical(out$data$imputation, "GGM")
    expect_identical(out$data$transformation, "log")
    expect_identical(out$data$method, "x")
    expect_identical(out$dropped, character())
  })

  it("scrubs single-quoted and backticked substrings as well as double-quoted ones", {
    out <- telemetry_schema$scrub_user_strings("bad 'age', `sex` and \"id\" columns")
    expect_identical(out, "bad '…', `…` and \"…\" columns")
  })
})
