# Every rejection message from check_data() carries a stable machine-readable code, so the
# analytics work can group failures by code instead of by message text (which is now scrubbed
# of the user's header strings before it is recorded).

box::use(
  testthat[...],
)

box::use(
  app / logic / validate,
)

code_for <- function(dat, type = "demand") {
  validate$reason_code(validate$check_data(dat, type = type))
}

describe("reason_code()", {
  it("is NULL for a passing check", {
    ok <- data.frame(id = 1:2, x = c(1, 2), y = c(3, 4))
    expect_true(isTRUE(validate$check_data(ok, "demand")))
    expect_null(validate$reason_code(validate$check_data(ok, "demand")))
  })

  it("is unspecified for a plain character message", {
    expect_identical(validate$reason_code("some message"), "unspecified")
  })

  it("names the demand header failures", {
    expect_identical(code_for(data.frame(id = 1:2, a = 1:2, b = 3:4)), "unparsable_price_headers")
    expect_identical(
      code_for(data.frame(id = 1:2, `1` = 1:2, `1.00` = 3:4, check.names = FALSE)),
      "duplicate_price_headers"
    )
    expect_identical(code_for(data.frame(id = 1:2)), "no_price_columns")
    expect_identical(code_for(data.frame(participant = 1:2, `1` = 1:2, check.names = FALSE)), "first_column_not_id")
    expect_identical(
      code_for(data.frame(group = 1:2, id = 1:2, `1` = 1:2, check.names = FALSE)),
      "first_columns_not_id_group"
    )
  })

  it("names the demand sufficiency failures", {
    one_col <- data.frame(id = 1:2, `1` = 1:2, check.names = FALSE)
    expect_identical(validate$reason_code(validate$check_demand_sufficiency(one_col)), "too_few_price_columns")
    long <- data.frame(id = c(1, 1, 2), x = c(1, 2, 1), y = c(5, 4, 3))
    expect_identical(
      validate$reason_code(validate$check_demand_sufficiency(long)),
      "insufficient_price_points"
    )
  })

  it("names the discounting failures", {
    expect_identical(code_for(data.frame(subjectid = 1, q1 = 1), "discounting"), "mcq_shape")
    expect_identical(code_for(data.frame(responseid = "R1", foo = 1), "discounting"), "qualtrics_template")
    expect_identical(code_for(data.frame(id = 1, x = 1, y = 1, z = 1), "discounting"), "ip_three_columns")
    expect_identical(code_for(data.frame(id = 1, a = 1), "discounting"), "wide_ip_needs_delays")
    expect_identical(
      code_for(data.frame(id = 1, a = 1, b = 2), "discounting"),
      "unparsable_delay_headers"
    )
    expect_identical(
      code_for(data.frame(id = 1, `1` = 1, `1.0` = 2, check.names = FALSE), "discounting"),
      "duplicate_delay_headers"
    )
    expect_identical(
      code_for(data.frame(id = 1, `0` = 1, `2` = 2, check.names = FALSE), "discounting"),
      "nonpositive_delays"
    )
    expect_identical(
      code_for(data.frame(id = c(1, 1), `1` = 1:2, `2` = 1:2, check.names = FALSE), "discounting"),
      "wide_ip_one_row_per_id"
    )
    expect_identical(code_for(data.frame(foo = 1), "discounting"), "unrecognised_discounting_format")
  })

  it("names the mixed-effects failures", {
    expect_identical(code_for(data.frame(x = 1, y = 1), "mixed_effects_demand"), "me_missing_id")
    expect_identical(code_for(data.frame(id = 1, y = 1), "mixed_effects_demand"), "me_missing_x")
    expect_identical(code_for(data.frame(id = 1, x = 1), "mixed_effects_demand"), "me_missing_y")
  })

  it("keeps the message text unchanged", {
    msg <- validate$check_data(data.frame(id = 1:2, a = 1:2, b = 3:4), "demand")
    expect_match(as.character(msg), 'Could not parse these price headers: "a", "b"', fixed = TRUE)
  })
})
