box::use(
  app/logic/mixed_effects/emms_utils,
  app/logic/mixed_effects/systematic_utils,
)

# CLOSE-OUTS for three telemetry signatures the 2026-07-01 triage rated Low
# confidence. None could be reproduced against current develop source; the guard
# that prevents each one is already present. Rather than invent a change to
# justify a ticket, these tests PIN the existing guards so they cannot regress
# silently, and the tickets are closed.
#
#   a588d550  "object 'sys_result' not found"
#             discounting_data_table.R:156 binds `sys_result <- NULL` at the top of
#             the renderDT block, before any branch, and req()s it at :179. The
#             symbol is always bound. Not unit-testable (it is a renderDT reactive)
#             and not reproducible. CLOSED.
#
#   5a8206d0  "undefined columns selected"
#             The triage said the call site was unconfirmed. Every base-R column
#             selection that could raise it is guarded - the two below are the
#             plausible sites, and the correlation table derives k_cols from
#             names() and req()s a non-empty result. CLOSED; guards pinned here.
#
#   abc75312  startup "argument is of length zero"
#             The investigation states the cause is UNKNOWN. Last occurrence
#             2026-03-27. No reproduction, so no test and no speculative req().
#             CLOSED as unreproducible - see the handoff: "Reproduce it or drop
#             it; do not guess at a req()."

describe("undefined columns selected - the guards that prevent it", {
  it("compute_systematic_criteria returns NULL when the id/x/y columns are absent", {
    df <- data.frame(subject = c("a", "b"), price = c(1, 2), consumption = c(9, 8))

    res <- systematic_utils$compute_systematic_criteria(
      df_raw = df, id_col = "id", x_col = "x", y_col = "y"
    )

    expect_null(res)
  })

  it("compute_systematic_criteria returns NULL when a grouping column is absent", {
    df <- data.frame(id = c("a", "a"), x = c(1, 2), y = c(9, 8))

    res <- systematic_utils$compute_systematic_criteria(
      df_raw = df, id_col = "id", x_col = "x", y_col = "y",
      group_vars = "not_a_column"
    )

    expect_null(res)
  })

  it("emms column selection intersects with the frame, never selecting a missing name", {
    emms <- data.frame(
      dose = factor(c("lo", "hi")),
      Q0 = c(10, 12),
      stringsAsFactors = FALSE
    )

    # asking for a column that does not exist must not raise
    expect_no_error(
      res <- emms_utils$extract_q0_columns(emms, factor_cols = c("dose", "ghost"))
    )
    expect_false("ghost" %in% names(res))
  })
})
