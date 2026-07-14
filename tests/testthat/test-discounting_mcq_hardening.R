# Regression tests for the MCQ scoring cluster in the 2026-07-01 triage.
#
# 1155c2d9 (21 events — the second-largest signature):
#     Impute method must be one of none, ggm, GGM, inn, INN
#   resolve_imputation() guarded only is.null(), so an empty-string value from an
#   uninitialised selectInput sailed through to beezdiscounting and aborted.
#
# Batch-2 finding #4:
#     Response length \n not equal to 27 \n for subjectid: 1
#   score_mcq27() aborts unless every subject has exactly 27 rows.
#
# ...and the translator bug that hid both from users: friendly_discounting_error() matches
# with grepl(fixed = TRUE), but beezdiscounting's real messages are wrapped across lines with
# runs of padding spaces, so the patterns NEVER matched and every user saw the generic
# "An error occurred during scoring:" fallback. The pre-existing test for this passed a clean
# single-line string, which is exactly why the bug survived.

box::use(
  testthat[...],
)

box::use(
  app / logic / discounting / scoring,
)

# The message beezdiscounting actually emits (captured verbatim from score_mcq27), as opposed
# to the tidied-up single-line version the old test used.
real_response_length_error <- function() {
  paste0(
    "Response length\n",
    "                                                      not equal to 27\n",
    "                                                      for subjectid: 1"
  )
}

mcq_long <- function(n_subjects = 2) {
  data.frame(
    subjectid = rep(seq_len(n_subjects), each = 27),
    questionid = rep(1:27, n_subjects),
    response = rep(c(1L, 2L), length.out = 27 * n_subjects)
  )
}

describe("resolve_imputation", {
  it("treats an empty imputation value as none (1155c2d9)", {
    # An uninitialised selectInput yields "", which beezdiscounting rejects outright.
    expect_identical(scoring$resolve_imputation("")$impute_method, "none")
  })

  it("treats an unrecognised imputation value as none", {
    expect_identical(scoring$resolve_imputation("bogus")$impute_method, "none")
    expect_identical(scoring$resolve_imputation(NA)$impute_method, "none")
  })

  it("still resolves NULL to none", {
    expect_identical(scoring$resolve_imputation(NULL)$impute_method, "none")
  })

  # Pin the UI options to the Yeh et al. (2023) methods they are meant to select
  # (doi:10.1371/journal.pone.0292258). `random` is a separate boolean argument to
  # score_mcq27 — "insert a random draw (0 or 1) for NAs" — NOT an imputation method, so the
  # whitelist must constrain impute_method without collapsing the INN-with-random option.
  it("maps each UI option to its Yeh et al. method and random flag", {
    expected <- list(
      none = list(impute_method = "none", random = FALSE),
      GGM = list(impute_method = "GGM", random = FALSE),
      INN = list(impute_method = "INN", random = FALSE),
      INN_random = list(impute_method = "INN", random = TRUE)
    )

    for (ui_value in names(expected)) {
      resolved <- scoring$resolve_imputation(ui_value)
      expect_identical(
        resolved$impute_method, expected[[ui_value]]$impute_method,
        info = ui_value
      )
      expect_identical(resolved$random, expected[[ui_value]]$random, info = ui_value)
    }
  })
})

describe("friendly_discounting_error", {
  it("translates the real multi-line response-length message", {
    # Pre-fix this fell through to the generic fallback, because the real message wraps
    # across lines and grepl(fixed = TRUE) could never match the flat pattern.
    msg <- scoring$friendly_discounting_error(real_response_length_error())

    expect_match(msg, "exactly 27 items", fixed = TRUE)
    expect_no_match(msg, "An error occurred during scoring", fixed = TRUE)
  })

  it("still translates a clean single-line message", {
    msg <- scoring$friendly_discounting_error("Response length not equal to 27")
    expect_match(msg, "exactly 27 items", fixed = TRUE)
  })

  it("translates a wrapped impute-method message", {
    msg <- scoring$friendly_discounting_error(
      "Impute method must be one of\n   none, ggm, GGM, inn, INN"
    )
    expect_match(msg, "Invalid imputation method", fixed = TRUE)
  })

  it("still falls back for genuinely unknown errors", {
    msg <- scoring$friendly_discounting_error("something completely unknown")
    expect_match(msg, "An error occurred during scoring", fixed = TRUE)
  })
})

describe("score_and_format_mcq", {
  it("names the offending subject and its row count", {
    # Subject 1 is one item short. The raw abort says only "for subjectid: 1"; the user
    # deserves to be told how many items it actually has.
    short <- mcq_long(2)[-1, ]

    expect_error(
      scoring$score_and_format_mcq(short),
      "Subject 1 has 26 of the 27 required MCQ items",
      fixed = TRUE
    )
  })

  it("reports every offending subject, not just the first", {
    short <- mcq_long(3)
    short <- short[-c(1, 28), ] # subjects 1 and 2 each lose an item

    err <- expect_error(scoring$score_and_format_mcq(short))
    expect_match(conditionMessage(err), "Subject 1", fixed = TRUE)
    expect_match(conditionMessage(err), "Subject 2", fixed = TRUE)
  })

  it("scores a complete 27-item dataset", {
    expect_no_error(scoring$score_and_format_mcq(mcq_long(2)))
  })
})
