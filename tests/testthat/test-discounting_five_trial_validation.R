box::use(
  app/logic/discounting/five_trial,
)

# Two defects in five_trial.R, both in the column-name handling:
#
#  1. NO FORMAT VALIDATION (signature 9a74502f, 8 events).
#     restore_qualtrics_case() renamed responseid -> ResponseId and i<n> -> I<n>
#     without ever checking those columns exist. A wrong-format upload sailed
#     straight into beezdiscounting::calc_dd/calc_pd and died there with
#     "Can't select columns that don't exist. Column `ResponseId` doesn't exist."
#
#  2. INCOMPLETE CASE RESTORATION (the known calc_dd unique-key error).
#     file_input.R lowercases every column name on upload, but the restore only
#     handled ResponseId and the BARE item columns - never the timing columns
#     ("I16-Timing_First Click") or the attention checks ("Attend-LL"). Inside
#     beezdiscounting, timing_dd()'s gsub is case-sensitive, so those columns were
#     never renamed, separate() then split them wrong, and calc_dd aborted with
#     "Each row of output must be identified by a unique combination of keys".
#     Valid Qualtrics uploads were broken.

qualtrics_fixture <- function(task = c("dd", "pd")) {
  task <- match.arg(task)
  path <- file.path(
    "..", "..", "app", "static", "data", "examples",
    paste0("shinybeez-ex-five.fivetrial_", task, ".csv")
  )
  utils::read.csv(path, check.names = FALSE)
}

# file_input.R lowercases and trims every column name before storing the frame.
as_uploaded <- function(dat) {
  colnames(dat) <- trimws(tolower(colnames(dat)))
  dat
}

describe("validate_five_trial", {
  it("rejects a non-Qualtrics file naming the missing ResponseId", {
    dat <- data.frame(id = c("a", "b"), x = c(1, 2), y = c(0.5, 0.4))

    res <- five_trial$validate_five_trial(dat)

    expect_type(res, "character")
    expect_match(res, "ResponseId", fixed = TRUE)
    # the raw beezdiscounting abort must NOT be what the user sees
    expect_false(grepl("Can't select columns", res, fixed = TRUE))
  })

  it("rejects a Qualtrics file missing item columns, naming them", {
    dat <- data.frame(responseid = c("r1", "r2"), i1 = c(1, 2), i2 = c(1, 2))

    res <- five_trial$validate_five_trial(dat)

    expect_type(res, "character")
    expect_match(res, "I3", fixed = TRUE)
    expect_match(res, "I31", fixed = TRUE)
  })

  it("rejects a Qualtrics file with items but no timing columns", {
    dat <- as.data.frame(
      c(
        list(responseid = c("r1", "r2")),
        stats::setNames(
          rep(list(c(1, 2)), 31),
          paste0("i", 1:31)
        )
      )
    )

    res <- five_trial$validate_five_trial(dat)

    expect_type(res, "character")
    expect_match(res, "timing", ignore.case = TRUE)
  })

  it("accepts the real DD template as it arrives from the uploader", {
    expect_true(
      five_trial$validate_five_trial(as_uploaded(qualtrics_fixture("dd")))
    )
  })

  it("accepts the real PD template as it arrives from the uploader", {
    expect_true(
      five_trial$validate_five_trial(as_uploaded(qualtrics_fixture("pd")))
    )
  })
})

describe("restore_qualtrics_case", {
  it("restores timing and attention-check columns, not just the items", {
    dat <- as_uploaded(qualtrics_fixture("dd"))

    restored <- names(five_trial$restore_qualtrics_case(dat))

    expect_true("ResponseId" %in% restored)
    expect_true("I16" %in% restored)
    expect_true("I16-Timing_First Click" %in% restored)
    expect_true("Attend-LL" %in% restored)
    expect_true("Attend-SS-Timing_Page Submit" %in% restored)
    # nothing left lowercased from the known Qualtrics vocabulary
    expect_false(any(grepl("^i[0-9]+-timing", restored)))
    expect_false(any(restored == "responseid"))
  })

  it("leaves unknown columns untouched", {
    dat <- data.frame(responseid = "r1", something_custom = 1)
    restored <- names(five_trial$restore_qualtrics_case(dat))
    expect_true("something_custom" %in% restored)
  })
})

describe("5.5-Trial scoring through the real upload path", {
  it("scores DD from a lowercased upload, matching the original-case result", {
    uploaded <- as_uploaded(qualtrics_fixture("dd"))

    res <- suppressWarnings(five_trial$compute_five_trial_dd(uploaded))
    reference <- suppressWarnings(
      five_trial$compute_five_trial_dd(qualtrics_fixture("dd"))
    )

    expect_s3_class(res, "data.frame")
    expect_gt(nrow(res), 0)
    expect_equal(nrow(res), nrow(reference))
    expect_equal(sum(!is.na(res$kval)), sum(!is.na(reference$kval)))
  })

  it("scores PD from a lowercased upload", {
    uploaded <- as_uploaded(qualtrics_fixture("pd"))

    res <- suppressWarnings(five_trial$compute_five_trial_pd(uploaded))

    expect_s3_class(res, "data.frame")
    expect_gt(nrow(res), 0)
  })

  it("gives a friendly error, not a beezdiscounting abort, on a bad file", {
    dat <- data.frame(id = c("a", "b"), x = c(1, 2), y = c(0.5, 0.4))

    expect_error(five_trial$compute_five_trial_dd(dat), "ResponseId")
    expect_error(five_trial$compute_five_trial_pd(dat), "ResponseId")
  })
})
