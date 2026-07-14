box::use(
  app/logic/discounting/five_trial,
  app/logic/validate,
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

# Reproduce app/view/file_input.R's discounting path EXACTLY: lowercase the names,
# then run the empty-column step - which must now SKIP Qualtrics/MCQ, because an
# unanswered branch of the 5.5-trial adaptive tree is an entirely empty item
# column and beezdiscounting still selects all of I1-I31 by name.
#
# An earlier version of this helper only lowercased. That omission is exactly why
# these tests passed while the app could not score its own bundled template:
# obliterate_empty_cols() strips 17 of the 31 DD item columns.
as_uploaded <- function(dat) {
  colnames(dat) <- trimws(tolower(colnames(dat)))
  if (!validate$preserves_empty_cols(dat)) {
    dat <- validate$obliterate_empty_cols(dat)
  }
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

  it("rejects a Qualtrics file with items and attention checks but no timing", {
    dat <- as.data.frame(
      c(
        list(responseid = c("r1", "r2")),
        stats::setNames(rep(list(c(1, 2)), 31), paste0("i", 1:31)),
        stats::setNames(rep(list(c(1, 2)), 2), c("attend-ll", "attend-ss"))
      ),
      check.names = FALSE
    )

    res <- five_trial$validate_five_trial(dat)

    expect_type(res, "character")
    expect_match(res, "timing", ignore.case = TRUE)
  })

  it("rejects a file missing the attention checks", {
    dat <- as_uploaded(qualtrics_fixture("dd"))
    dat <- dat[, !names(dat) %in% c("attend-ss", "attend-ll"), drop = FALSE]

    res <- five_trial$validate_five_trial(dat)

    expect_type(res, "character")
    expect_match(res, "Attend-SS", fixed = TRUE)
  })

  it("rejects a file missing some of the four Timing measures", {
    dat <- as_uploaded(qualtrics_fixture("dd"))
    dat <- dat[, !grepl("timing_click count", names(dat), fixed = TRUE), drop = FALSE]

    res <- five_trial$validate_five_trial(dat)

    expect_type(res, "character")
    expect_match(res, "Click Count", ignore.case = TRUE)
  })

  it("rejects a Timing measure whose values are all NA", {
    # Headers present but no usable data: timing_dd() drops incomplete cases, the
    # measure vanishes from the spread, and beezdiscounting aborts with
    # "Location 7 doesn't exist. There are only 6 columns."
    dat <- as_uploaded(qualtrics_fixture("dd"))
    first_click <- grep("timing_first click", names(dat), fixed = TRUE)
    dat[first_click] <- NA

    res <- five_trial$validate_five_trial(dat)

    expect_type(res, "character")
    expect_match(res, "First Click", fixed = TRUE)
    expect_match(res, "usable", fixed = TRUE)
  })

  it("rejects a file with no rows", {
    dat <- as_uploaded(qualtrics_fixture("dd"))[0, , drop = FALSE]

    res <- five_trial$validate_five_trial(dat)

    expect_type(res, "character")
    expect_match(res, "no rows", fixed = TRUE)
  })

  it("rejects duplicate ResponseId values", {
    # calc_dd joins on ResponseId; duplicates produce the unique-key abort.
    dat <- as_uploaded(qualtrics_fixture("dd"))
    dat <- rbind(dat, dat[1, , drop = FALSE])

    res <- five_trial$validate_five_trial(dat)

    expect_type(res, "character")
    expect_match(res, "duplicate", ignore.case = TRUE)
  })

  it("rejects duplicate column names that differ only by case", {
    # A file with both "ResponseId" and "responseid" survives vroom; the uploader
    # lowercases both, case restoration then makes two ResponseId columns, and
    # calc_dd aborts with "Names must be unique".
    dat <- as_uploaded(qualtrics_fixture("dd"))
    dat <- cbind(dat, responseid = dat$responseid)

    res <- five_trial$validate_five_trial(dat)

    expect_type(res, "character")
    expect_match(res, "duplicate column name", ignore.case = TRUE)
  })

  it("rejects non-numeric Timing values", {
    # One junk entry makes vroom read the whole column as character while its
    # neighbours stay double; pivot_longer() then cannot combine the two types.
    dat <- as_uploaded(qualtrics_fixture("dd"))
    col <- grep("timing_first click", names(dat), fixed = TRUE)[1]
    dat[[col]] <- as.character(dat[[col]])
    dat[[col]][1] <- "not-a-number"

    res <- five_trial$validate_five_trial(dat)

    expect_type(res, "character")
    expect_match(res, "non-numeric", fixed = TRUE)
  })

  it("rejects a blank ResponseId", {
    dat <- as_uploaded(qualtrics_fixture("dd"))
    dat$responseid[1] <- NA

    res <- five_trial$validate_five_trial(dat)

    expect_type(res, "character")
    expect_match(res, "blank", ignore.case = TRUE)
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

describe("empty columns are structural for Qualtrics and MCQ", {
  it("keeps every item column of the DD template through the upload path", {
    # obliterate_empty_cols() would strip 17 of the 31 item columns - unanswered
    # branches of the adaptive tree - and beezdiscounting selects all 31 by name.
    uploaded <- as_uploaded(qualtrics_fixture("dd"))

    items <- grep("^i[0-9]+$", names(uploaded), value = TRUE)
    expect_equal(length(items), 31)
  })

  it("flags Qualtrics and MCQ frames as preserving empty columns", {
    expect_true(validate$preserves_empty_cols(data.frame(responseid = "r1")))
    expect_true(validate$preserves_empty_cols(data.frame(subjectid = 1)))
  })

  it("still obliterates empty columns for ordinary discounting data", {
    expect_false(
      validate$preserves_empty_cols(data.frame(id = "a", x = 1, y = 2))
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
