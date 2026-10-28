# Codex Finding 1: check_discounting_data() accepted malformed schemas.
#
# The old guard passed a frame if ANY of subjectid/responseid/id/x/y was present,
# then only tightened WITHIN the subjectid/responseid/id branches. Two shapes slipped
# through and deferred their failure to the scoring/model code (the exact
# crash-instead-of-friendly-error class this whole backlog exists to remove):
#
#   1. An "x,y"-only frame (no id column) matched the outer any() on x/y, matched
#      none of the branches, and returned TRUE.
#   2. A three-column "subjectid,foo,bar" frame matched the subjectid branch's
#      `ncol == 3` test without ever being the real subjectid,questionid,response
#      MCQ-long shape.
#
# check_data() lowercases column names before dispatch, so these assertions use
# lowercased names; a mixed-case case is included to pin that path too.

box::use(
  testthat[...],
)

box::use(
  app / logic / validate,
)

describe("check_discounting_data - rejects malformed schemas (Finding 1)", {
  it("rejects an x,y frame with no identifier column", {
    df <- data.frame(x = c(0.01, 0.1), y = c(100, 80))
    res <- validate$check_data(df, type = "discounting")
    expect_type(res, "character")
  })

  it("rejects a y-only frame", {
    df <- data.frame(y = c(100, 80))
    res <- validate$check_data(df, type = "discounting")
    expect_type(res, "character")
  })

  it("rejects a three-column subjectid frame that is not MCQ long", {
    df <- data.frame(subjectid = c(1, 1), foo = c(2, 3), bar = c(4, 5))
    res <- validate$check_data(df, type = "discounting")
    expect_type(res, "character")
  })

  it("rejects a frame with none of the recognised identifier columns", {
    df <- data.frame(alpha = c(1, 2), beta = c(3, 4), gamma = c(5, 6))
    res <- validate$check_data(df, type = "discounting")
    expect_type(res, "character")
  })

  it("rejects a 28-column MCQ-wide frame whose subjectid is not first", {
    # wide_to_long_mcq() pivots columns 2 onward and treats column 1 as the
    # subject id, so subjectid must be first or the reshape is silently wrong.
    df <- as.data.frame(matrix(1, nrow = 2, ncol = 28))
    colnames(df) <- c("foo", "subjectid", as.character(1:26))
    res <- validate$check_data(df, type = "discounting")
    expect_type(res, "character")
  })

  it("rejects a reordered MCQ-long frame (questionid, subjectid, response)", {
    # The uploader classifies MCQ-long with an ordered identical() check; a
    # reordered frame would otherwise pass validation and be routed to the
    # standard-data branch, whose NA-row dropping defeats MCQ imputation.
    df <- data.frame(
      questionid = 1:3, subjectid = rep(1, 3), response = c(1, 2, 1)
    )
    res <- validate$check_data(df, type = "discounting")
    expect_type(res, "character")
  })
})

describe("check_discounting_data - still accepts every valid shape", {
  it("accepts long indifference-point id,x,y (mixed case)", {
    df <- data.frame(ID = c("p1", "p1"), X = c(0.01, 0.1), Y = c(100, 80))
    expect_true(validate$check_data(df, type = "discounting"))
  })

  it("accepts MCQ long: subjectid, questionid, response", {
    df <- data.frame(
      subjectid = rep(1, 3), questionid = 1:3, response = c(1, 2, 1)
    )
    expect_true(validate$check_data(df, type = "discounting"))
  })

  it("accepts MCQ wide: subjectid + 27 item columns (28 cols)", {
    df <- as.data.frame(matrix(1, nrow = 2, ncol = 28))
    colnames(df) <- c("subjectid", as.character(1:27))
    df$subjectid <- c(1, 2) # one row per subject; the old fixture repeated id 1
    expect_true(validate$check_data(df, type = "discounting"))
  })

  it("accepts a Qualtrics candidate: responseid + at least one item column", {
    df <- data.frame(responseid = c(1, 2), i1 = c("a", "b"), i16 = c("c", "d"))
    expect_true(validate$check_data(df, type = "discounting"))
  })
})

describe("check_discounting_data - MCQ wide must be one row per subject", {
  # 2026-10-28 (signatures a24ff1e1 and d1413265): wide MCQ uploads with a repeated
  # subjectid passed the 28-column check and aborted inside
  # beezdiscounting::wide_to_long_mcq(), which sizes questionid by the number of DISTINCT
  # subjects while the pivot yields one block of 27 per ROW. The abort was raised in the
  # eager reshape observer, so it ended the session instead of showing a message.
  mcq_wide <- function(ids) {
    items <- as.data.frame(matrix(1L, nrow = length(ids), ncol = 27))
    colnames(items) <- as.character(1:27)
    cbind(data.frame(subjectid = ids, stringsAsFactors = FALSE), items)
  }

  it("rejects a repeated subjectid and names only the repeated id", {
    res <- validate$check_data(mcq_wide(c("s1", "s2", "s1")), type = "discounting")
    expect_type(res, "character")
    expect_match(res, "one row per subjectid", fixed = TRUE)
    expect_match(res, "\"s1\"", fixed = TRUE)
    expect_no_match(res, "\"s2\"", fixed = TRUE)
    expect_identical(validate$reason_code(res), "mcq_wide_duplicate_subjectid")
  })

  it("rejects repeated numeric subjectids", {
    res <- validate$check_data(mcq_wide(c(1, 2, 2)), type = "discounting")
    expect_identical(validate$reason_code(res), "mcq_wide_duplicate_subjectid")
    expect_match(res, "\"2\"", fixed = TRUE)
  })

  it("rejects NA and blank subjectids before looking for duplicates", {
    for (ids in list(c("s1", NA), c("s1", ""), c("s1", "  "), c(NA, NA))) {
      res <- validate$check_data(mcq_wide(ids), type = "discounting")
      expect_identical(validate$reason_code(res), "mcq_wide_blank_subjectid")
      expect_match(res, "missing or blank subjectid", fixed = TRUE)
    }
    expect_match(
      validate$check_data(mcq_wide(c(NA, NA)), type = "discounting"),
      "2 row(s)", fixed = TRUE
    )
  })

  it("lists at most ten repeated ids and counts the rest", {
    ids <- c(sprintf("p%02d", 1:30), sprintf("p%02d", 1:30))
    res <- validate$check_data(mcq_wide(ids), type = "discounting")
    expect_identical(validate$reason_code(res), "mcq_wide_duplicate_subjectid")
    expect_identical(lengths(regmatches(res, gregexpr("\"p[0-9]{2}\"", res))), 10L)
    expect_match(res, "and 20 more", fixed = TRUE)
  })

  it("still accepts one row per subject, including the bundled template", {
    expect_true(validate$check_data(mcq_wide(c("s1", "s2", "s3")), type = "discounting"))
    path <- file.path(find_project_root(), "app/static/data/templates/template_mcq27_wide.csv")
    template <- read.csv(path, check.names = FALSE)
    expect_true(validate$check_data(template, type = "discounting"))
  })

  it("stops the exported reshape with the same message for a direct caller", {
    err <- expect_error(validate$reshape_data(mcq_wide(c("s1", "s1")), type = "discounting"))
    expect_match(conditionMessage(err), "one row per subjectid", fixed = TRUE)
  })

  it("documents the package abort the guard now prevents (production shape)", {
    # 505 wide rows over 443 distinct ids: 505 * 27 = 13635 pivoted rows against a
    # questionid of 443 * 27 = 11961 values (signature a24ff1e1).
    skip_if(
      packageVersion("beezdiscounting") >= "0.4.0.9000",
      "wide_to_long_mcq() numbers items by position from 0.4.0.9000"
    )
    dat <- mcq_wide(c(sprintf("s%03d", 1:443), sprintf("s%03d", 1:62)))
    expect_error(beezdiscounting::wide_to_long_mcq(dat), "13635")
    expect_identical(
      validate$reason_code(validate$check_data(dat, type = "discounting")),
      "mcq_wide_duplicate_subjectid"
    )
  })
})
