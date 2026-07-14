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
    expect_true(validate$check_data(df, type = "discounting"))
  })

  it("accepts a Qualtrics candidate: responseid + at least one item column", {
    df <- data.frame(responseid = c(1, 2), i1 = c("a", "b"), i16 = c("c", "d"))
    expect_true(validate$check_data(df, type = "discounting"))
  })
})
