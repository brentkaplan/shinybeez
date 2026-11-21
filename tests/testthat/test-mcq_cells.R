# The 27-Item MCQ is a binary questionnaire: 0 for the smaller-sooner reward, 1 for the
# larger-later one. The upload check looked at the shape of the file only. A response of 2,
# 0.5, -1 or 11 then reached the scorer, which sums responses, and came back as a
# consistency above 1 or a different k with nothing said. A text cell aborted later with
# "invalid 'type' (character) of argument" (long) or "Can't combine <double> and
# <character>" (wide).

box::use(
  testthat[...],
)

box::use(
  app / logic / validate,
)

mcq_long <- function(response = rep(c(0, 1, 1), 9)) {
  data.frame(
    subjectid = rep(c("s1", "s2"), each = 27), questionid = rep(1:27, 2),
    response = rep(response, 2), stringsAsFactors = FALSE
  )
}

mcq_wide <- function() {
  dat <- as.data.frame(matrix(rep(c(0, 1, 1), 18), nrow = 2, byrow = TRUE))
  names(dat) <- as.character(1:27)
  cbind(subjectid = c("s1", "s2"), dat, stringsAsFactors = FALSE)
}

describe("check_data() on 27-Item MCQ responses", {
  it("accepts 0, 1 and missing responses, long and wide", {
    expect_true(validate$check_data(mcq_long(), type = "discounting"))
    expect_true(validate$check_data(mcq_long(c(0, NA, 1)), type = "discounting"))
    expect_true(validate$check_data(mcq_wide(), type = "discounting"))
    wide <- mcq_wide()
    wide[1, "5"] <- NA
    expect_true(validate$check_data(wide, type = "discounting"))
  })

  it("accepts 0 and 1 typed as text", {
    dat <- mcq_long()
    dat$response <- as.character(dat$response)
    expect_true(validate$check_data(dat, type = "discounting"))
  })

  for (bad in c(2, 0.5, -1, 11)) {
    it(paste("rejects a long response of", bad, "and names it"), {
      dat <- mcq_long()
      dat$response[3] <- bad
      res <- validate$check_data(dat, type = "discounting")
      expect_identical(validate$reason_code(res), "mcq_response_not_0_or_1")
      expect_match(res, paste0("\"", bad, "\""), fixed = TRUE)
    })
  }

  it("rejects a text response, long", {
    dat <- mcq_long()
    dat$response <- as.character(dat$response)
    dat$response[3] <- "yes"
    res <- validate$check_data(dat, type = "discounting")
    expect_identical(validate$reason_code(res), "mcq_response_not_0_or_1")
    expect_match(res, "\"yes\"", fixed = TRUE)
  })

  it("rejects a wide cell that is not 0 or 1, naming the item column", {
    wide <- mcq_wide()
    wide[["3"]] <- as.character(wide[["3"]])
    wide[1, "3"] <- "1,234"
    wide[2, "7"] <- 2
    res <- validate$check_data(wide, type = "discounting")
    expect_identical(validate$reason_code(res), "mcq_response_not_0_or_1")
    expect_match(res, "Column \"3\"", fixed = TRUE)
    expect_match(res, "\"1,234\"", fixed = TRUE)
    expect_match(res, "Column \"7\"", fixed = TRUE)
  })

  it("still reports a blank subjectid first in a wide file", {
    wide <- mcq_wide()
    wide$subjectid[2] <- ""
    wide[1, "3"] <- 2
    res <- validate$check_data(wide, type = "discounting")
    expect_identical(validate$reason_code(res), "mcq_wide_blank_subjectid")
  })
})
