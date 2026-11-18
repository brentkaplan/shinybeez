# parse_single_number(): a text cell is a number only when it holds exactly one. A unit or
# currency around it is fine ("6 drinks", "$1"); two numbers, a decimal comma, a date or a
# comparison is not something to guess at. readr::parse_number() read "7,5" as 75, "1-2" as
# 1 and "2026-01-01" as 2026.
#
# Wide demand files stop at upload when a consumption cell is not a number, naming the cell
# (test-demand_wide_cells.R). A long id, x, y file with the same cell passed validation, and
# retype_data() then turned it into NA without a word: the upload's missing-row check had
# already run, so nothing told the user a response was dropped.

box::use(
  testthat[...],
)

box::use(
  app / logic / validate,
)

describe("parse_single_number", {
  it("reads a cell that holds one number, whatever surrounds it", {
    cells <- c("6 drinks", "$1", "R$0.50", "0.50€", "-2", "1e-1", " 3 ", "50%", ".5", "7.")
    expect_equal(validate$parse_single_number(cells), c(6, 1, 0.5, 0.5, -2, 0.1, 3, 50, 0.5, 7))
  })

  it("does not guess when a cell holds more than one number", {
    cells <- c("7,5", "1,234", "2026-01-01", "1-2", "10.5.2", "1 000", "3 of 10")
    expect_identical(validate$parse_single_number(cells), rep(NA_real_, length(cells)))
  })

  it("does not read a comparison as the number in it", {
    cells <- c("<1", ">5", "≤1", "~3")
    expect_identical(validate$parse_single_number(cells), rep(NA_real_, length(cells)))
  })

  it("gives NA for text without a number and for missing cells", {
    cells <- c("n/a", "abc", "TRUE", "", "NA", NA)
    expect_identical(validate$parse_single_number(cells), rep(NA_real_, length(cells)))
  })

  it("returns numeric input as it is and reads a factor by its labels", {
    expect_identical(validate$parse_single_number(c(1.5, NA, 0)), c(1.5, NA, 0))
    expect_equal(validate$parse_single_number(factor(c("$5", "10"))), c(5, 10))
  })
})

long_demand <- function(y) {
  data.frame(
    id = rep(c("p1", "p2"), each = 3), x = rep(c(0.5, 1, 5), 2), y = y,
    stringsAsFactors = FALSE
  )
}

describe("check_data() on a long demand file with a cell that is not a number", {
  it("rejects \"n/a\" consumption, naming the column and the cell", {
    res <- validate$check_data(long_demand(c("10", "8", "2", "9", "n/a", "1")), type = "demand")
    expect_type(res, "character")
    expect_identical(validate$reason_code(res), "long_demand_nonnumeric_cells")
    expect_match(res, "\"y\"", fixed = TRUE)
    expect_match(res, "\"n/a\"", fixed = TRUE)
  })

  it("rejects \"<1\" and a decimal comma rather than guessing a number", {
    res <- validate$check_data(long_demand(c("10", "8", "<1", "9", "7,5", "1")), type = "demand")
    expect_identical(validate$reason_code(res), "long_demand_nonnumeric_cells")
    expect_match(res, "\"<1\"", fixed = TRUE)
    expect_match(res, "\"7,5\"", fixed = TRUE)
  })

  it("rejects the same cell when the file has a group column", {
    dat <- long_demand(c("10", "8", "2", "9", "n/a", "1"))
    dat <- data.frame(
      id = dat$id, group = "a", x = dat$x, y = dat$y, stringsAsFactors = FALSE
    )
    res <- validate$check_data(dat, type = "demand")
    expect_identical(validate$reason_code(res), "long_demand_nonnumeric_cells")
  })

  it("rejects a price cell that is not one number, naming it", {
    dat <- long_demand(c(10, 8, 2, 9, 7, 1))
    dat$x <- c("0.5", "1", "5", "0,5", "1-2", "5")
    res <- validate$check_data(dat, type = "demand")
    expect_identical(validate$reason_code(res), "long_demand_nonnumeric_cells")
    expect_match(res, "\"x\"", fixed = TRUE)
    expect_match(res, "\"0,5\"", fixed = TRUE)
    expect_match(res, "\"1-2\"", fixed = TRUE)
  })

  it("accepts numeric consumption", {
    expect_true(validate$check_data(long_demand(c(10, 8, 2, 9, 7, 1)), type = "demand"))
  })

  it("accepts consumption typed as text when every cell is a number or empty", {
    res <- validate$check_data(long_demand(c("10", "8", "2", "9", "", "1e0")), type = "demand")
    expect_true(res)
  })

  it("still accepts a currency price, which the price parser reads", {
    dat <- long_demand(c(10, 8, 2, 9, 7, 1))
    dat$x <- rep(c("$0.50", "$1", "$5"), 2)
    expect_true(validate$check_data(dat, type = "demand"))
  })
})

describe("retype_data() on long prices typed as text", {
  it("reads a currency price and never turns a decimal comma into a larger number", {
    dat <- data.frame(id = "p1", x = c("$0.50", "1", "7,5"), y = c(10, 8, 2))
    out <- suppressWarnings(validate$retype_data(dat))
    expect_identical(out$x, c(0.5, 1, NA))
  })
})

describe("check_data() on a long indifference-point file", {
  long_ip <- function(y, x = rep(c(1, 7, 30), 2)) {
    data.frame(id = rep(c("p1", "p2"), each = 3), x = x, y = y, stringsAsFactors = FALSE)
  }

  it("rejects an indifference point that is not a number, naming the cell", {
    res <- validate$check_data(
      long_ip(c("0.9", "0.6", "n/a", "0.8", "0,5", "0.1")),
      type = "discounting"
    )
    expect_identical(validate$reason_code(res), "long_ip_nonnumeric_cells")
    expect_match(res, "\"n/a\"", fixed = TRUE)
    expect_match(res, "\"0,5\"", fixed = TRUE)
  })

  it("rejects a delay that is not one number", {
    res <- validate$check_data(
      long_ip(c(0.9, 0.6, 0.2, 0.8, 0.5, 0.1), x = c("1", "7", "30", "1", "1-7", "30")),
      type = "discounting"
    )
    expect_identical(validate$reason_code(res), "long_ip_nonnumeric_cells")
    expect_match(res, "\"1-7\"", fixed = TRUE)
  })

  it("accepts numeric indifference points and delays", {
    expect_true(
      validate$check_data(long_ip(c(0.9, 0.6, 0.2, 0.8, 0.5, 0.1)), type = "discounting")
    )
  })
})
