# A single text cell makes the upload reader type a whole price column as character, and
# pivot_demand_data() then aborts with "Can't combine `1` <double> and `5` <character>". The
# demand navpanel catches that, so the session survives, but the user was shown the raw vctrs
# message. If every price column was character the pivot succeeded and retype_data() turned
# each unreadable cell into NA without a word. Both now stop at upload, naming the cell, as
# wide indifference-point files have since 1.4.0.

box::use(
  testthat[...],
)

box::use(
  app / logic / validate,
)

wide_demand <- function() {
  data.frame(
    id = c("p1", "p2"), `0.5` = c(10, 9), `1` = c(8, 7), `5` = c(2, 1),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

describe("check_data - wide demand value cells", {
  it("rejects a text cell, naming the column and the cell", {
    dat <- wide_demand()
    dat[["1"]] <- c("8", "n/a")
    res <- validate$check_data(dat, type = "demand")
    expect_type(res, "character")
    expect_match(res, 'Column "1" has values that are not numbers: "n/a"', fixed = TRUE)
    expect_identical(validate$reason_code(res), "wide_demand_nonnumeric_cells")
  })

  it("describes the file as demand data", {
    dat <- wide_demand()
    dat[["1"]] <- c("8", "n/a")
    res <- validate$check_data(dat, type = "demand")
    expect_match(res, "demand", fixed = TRUE)
    expect_no_match(res, "indifference", fixed = TRUE)
  })

  it("rejects values that retype_data() would turn into NA in an all-character file", {
    dat <- wide_demand()
    dat[] <- list(c("p1", "p2"), c("$10", "$9"), c("$8", "$7"), c("$2", "$1"))
    expect_identical(
      validate$reason_code(validate$check_data(dat, type = "demand")),
      "wide_demand_nonnumeric_cells"
    )
  })

  it("does not read a text group column as a price column", {
    dat <- data.frame(
      id = c("p1", "p2"), group = c("control", "drug"), `1` = c(8, 7), `5` = c(2, 1),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    expect_true(validate$check_data(dat, type = "demand"))
  })

  it("accepts text cells that are numbers or missing", {
    dat <- wide_demand()
    dat[["1"]] <- c("2.5", "")
    dat[["5"]] <- c("NA", "1e-1")
    expect_true(validate$check_data(dat, type = "demand"))
  })

  it("does not check long files", {
    dat <- data.frame(id = c("p1", "p1"), x = c(1, 5), y = c("8", "n/a"))
    expect_true(validate$check_data(dat, type = "demand"))
  })
})

describe("reshape_data - wide demand value cells", {
  it("stops with the cell message, never the vctrs one", {
    dat <- wide_demand()
    dat[["1"]] <- c("8", "n/a")
    err <- expect_error(validate$reshape_data(validate$rename_cols(dat), type = "demand"))
    expect_match(conditionMessage(err), "not numbers", fixed = TRUE)
    expect_no_match(conditionMessage(err), "Can't combine", fixed = TRUE)
  })

  it("converts a character column whose cells all parse, so it pivots beside numeric ones", {
    dat <- wide_demand()
    dat[["1"]] <- c("2.5", "")
    res <- validate$reshape_data(validate$rename_cols(dat), type = "demand")
    expect_type(res$y, "double")
    expect_identical(res$y[res$id == "p1"], c(10, 2.5, 2))
    expect_identical(res$y[res$id == "p2"], c(9, NA, 1))
  })

  it("leaves a text group column alone", {
    dat <- data.frame(
      id = c("p1", "p2"), group = c("control", "drug"), `1` = c("8", "7"), `5` = c(2, 1),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    res <- validate$reshape_data(validate$rename_cols(dat), type = "demand")
    expect_identical(res$group, c("control", "control", "drug", "drug"))
    expect_identical(res$y, c(8, 2, 7, 1))
  })
})
