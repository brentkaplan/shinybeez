# Regression tests for production errors 2a0fc474 / 46abab75 / 11dc772e (~30 events, the
# largest cluster in the 2026-07-01 triage).
#
#   ℹ In index: 1.
#   Caused by error in `y[-length(y)] + y[-1]`:
#   ! non-numeric argument to binary operator
#
# app/view/discounting.R coerced uploads to numeric only on the id-first/ncol>3 branch.
# A long `id,x,y` upload (ncol == 3) fell through to a bare `else` that stored the frame
# verbatim, so a character `y` — quoted numbers, a stray blank, a text cell — survived all
# the way into beezdiscounting::check_unsystematic, which does arithmetic on it.
#
# The branch logic now lives in validate$prepare_discounting_data() so it is testable, rather
# than being inlined in a Shiny observer where it was not.

box::use(
  testthat[...],
)

box::use(
  app / logic / validate,
  app / logic / discounting / systematic,
)

# --- Fixtures -----------------------------------------------------------------

# The shape that broke production: already-long, and `y` arrived as character.
long_character_y <- function() {
  data.frame(
    id = rep(c("1", "2"), each = 5),
    x = rep(c(1, 30, 180, 365, 3650), 2),
    y = c("100", "80", "60", "40", "20", "95", "75", "55", "35", "15"),
    stringsAsFactors = FALSE
  )
}

# Qualtrics 5.5-Trial data also lands in the `else` branch and must NOT be coerced.
qualtrics_five_trial <- function() {
  data.frame(
    ResponseId = c("R_1", "R_2"),
    I1 = c(1, 2), I2 = c(2, 1), I3 = c(1, 1),
    stringsAsFactors = FALSE
  )
}

describe("prepare_discounting_data", {
  it("coerces a long id/x/y upload with a character y to numeric", {
    prepared <- validate$prepare_discounting_data(long_character_y())

    expect_true(is.numeric(prepared$y))
    expect_true(is.numeric(prepared$x))
    expect_identical(prepared$y[1], 100)
  })

  it("reproduces the production failure without the coercion (2a0fc474)", {
    # Guard the RED: the raw frame — what the old `else` branch stored — still detonates
    # inside check_unsystematic with the verbatim production message.
    expect_error(
      systematic$compute_systematic_discounting(long_character_y()),
      "non-numeric argument to binary operator",
      fixed = TRUE
    )
  })

  it("lets the systematic check run once the data is prepared", {
    prepared <- validate$prepare_discounting_data(long_character_y())

    expect_no_error(systematic$compute_systematic_discounting(prepared))
  })

  # The `else` branch is a catch-all: 5.5-Trial Qualtrics uploads land there too, and have no
  # id/x/y columns at all. Coercing them would be a regression, so shape-gate the coercion.
  it("passes Qualtrics 5.5-Trial data through untouched", {
    raw <- qualtrics_five_trial()

    expect_identical(validate$prepare_discounting_data(raw), raw)
  })

  it("leaves a frame with no id/x/y columns alone", {
    raw <- data.frame(foo = 1:3, bar = letters[1:3], stringsAsFactors = FALSE)

    expect_identical(validate$prepare_discounting_data(raw), raw)
  })

  it("tolerates a y that is not fully numeric rather than silently dropping rows", {
    # A stray text cell coerces to NA (with a warning) — the systematic check must still
    # produce a result rather than aborting on a type error.
    dirty <- long_character_y()
    dirty$y[3] <- "n/a"

    prepared <- suppressWarnings(validate$prepare_discounting_data(dirty))
    expect_true(is.numeric(prepared$y))
    expect_true(is.na(prepared$y[3]))
  })
})

# --- Wide indifference-point files with a cell that is not a number -------------------------
# 2026-10-27 (9fcff37e, a920a0a0, 94eb3497, six uploads of one user): a single text cell
# made vroom read a whole delay column as character, and the wide-to-long pivot in
# reshape_data() ran BEFORE retype_data() could coerce, so vctrs aborted with
# "Can't combine `1` <double> and `5` <character>" inside the eager observer and the
# session ended. check_wide_discounting() validated headers and ids only, never cells.

wide_ip <- function() {
  data.frame(
    id = c("p1", "p2", "p3"),
    `1` = c(0.95, 0.90, 0.85), `5` = c(0.80, 0.70, 0.60),
    `10` = c(0.60, 0.55, 0.50), `100` = c(0.20, 0.15, 0.10),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

describe("prepare_discounting_data - wide file with a non-numeric cell", {
  it("reproduces the production vctrs abort on the stored shape", {
    # Guard the RED: a character delay column beside doubles cannot be pivoted.
    dat <- wide_ip()
    dat[["5"]] <- c("0.80", "n/a", "0.60")
    expect_error(
      tidyr::pivot_longer(dat, cols = 2:ncol(dat), names_to = "x", values_to = "y"),
      "Can't combine", fixed = TRUE
    )
  })

  it("is rejected at validation, naming the column and the cell", {
    dat <- wide_ip()
    dat[["5"]] <- c("0.80", "n/a", "0.60")
    res <- validate$check_data(dat, type = "discounting")
    expect_type(res, "character")
    expect_match(res, 'Column "5" has values that are not numbers: "n/a"', fixed = TRUE)
    expect_identical(validate$reason_code(res), "wide_ip_nonnumeric_cells")
  })

  it("stops prepare_discounting_data() with that message, never the vctrs one", {
    dat <- wide_ip()
    dat[["5"]] <- c("0.80", "n/a", "0.60")
    err <- expect_error(validate$prepare_discounting_data(dat))
    expect_match(conditionMessage(err), "not numbers", fixed = TRUE)
    expect_no_match(conditionMessage(err), "Can't combine", fixed = TRUE)
  })

  it("names every offending column with at most three example cells each", {
    dat <- wide_ip()
    dat[["5"]] <- c("a", "b", "c")
    dat[["100"]] <- c("0.2", "d", "e")
    dat <- rbind(dat, data.frame(
      id = "p4", `1` = 0.5, `5` = "z", `10` = 0.3, `100` = "f",
      check.names = FALSE, stringsAsFactors = FALSE
    ))
    res <- validate$check_data(dat, type = "discounting")
    expect_match(res, 'Column "5" has values that are not numbers: "a", "b", "c" (and 1 more).', fixed = TRUE)
    expect_match(res, 'Column "100" has values that are not numbers: "d", "e", "f".', fixed = TRUE)
  })

  it("accepts cells that parse as numbers and converts them before the pivot", {
    dat <- wide_ip()
    dat[["5"]] <- c(" 0.80 ", "1e-1", "-0.5") # character, but every cell is a number
    dat[["10"]] <- c(NA, NA, NA) # an empty column reads as logical
    dat[["100"]] <- factor(c("0.2", "0.15", "0.1")) # labels, never the factor codes
    expect_true(validate$check_data(dat, type = "discounting"))
    prepared <- validate$prepare_discounting_data(dat)
    expect_true(is.numeric(prepared$y))
    expect_equal(prepared$y[prepared$x == 5], c(0.8, 0.1, -0.5))
    expect_true(all(is.na(prepared$y[prepared$x == 10])))
    expect_equal(prepared$y[prepared$x == 100], c(0.2, 0.15, 0.1))
  })

  it("treats the literal string NA and blank cells as missing", {
    dat <- wide_ip()
    dat[["5"]] <- c("0.80", "NA", "")
    expect_true(validate$check_data(dat, type = "discounting"))
    prepared <- validate$prepare_discounting_data(dat)
    expect_equal(prepared$y[prepared$x == 5], c(0.8, NA, NA))
  })

  it("rejects TRUE/FALSE and date cells instead of silently coercing them", {
    dat <- wide_ip()
    dat[["5"]] <- c(TRUE, FALSE, TRUE)
    expect_identical(
      validate$reason_code(validate$check_data(dat, type = "discounting")),
      "wide_ip_nonnumeric_cells"
    )
    dat <- wide_ip()
    dat[["5"]] <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))
    expect_identical(
      validate$reason_code(validate$check_data(dat, type = "discounting")),
      "wide_ip_nonnumeric_cells"
    )
  })
})
