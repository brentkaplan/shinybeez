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
