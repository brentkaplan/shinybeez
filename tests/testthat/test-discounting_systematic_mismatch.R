# 2026-10-29 (signature 29be9b01): the systematic table branched on the "What are you
# scoring?" dropdown alone. Its default is Indifference Point Regression, and
# prepare_discounting_data() keeps 5.5-Trial and MCQ uploads in their own shapes, so opening
# the tab with either file aborted group_split(id) and showed the raw dplyr message.
# A dropdown and file that disagree are a user's mismatch, not an app failure: the tab says so
# in place, with no error notification.

box::use(
  testthat[...],
  shiny,
)

box::use(
  app / logic / validate,
  app / view / discounting_data_table,
)

# box gives the view module its own locked copy of the shiny exports; swap
# showNotification there and collect what the module shows.
capture_notifications <- function(env = parent.frame()) {
  shown <- new.env(parent = emptyenv())
  shown$messages <- character()
  shiny_env <- environment(discounting_data_table$server)$shiny
  fn <- "showNotification"
  original <- shiny_env[[fn]]
  unlockBinding(fn, shiny_env)
  assign(fn, function(ui, ...) {
    shown$messages <- c(shown$messages, as.character(ui))
    invisible(NULL)
  }, envir = shiny_env)
  withr::defer({
    assign(fn, original, envir = shiny_env)
    lockBinding(fn, shiny_env)
  }, envir = env)
  shown
}

five_trial_upload <- function() {
  data.frame(responseid = c("r1", "r2"), i1 = c(1, 2), i2 = c(2, 1))
}

mcq_long_upload <- function() {
  data.frame(subjectid = c(1, 1), questionid = c(1, 2), response = c(0, 1))
}

ip_long_upload <- function() {
  data.frame(
    id = rep(c("1", "2"), each = 5),
    x = rep(c(1, 7, 30, 90, 365), 2),
    y = c(95, 85, 60, 30, 10, 90, 80, 50, 25, 5)
  )
}

# Run `check` against the module with `upload` stored, prepared as discounting.R prepares it,
# and the dropdown on its default.
with_systematic_table <- function(upload, check) {
  data_r <- shiny$reactiveValues(data_d = validate$prepare_discounting_data(upload))
  shiny$testServer(
    discounting_data_table$server,
    args = list(data_r = data_r, type = shiny$reactive("Indifference Point Regression")),
    {
      session$userData$data <- shiny$reactiveValues(discounting = upload)
      session$setInputs(c1 = 0.2, c2 = 0.1)
      session$flushReact()
      check(output)
    }
  )
}

describe("systematic table under Indifference Point Regression", {
  it("explains the mismatch for a 5.5-Trial file, without an error notification", {
    shown <- capture_notifications()
    with_systematic_table(five_trial_upload(), function(output) {
      expect_error(output$systematic_table, "What are you scoring?", fixed = TRUE)
    })
    expect_length(shown$messages, 0)
  })

  it("explains the mismatch for an MCQ file, without an error notification", {
    shown <- capture_notifications()
    with_systematic_table(mcq_long_upload(), function(output) {
      expect_error(output$systematic_table, "What are you scoring?", fixed = TRUE)
    })
    expect_length(shown$messages, 0)
  })

  it("still renders for indifference-point data", {
    shown <- capture_notifications()
    with_systematic_table(ip_long_upload(), function(output) {
      expect_no_error(output$systematic_table)
    })
    expect_length(shown$messages, 0)
  })
})
