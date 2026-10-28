# The eager reshape observers in app/view/discounting.R and app/view/demand.R prepare the
# stored upload as soon as it changes. An error there used to end the session: shiny closes
# the connection on an uncaught observer error, so the user saw the grey screen instead of
# a message (every one of the 14 production error rows of 2026-10-27/28 was followed by
# logout and session_end within 0.5 s). Both observers now catch, record and notify.
#
# shiny::testServer() swallows observer errors, so "no error" alone would prove nothing. The
# discriminating assertions are the state: without the catch, data_d keeps the previous
# frame and nothing is shown; with it, data_d is NULL and a notification names the problem.

box::use(
  testthat[...],
  shiny,
)

box::use(
  app / logic / async / task,
  app / logic / async / workers,
  app / view / demand,
  app / view / discounting,
)

# box gives each view module its own locked copy of the shiny exports; swap
# showNotification there and collect what the module shows.
capture_notifications <- function(module_server, env = parent.frame()) {
  shown <- new.env(parent = emptyenv())
  shown$messages <- character()
  shiny_env <- environment(module_server)$shiny
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

good_wide_ip <- function() {
  data.frame(
    id = c("p1", "p2"), `1` = c(0.9, 0.8), `5` = c(0.7, 0.6), `100` = c(0.2, 0.1),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

bad_wide_ip <- function() {
  dat <- good_wide_ip()
  dat[["5"]] <- c("0.7", "n/a") # reshape_data() stops on this cell
  dat
}

good_wide_demand <- function() {
  data.frame(
    id = c("p1", "p2"), `0.5` = c(10, 9), `1` = c(8, 7), `5` = c(2, 1),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

bad_wide_demand <- function() {
  dat <- good_wide_demand()
  colnames(dat)[2] <- "apt_1" # rename_cols() stops on this header
  dat
}

describe("discounting navpanel - a failing prepare notifies instead of ending the session", {
  it("sets data_d NULL, notifies, and recovers on the next good upload", {
    shown <- capture_notifications(discounting$navpanel_server)
    shiny$testServer(discounting$navpanel_server, {
      # As in app/main.R, the reactive store exists before any observer runs, so the
      # observer's first (empty) run registers its dependency. The data-table child
      # branches on the sidebar's analysis type, which the real UI always has set.
      session$userData$data <- shiny$reactiveValues()
      session$setInputs(calc_discounting = "Indifference Point Regression")
      session$userData$data$discounting <- good_wide_ip()
      session$flushReact()
      expect_s3_class(data_r$data_d, "data.frame")

      session$userData$data$discounting <- bad_wide_ip()
      session$flushReact()
      expect_null(data_r$data_d)

      session$userData$data$discounting <- good_wide_ip()
      session$flushReact()
      expect_s3_class(data_r$data_d, "data.frame")
    })
    expect_true(any(grepl("Could not prepare", shown$messages, fixed = TRUE)))
    expect_true(any(grepl("not numbers", shown$messages, fixed = TRUE)))
  })
})

describe("demand navpanel - a failing reshape notifies instead of ending the session", {
  it("sets data_d NULL, notifies, and recovers on the next good upload", {
    shown <- capture_notifications(demand$navpanel_server)
    fit_task <- task$make_fit_task(workers$fit_demand_fixed_worker, async = FALSE)
    shiny$testServer(demand$navpanel_server, args = list(fit_task = fit_task), {
      session$userData$data <- shiny$reactiveValues()
      session$userData$data$demand <- good_wide_demand()
      session$flushReact()
      expect_s3_class(data_r$data_d, "data.frame")

      session$userData$data$demand <- bad_wide_demand()
      session$flushReact()
      expect_null(data_r$data_d)

      session$userData$data$demand <- good_wide_demand()
      session$flushReact()
      expect_s3_class(data_r$data_d, "data.frame")
    })
    expect_true(any(grepl("Could not prepare", shown$messages, fixed = TRUE)))
    expect_true(any(grepl("price headers", shown$messages, fixed = TRUE)))
  })
})
