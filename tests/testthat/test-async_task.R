box::use(
  testthat[...],
  shiny,
  later,
  mirai,
)
box::use(
  app / logic / async / daemons,
  app / logic / async / task,
)

settle <- function(t, session, timeout_s = 30) {
  deadline <- Sys.time() + timeout_s
  while (t$status() %in% c("initial", "running") && Sys.time() < deadline) {
    later$run_now(0.05)
    session$flushReact()
  }
  t$status()
}

describe("classify_outcome", {
  it("passes through non-error states", {
    expect_identical(task$classify_outcome("running", NULL), "running")
    expect_identical(task$classify_outcome("success", NULL), "success")
  })
  it("maps mirai error codes 20 and 5 to cancelled and timeout", {
    cancelled <- list(data = structure(20L, class = c("errorValue", "try-error")))
    timed_out <- list(data = structure(5L, class = c("errorValue", "try-error")))
    expect_identical(task$classify_outcome("error", cancelled), "cancelled")
    expect_identical(task$classify_outcome("error", timed_out), "timeout")
  })
  it("treats any other error as error", {
    expect_identical(task$classify_outcome("error", NULL), "error")
    expect_identical(task$classify_outcome("error", list(data = "boom")), "error")
  })
})

describe("make_fit_task (synchronous fallback)", {
  it("runs the worker inline and reports success", {
    shiny$testServer(function(input, output, session) {
      t <- task$make_fit_task(function(spec, data) list(fit = spec$x * 2), async = FALSE)
      expect_null(t$started_at())
      t$invoke(list(x = 21), NULL)
      expect_identical(settle(t, session), "success")
      expect_identical(t$result()$fit, 42)
      expect_s3_class(t$started_at(), "POSIXct")
    }, expr = NULL)
  })
})

describe("make_fit_task (daemon)", {
  it("runs the worker in a daemon and reports success", {
    daemons$start_daemons(1L)
    on.exit(daemons$stop_daemons(), add = TRUE)
    shiny$testServer(function(input, output, session) {
      t <- task$make_fit_task(function(spec, data) list(fit = spec$x * 2), async = TRUE)
      t$invoke(list(x = 21), NULL)
      expect_identical(t$status(), "running")
      expect_identical(settle(t, session), "success")
      expect_identical(t$result()$fit, 42)
      expect_identical(t$outcome(), "success")
    }, expr = NULL)
  })
  it("cancel() yields outcome 'cancelled' and leaves the daemon usable", {
    daemons$start_daemons(1L)
    on.exit(daemons$stop_daemons(), add = TRUE)
    shiny$testServer(function(input, output, session) {
      t <- task$make_fit_task(function(spec, data) {
        Sys.sleep(30)
        1
      }, async = TRUE)
      t$invoke(list(), NULL)
      Sys.sleep(0.5)
      t$cancel()
      expect_identical(settle(t, session), "error")
      expect_identical(t$outcome(), "cancelled")
    }, expr = NULL)
    expect_identical(mirai$mirai(1 + 1)[], 2)
  })
  it("refuses a second invoke while running instead of queueing it", {
    daemons$start_daemons(1L)
    on.exit(daemons$stop_daemons(), add = TRUE)
    shiny$testServer(function(input, output, session) {
      t <- task$make_fit_task(function(spec, data) {
        Sys.sleep(2)
        spec$x
      }, async = TRUE)
      expect_true(t$invoke(list(x = 1), NULL))
      expect_false(t$invoke(list(x = 2), NULL))
      expect_identical(settle(t, session), "success")
      expect_identical(t$result(), 1)
    }, expr = NULL)
  })
  it("timeout yields outcome 'timeout'", {
    daemons$start_daemons(1L)
    on.exit(daemons$stop_daemons(), add = TRUE)
    shiny$testServer(function(input, output, session) {
      t <- task$make_fit_task(function(spec, data) {
        Sys.sleep(10)
        1
      }, timeout_ms = 500, async = TRUE)
      t$invoke(list(), NULL)
      expect_identical(settle(t, session), "error")
      expect_identical(t$outcome(), "timeout")
    }, expr = NULL)
  })
  it("a worker error yields outcome 'error' with its message", {
    daemons$start_daemons(1L)
    on.exit(daemons$stop_daemons(), add = TRUE)
    shiny$testServer(function(input, output, session) {
      t <- task$make_fit_task(function(spec, data) stop("boom"), async = TRUE)
      t$invoke(list(), NULL)
      expect_identical(settle(t, session), "error")
      expect_identical(t$outcome(), "error")
      expect_match(t$error_message(), "boom")
    }, expr = NULL)
  })
})
