# The integration files stop their shinytest2 driver through local_app_stop(), which
# must run when the enclosing describe() block ends, not when the whole test_dir()
# run ends. Parking app$stop() on teardown_env() kept every app subprocess (and,
# since the async fits, its mirai daemon) alive across the entire suite and
# exhausted the 16 GB CI runner (2026-09-01).

stop_log <- new.env()

describe("local_app_stop inside a describe block", {
  app <- NULL

  it("registers the stop without stopping the app while the block runs", {
    app <<- list(stop = function() assign("stopped", TRUE, envir = stop_log))
    expect_null(stop_log$stopped)
  })

  local_app_stop()

  it("still has the app running for later tests in the same block", {
    expect_null(stop_log$stopped)
  })
})

describe("local_app_stop after the describe block ended", {
  it("stopped the app when the block ended, before the next block ran", {
    expect_true(isTRUE(stop_log$stopped))
  })
})

describe("local_app_stop with no app", {
  it("is a no-op when app is NULL", {
    app <- NULL
    expect_no_error(local({
      local_app_stop()
    }))
  })
})
