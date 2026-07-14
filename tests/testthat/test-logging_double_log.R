box::use(
  withr,
  app/logic/logging_utils,
)

# with_performance_logging() used to log the failure itself AND re-throw to a
# caller tryCatch that logged the raw message again. Every failure therefore wrote
# TWO type='error' rows to the telemetry SQLite - with different messages
# ("Error in {op}: {msg}" from the wrapper, "<Context> failed: {msg}" from the
# caller) - so one defect surfaced as two or three distinct signatures. Three of
# the four signatures in the second 2026-07-01 triage report were one defect,
# double-logged. It inflates the new-error count and poisons triage.
#
# The caller owns the error record: 12 of the 13 with_performance() call sites
# already log in their own error handler, with more context than the wrapper has.
# The wrapper now records only the *_FAILED performance event (which is
# category='performance', NOT a type='error' row) and re-throws untouched.

# testthat::local_mocked_bindings() needs a package context and Rhino apps are not
# packages, so spy by rebinding inside the box module's own environment - which is
# where with_performance_logging() resolves the call.
local_spy_on_error_log <- function(counter, env = parent.frame()) {
  ns <- environment(logging_utils$with_performance_logging)
  original <- get("log_error_enhanced", envir = ns, inherits = FALSE)

  if (bindingIsLocked("log_error_enhanced", ns)) {
    unlockBinding("log_error_enhanced", ns)
  }
  assign(
    "log_error_enhanced",
    function(...) {
      counter$n <- counter$n + 1L
      invisible(NULL)
    },
    envir = ns
  )

  withr::defer(assign("log_error_enhanced", original, envir = ns), envir = env)
  invisible(NULL)
}

describe("with_performance_logging error ownership", {
  it("does not write an error record of its own", {
    counter <- new.env(parent = emptyenv())
    counter$n <- 0L
    local_spy_on_error_log(counter)

    expect_error(
      logging_utils$with_performance_logging(
        "unit_test_op",
        function() stop("boom")
      ),
      "boom"
    )

    # the wrapper must not create the telemetry error row - its caller does
    expect_equal(counter$n, 0L)
  })

  it("the spy would catch a wrapper-side error record if one were written", {
    # guards the test itself: prove the spy is wired to the right binding
    counter <- new.env(parent = emptyenv())
    counter$n <- 0L
    local_spy_on_error_log(counter)

    ns <- environment(logging_utils$with_performance_logging)
    get("log_error_enhanced", envir = ns, inherits = FALSE)(error_message = "x")

    expect_equal(counter$n, 1L)
  })

  it("still re-throws the original condition untouched", {
    err <- tryCatch(
      logging_utils$with_performance_logging(
        "unit_test_op",
        function() stop("original message")
      ),
      error = function(e) e
    )

    expect_s3_class(err, "error")
    expect_match(conditionMessage(err), "original message", fixed = TRUE)
    # it must NOT be rewrapped as "Error in unit_test_op: ..."
    expect_false(
      grepl("Error in unit_test_op", conditionMessage(err), fixed = TRUE)
    )
  })

  it("still returns the value on success", {
    expect_equal(
      logging_utils$with_performance_logging("unit_test_op", function() 42),
      42
    )
  })
})
