box::use(
  testthat[...],
)

box::use(
  app / logic / demand / fitting,
  app / logic / logging_utils,
)

# rhino::log formats its first argument as a glue template. A message built from
# user text (an uploaded group label, a condition message) is data, not a template:
# braces in it must be written as typed, never evaluated and never a reason to throw.

# Route the main log to a file for the duration of `expr` and return its lines.
logged <- function(expr) {
  log_file <- withr::local_tempfile()
  logger::log_appender(logger::appender_file(log_file))
  withr::defer(logger::log_appender(logger::appender_console))
  force(expr)
  readLines(log_file)
}

describe("log_structured with braces in the message", {
  it("writes an unknown {name} literally instead of failing to look it up", {
    lines <- logged(logging_utils$log_structured("error", "fit failed for {zz}"))
    expect_true(any(grepl("fit failed for {zz}", lines, fixed = TRUE)))
  })

  it("writes an unbalanced brace literally", {
    lines <- logged(logging_utils$log_structured("warn", "open { brace"))
    expect_true(any(grepl("open { brace", lines, fixed = TRUE)))
  })

  it("does not evaluate code inside braces", {
    withr::local_envvar(BRACE_SAFETY_RAN = "no")

    lines <- logged(
      logging_utils$log_structured(
        "error", "group {Sys.setenv(BRACE_SAFETY_RAN = 'yes')} failed"
      )
    )

    expect_identical(Sys.getenv("BRACE_SAFETY_RAN"), "no")
    expect_true(any(grepl("{Sys.setenv(", lines, fixed = TRUE)))
  })
})

describe("fit_demand_grouped with braces in a group label", {
  it("reports a failing group named with braces instead of erroring in the log call", {
    prices <- c(0.01, 0.1, 1, 10, 100)
    dat <- data.frame(
      id = rep(1:3, each = length(prices)),
      group = rep(c("A", "A", "{zz}"), each = length(prices)),
      x = rep(prices, 3),
      # an all-missing group has nothing to aggregate, so its fit errors
      y = c(10, 8, 5, 2, 0, 12, 9, 6, 3, 1, rep(NA_real_, 5))
    )

    lines <- logged(
      result <- suppressWarnings(
        fitting$fit_demand_grouped(data = dat, eq = "koff", agg = "Mean", k = 2)
      )
    )

    expect_identical(result$failed_groups, "{zz}")
    expect_true(any(grepl("{zz}", lines, fixed = TRUE)))
  })
})
