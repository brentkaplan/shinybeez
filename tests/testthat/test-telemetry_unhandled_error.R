# shiny.telemetry's own error handler (`track_errors = TRUE`) wrote `error` rows shaped
# {output_id, message} with the raw, unscrubbed condition message, and the analytics consumer
# never read them (it reads `error_message`). The app now owns unhandled errors: a
# shiny::onUnhandledError() hook calls track_error() so the row is scrubbed and visible.

box::use(
  testthat[...],
  DBI,
  RSQLite,
  jsonlite,
)

box::use(
  app / logic / telemetry_utils,
  app / main,
)

error_rows <- function(db_path) {
  con <- DBI$dbConnect(RSQLite$SQLite(), db_path)
  on.exit(DBI$dbDisconnect(con), add = TRUE)
  DBI$dbGetQuery(con, "SELECT session, details FROM event_log WHERE type = 'error'")
}

describe("unhandled_error_handler()", {
  it("records the error through track_error with the unhandled context, scrubbed", {
    db <- with_sqlite_telemetry()
    s <- consented_session()
    handler <- main$unhandled_error_handler(telemetry_utils$create_session_telemetry(s))
    expect_true(is.function(handler))

    handler(simpleError('oops in column "Age"'))

    rows <- error_rows(db)
    expect_equal(nrow(rows), 1L)
    expect_identical(rows$session, s$token)
    details <- jsonlite$fromJSON(rows$details)
    expect_identical(details$context, "unhandled")
    expect_identical(details$error_message, 'oops in column "…"')
  })

  it("never throws, even for a condition without a message", {
    with_sqlite_telemetry()
    handler <- main$unhandled_error_handler(telemetry_utils$create_session_telemetry(consented_session()))
    expect_no_error(handler(structure(list(), class = c("custom_condition", "condition"))))
  })
})
