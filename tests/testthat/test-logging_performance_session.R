# Every performance row in production to date (888 of them) has a NULL session: the session
# logger passed only its token string down to log_performance(), which never handed the
# session object to track_performance(). The consent gate drops rows without a token, so a
# performance row must now carry the session or it is lost.

box::use(
  testthat[...],
  DBI,
  RSQLite,
)

box::use(
  app / logic / logging_utils,
  app / logic / telemetry_utils,
)

performance_rows <- function(db_path) {
  con <- DBI$dbConnect(RSQLite$SQLite(), db_path)
  on.exit(DBI$dbDisconnect(con), add = TRUE)
  DBI$dbGetQuery(con, "SELECT session, details FROM event_log WHERE type = 'performance'")
}

describe("performance telemetry carries the session", {
  it("writes a performance row with the session token from the session logger", {
    db <- with_sqlite_telemetry()
    s <- consented_session()
    logger <- logging_utils$create_session_logger(s)

    result <- logger$with_performance("unit_op", function() 42, always_log = TRUE)

    expect_identical(result, 42)
    rows <- performance_rows(db)
    expect_equal(nrow(rows), 1L)
    expect_identical(rows$session, s$token)
    expect_match(rows$details, '"operation":"unit_op"', fixed = TRUE)
  })

  it("accepts the session on log_performance() directly", {
    db <- with_sqlite_telemetry()
    s <- consented_session()
    logging_utils$log_performance("direct_op", 12, session_id = s$token, always_log = TRUE, session = s)
    rows <- performance_rows(db)
    expect_equal(nrow(rows), 1L)
    expect_identical(rows$session, s$token)
  })

  it("still records the *_FAILED timing with the session when the wrapped call errors", {
    db <- with_sqlite_telemetry()
    s <- consented_session()
    logger <- logging_utils$create_session_logger(s)
    expect_error(logger$with_performance("bad_op", function() stop("boom"), always_log = TRUE), "boom")
    rows <- performance_rows(db)
    expect_equal(nrow(rows), 1L)
    expect_identical(rows$session, s$token)
    expect_match(rows$details, '"operation":"bad_op_FAILED"', fixed = TRUE)
  })
})
