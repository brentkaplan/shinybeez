box::use(
  testthat[...],
  jsonlite,
)

box::use(
  app / logic / telemetry_utils,
)

describe("track_reshape", {
  it("writes a reshape row carrying target, outcome and the summary", {
    db <- with_sqlite_telemetry()
    telemetry_utils$track_reshape(
      "demand", "confirmed",
      summary = list(n_series = 2L, x_source = "manual", n_cols_in = 8L, n_rows_out = 30L, n_dropped = 1L),
      session = fake_session()
    )
    rows <- read_event_rows(db, "reshape")
    expect_equal(nrow(rows), 1)
    details <- jsonlite$fromJSON(rows$details)
    expect_equal(details$target, "demand")
    expect_equal(details$outcome, "confirmed")
    expect_equal(details$n_series, 2)
    expect_equal(details$x_source, "manual")
  })
})

describe("track_data_upload reshaped flag", {
  it("records reshaped = TRUE when the upload came through the mapper", {
    db <- with_sqlite_telemetry()
    telemetry_utils$track_data_upload(
      file_info = list(size = 10, type = "csv", rows = 3, cols = 3, reshaped = TRUE),
      session = fake_session()
    )
    details <- jsonlite$fromJSON(read_event_rows(db, "data_upload")$details)
    expect_true(isTRUE(details$reshaped))
  })
  it("records reshaped = FALSE by default", {
    db <- with_sqlite_telemetry()
    telemetry_utils$track_data_upload(
      file_info = list(size = 10, type = "csv", rows = 3, cols = 3),
      session = fake_session()
    )
    details <- jsonlite$fromJSON(read_event_rows(db, "data_upload")$details)
    expect_false(isTRUE(details$reshaped))
  })
})
