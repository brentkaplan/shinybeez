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

describe("track_validation shape", {
  it("records the failure shape as counts and flags, never column names", {
    db <- with_sqlite_telemetry()
    box::use(app / logic / upload_read)
    dat <- data.frame(`participant code` = 1:2, `secret header` = c("a", "b"), `5` = c(1, 2), check.names = FALSE)
    attr(dat, "delim") <- ";"
    telemetry_utils$track_validation(
      "demand", "failure", "check_data", "The first column is not `id`",
      fake_session(), shape = upload_read$upload_shape(dat)
    )
    rows <- read_event_rows(db, "validation_outcome")
    expect_equal(nrow(rows), 1)
    details <- jsonlite$fromJSON(rows$details)
    expect_equal(details$n_cols, 3)
    expect_equal(details$n_rows, 2)
    expect_equal(details$delim, "semicolon")
    expect_equal(details$n_numeric_cols, 2)
    expect_false(details$first_col_id_like)
    expect_false(details$has_qualtrics_meta)
    expect_false(grepl("secret|participant code", rows$details))
  })

  it("flags an id-like column and Qualtrics metadata", {
    box::use(app / logic / upload_read)
    dat <- data.frame(startdate = "2026-10-01", progress = 100, responseid = "R_1", q1 = 1)
    shape <- upload_read$upload_shape(dat)
    expect_true(shape$has_qualtrics_meta)
    expect_true(shape$any_id_like)
    expect_false(shape$first_col_id_like)
    expect_equal(shape$delim, "unknown")
  })
})
