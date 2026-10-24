# No telemetry row may carry a user's column names, cell values or file names. This drives
# every payload-building helper the way the app does, from an upload whose headers and values
# carry a marker string, and asserts the marker never reaches the stored details.

box::use(
  testthat[...],
  DBI,
  RSQLite,
)

box::use(
  app / logic / telemetry_utils,
  app / logic / upload_read,
  app / logic / validate,
)

# Lower case: check_data() lowercases headers before it quotes them in a message.
marker <- "qzx"

all_details <- function(db_path) {
  con <- DBI$dbConnect(RSQLite$SQLite(), db_path)
  on.exit(DBI$dbDisconnect(con), add = TRUE)
  DBI$dbGetQuery(con, "SELECT type, details FROM event_log")
}

describe("telemetry payloads built from a user's upload", {
  it("never contain the upload's header strings, cell values or file name", {
    db <- with_sqlite_telemetry()
    s <- consented_session()
    dat <- data.frame(
      id = c("valueqzx", "valueqzx"),
      priceqzx1 = c(1, 2), priceqzx2 = c(3, 4),
      check.names = FALSE
    )
    attr(dat, "delim") <- ","
    reason <- validate$check_data(dat, type = "demand")
    expect_true(is.character(reason))
    expect_match(reason, marker)

    telemetry_utils$track_validation(
      "demand", "failure", "check_data", reason, s,
      shape = upload_read$upload_shape(dat), reason_code = validate$reason_code(reason)
    )
    telemetry_utils$track_data_upload(
      list(size = 10, type = "fileqzx", rows = 2, cols = 3, name = "dataqzx.csv"), s
    )
    telemetry_utils$track_configuration(
      "mixed_effects",
      list(equation_form = "koff", factors = "priceqzx1", random_effects = "~ 1 | priceqzx1", n_rows = 2),
      s
    )
    telemetry_utils$track_model_fitting(
      "mixed_effects",
      list(equation = "koff", factors = "priceqzx1", error = 'fit failed on "priceqzx1"'),
      "failed", s
    )
    telemetry_utils$track_error(
      paste("wide_mapper output failed check_data:", reason),
      error_context = "wide_mapper", session = s,
      details = list(col = "priceqzx1", n = 2)
    )
    telemetry_utils$track_reshape("demand", "confirmed", list(layout = "wide", x_source = "priceqzx1"), s)
    telemetry_utils$track_export("csv", module = "demand", file_format = "dataqzx.csv", session = s)

    rows <- all_details(db)
    expect_gte(nrow(rows), 7L)
    leaks <- rows$type[grepl(marker, rows$details, ignore.case = TRUE)]
    expect_identical(leaks, character(), info = paste("leaking types:", paste(leaks, collapse = ", ")))
  })

  it("records the validation reason code alongside the scrubbed reason", {
    db <- with_sqlite_telemetry()
    s <- consented_session()
    dat <- data.frame(id = 1:2, a = 1:2, b = 3:4)
    reason <- validate$check_data(dat, type = "demand")
    telemetry_utils$track_validation(
      "demand", "failure", "check_data", reason, s, reason_code = validate$reason_code(reason)
    )
    rows <- all_details(db)
    details <- jsonlite::fromJSON(rows$details[rows$type == "validation_outcome"])
    expect_identical(details$reason_code, "unparsable_price_headers")
    expect_identical(details$reason, 'The column names are not numeric. Could not parse these price headers: "…", "…".')
  })
})

describe("mapper validation messages", {
  it("quote the series label so the scrub removes it", {
    box::use(app / logic / reshape / spec, app / logic / telemetry_schema)
    dat <- data.frame(responseid = c("r1", "r2"), a = c(1, 2), b = c(3, 4))
    s <- spec$new_spec(
      "demand", "responseid",
      list(spec$new_series("a", x = 1, label = "patientqzx")),
      x_source = "manual"
    )
    msg <- tryCatch(spec$apply_spec(s, dat), error = function(e) conditionMessage(e))
    expect_match(msg, '^"patientqzx": ')
    expect_false(grepl("qzx", telemetry_schema$scrub_user_strings(msg)))
  })
})
