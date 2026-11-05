# Every telemetry row, whichever helper built it and including the rows shiny.telemetry
# writes on its own, passes one gate inside the storage object's write(): the session's
# consent level decides. These tests drive real rows into a throwaway SQLite file and read
# them back.

box::use(
  testthat[...],
  DBI,
  RSQLite,
  jsonlite,
)

box::use(
  app / logic / consent_registry,
  app / logic / startup_info,
  app / logic / telemetry_utils,
)

all_rows <- function(db_path) {
  con <- DBI$dbConnect(RSQLite$SQLite(), db_path)
  on.exit(DBI$dbDisconnect(con), add = TRUE)
  DBI$dbGetQuery(con, "SELECT rowid, session, type, details FROM event_log ORDER BY rowid")
}

details_of <- function(rows, type) {
  jsonlite$fromJSON(rows$details[rows$type == type][[1]])
}

dropped <- function() telemetry_utils$get_telemetry()$data_storage$dropped_writes()

describe("the storage gate", {
  it("buffers rows written before the level arrives and lands them when it does", {
    consent_registry$reset()
    db <- with_sqlite_telemetry()
    s <- fake_session()
    telemetry_utils$track_event("navigation", list(tab = "Demand"), s)
    expect_equal(nrow(all_rows(db)), 0)

    telemetry_utils$set_session_consent(s, "usage", "default")

    rows <- all_rows(db)
    expect_identical(rows$type[rows$session == s$token], c("session_start", "navigation", "login"))
    start <- details_of(rows, "session_start")
    expect_identical(start$consent, "usage")
    expect_identical(start$consent_source, "default")
    expect_identical(start$platform, "web")
    expect_identical(start$app_version, startup_info$app_version())
  })

  it("drops usage rows at diagnostic and counts them", {
    consent_registry$reset()
    db <- with_sqlite_telemetry()
    s <- fake_session()
    telemetry_utils$set_session_consent(s, "diagnostic", "default_eu")
    telemetry_utils$track_navigation("Demand", s)
    telemetry_utils$track_error("boom", session = s)
    rows <- all_rows(db)
    expect_identical(rows$type, c("session_start", "error"))
    expect_identical(dropped()[["consent"]], 1L)
    expect_identical(details_of(rows, "session_start")$consent_source, "default_eu")
  })

  it("writes nothing but the consent_change row after a move to none", {
    consent_registry$reset()
    db <- with_sqlite_telemetry()
    s <- fake_session()
    telemetry_utils$set_session_consent(s, "usage", "stored")
    telemetry_utils$set_session_consent(s, "none", "banner")
    telemetry_utils$track_error("boom", session = s)
    telemetry_utils$track_navigation("Demand", s)
    rows <- all_rows(db)
    expect_identical(sum(rows$type == "consent_change"), 1L)
    expect_identical(sum(rows$type == "error"), 0L)
    expect_identical(sum(rows$type == "navigation"), 0L)
    change <- details_of(rows, "consent_change")
    expect_identical(change$from, "usage")
    expect_identical(change$to, "none")
    expect_identical(change$source, "banner")
  })

  it("lands nothing for a session whose level never arrives", {
    consent_registry$reset()
    db <- with_sqlite_telemetry()
    s <- fake_session()
    for (i in 1:3) telemetry_utils$track_navigation("Demand", s)
    expect_identical(consent_registry$expire(now = Sys.time() + 11), 3L)
    telemetry_utils$track_navigation("Demand", s)
    expect_equal(nrow(all_rows(db)), 0)
  })

  it("counts rows that overflow the pending buffer as expired", {
    consent_registry$reset()
    db <- with_sqlite_telemetry()
    s <- fake_session()
    for (i in seq_len(consent_registry$PENDING_CAP + 1L)) {
      telemetry_utils$track_navigation("Demand", s)
    }
    expect_equal(nrow(all_rows(db)), 0)
    expect_identical(dropped()[["expired"]], 1L)
  })

  it("drops a row with no session token and counts it", {
    consent_registry$reset()
    db <- with_sqlite_telemetry()
    telemetry_utils$track_event("error", list(error_message = "x"), session = NULL)
    expect_equal(nrow(all_rows(db)), 0)
    expect_identical(dropped()[["no_token"]], 1L)
  })

  it("starts shiny.telemetry's observers once and only at usage", {
    consent_registry$reset()
    db <- with_sqlite_telemetry()
    s <- fake_session()
    telemetry_utils$set_session_consent(s, "diagnostic", "default_eu")
    expect_identical(sum(all_rows(db)$type == "login"), 0L)
    telemetry_utils$set_session_consent(s, "usage", "settings")
    expect_identical(sum(all_rows(db)$type == "login"), 1L)
    telemetry_utils$set_session_consent(s, "diagnostic", "settings")
    telemetry_utils$set_session_consent(s, "usage", "settings")
    expect_identical(sum(all_rows(db)$type == "login"), 1L)
  })

  it("writes no consent_change for a repeated handshake with the same level", {
    consent_registry$reset()
    db <- with_sqlite_telemetry()
    s <- fake_session()
    telemetry_utils$set_session_consent(s, "usage", "stored")
    telemetry_utils$set_session_consent(s, "usage", "stored")
    rows <- all_rows(db)
    expect_identical(sum(rows$type == "consent_change"), 0L)
    expect_identical(sum(rows$type == "session_start"), 1L)
  })

  it("marks a session_start that follows an initial none as changed", {
    consent_registry$reset()
    db <- with_sqlite_telemetry()
    s <- fake_session()
    telemetry_utils$set_session_consent(s, "none", "stored")
    expect_equal(nrow(all_rows(db)), 0)
    telemetry_utils$set_session_consent(s, "diagnostic", "settings")
    rows <- all_rows(db)
    expect_identical(rows$type, c("session_start", "consent_change"))
    expect_identical(details_of(rows, "session_start")$consent_source, "changed")
  })

  it("drops rows after end_session_consent as closed", {
    consent_registry$reset()
    db <- with_sqlite_telemetry()
    s <- fake_session()
    telemetry_utils$set_session_consent(s, "usage", "stored")
    telemetry_utils$end_session_consent(s)
    telemetry_utils$track_error("late", session = s)
    expect_identical(sum(all_rows(db)$type == "error"), 0L)
    expect_identical(dropped()[["closed"]], 1L)
  })

  it("reports the current level, none when unknown", {
    consent_registry$reset()
    with_sqlite_telemetry()
    s <- fake_session()
    expect_identical(telemetry_utils$current_consent(s), "none")
    telemetry_utils$set_session_consent(s, "diagnostic", "stored")
    expect_identical(telemetry_utils$current_consent(s), "diagnostic")
  })

  it("pins track_values = FALSE and the cookie-free flags for start_session", {
    s <- fake_session()
    args <- telemetry_utils$usage_start_args(s)
    expect_mapequal(
      args[c("track_inputs", "track_values", "login", "logout", "browser_version",
             "track_errors", "track_anonymous_user")],
      list(
        track_inputs = TRUE, track_values = FALSE, login = TRUE, logout = TRUE,
        browser_version = FALSE, track_errors = FALSE, track_anonymous_user = FALSE
      )
    )
    expect_identical(args$username, s$token)
  })

  it("scrubs quoted user strings in error_message before writing", {
    consent_registry$reset()
    db <- with_sqlite_telemetry()
    s <- consented_session()
    telemetry_utils$track_error('bad column "Age"', session = s)
    expect_identical(details_of(all_rows(db), "error")$error_message, 'bad column "…"')
  })

  it("exposes set_consent and consent on the session tracker", {
    consent_registry$reset()
    with_sqlite_telemetry()
    s <- fake_session()
    st <- telemetry_utils$create_session_telemetry(s)
    expect_identical(st$consent(), "none")
    st$set_consent("diagnostic", "default_eu")
    expect_identical(st$consent(), "diagnostic")
  })
})

describe("start_session at usage sets no identifying cookie", {
  it("writes the login row under the session token, not an anonymous cookie user", {
    consent_registry$reset()
    db <- with_sqlite_telemetry()
    s <- fake_session()
    telemetry_utils$set_session_consent(s, "usage", "stored")
    rows <- all_rows(db)
    login <- details_of(rows, "login")
    expect_identical(unlist(login$username), s$token)
    expect_false(any(grepl("anon_user", rows$details, fixed = TRUE)))
  })
})
