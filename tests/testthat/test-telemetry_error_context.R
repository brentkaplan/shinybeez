# Error telemetry must be triageable: every `error` row carries the session token
# (so it joins the session timeline) and, when the caller has one, the fit spec that
# was running. Motivated by the 2026-08-31 production `subscript out of bounds`
# investigation, where error rows had a NULL session and no spec, so the trigger
# could only be inferred from neighbouring rows.

box::use(
  testthat[...],
  DBI,
  RSQLite,
  jsonlite,
  withr,
)

box::use(
  app / logic / logging_utils,
  app / logic / telemetry_utils,
)

# Enable telemetry against a throwaway SQLite DB for the duration of the caller's
# test, and return the DB path so rows can be read back. The env vars stay set for
# the whole test, not just init: is_telemetry_enabled() re-reads config on every
# event, and helper-integration.R sets TELEMETRY_ENABLED=FALSE process-wide, so an
# init-only override would let the writes silently no-op in a full-suite run.
with_sqlite_telemetry <- function(env = parent.frame()) {
  db_path <- withr::local_tempfile(fileext = ".sqlite", .local_envir = env)
  # Registered before local_envvar so it runs after the env is restored: reset
  # the cached telemetry object so it does not leak into later tests.
  withr::defer(
    withr::with_envvar(
      c(R_CONFIG_ACTIVE = "default", TELEMETRY_ENABLED = "FALSE"),
      telemetry_utils$init_telemetry()
    ),
    envir = env
  )
  withr::local_envvar(
    c(
      R_CONFIG_ACTIVE = "default",
      TELEMETRY_ENABLED = "TRUE",
      TELEMETRY_STORAGE = "sqlite",
      TELEMETRY_DB_PATH = db_path,
      SHINYBEEZ_ENV = "develop"
    ),
    .local_envir = env
  )
  telemetry_utils$init_telemetry()
  db_path
}

read_error_rows <- function(db_path) {
  con <- DBI$dbConnect(RSQLite$SQLite(), db_path)
  on.exit(DBI$dbDisconnect(con), add = TRUE)
  DBI$dbGetQuery(con, "SELECT session, type, details FROM event_log WHERE type = 'error'")
}

# shiny.telemetry accepts an R6 ShinySession (or a session_proxy) and reads `$token`
# from it. MockShinySession has a token but not the ShinySession class, so add it.
# (A classed list does not work: shiny's `$.session_proxy` method intercepts `$`.)
fake_session <- function() {
  s <- shiny::MockShinySession$new()
  class(s) <- c("ShinySession", class(s))
  s
}

describe("error telemetry context", {
  describe("telemetry_utils$track_error", {
    it("accepts an optional details list after session", {
      fmls <- names(formals(telemetry_utils$track_error))
      expect_equal(fmls, c("error_message", "error_context", "session", "details"))
    })

    it("writes the session token and nests details under the top-level keys", {
      db_path <- with_sqlite_telemetry()

      sess <- fake_session()
      telemetry_utils$track_error(
        "Error in FitCurves: subscript out of bounds",
        "demand_curve_fitting",
        session = sess,
        details = list(equation = "koff", constrainq0 = 1, aggregation = NULL, n_rows = 256L)
      )

      rows <- read_error_rows(db_path)
      expect_equal(nrow(rows), 1L)
      expect_equal(rows$session, sess$token)
      payload <- jsonlite$fromJSON(rows$details)
      expect_equal(payload$error_message, "Error in FitCurves: subscript out of bounds")
      expect_equal(payload$context, "demand_curve_fitting")
      expect_equal(payload$details$equation, "koff")
      expect_equal(payload$details$constrainq0, 1)
      expect_null(payload$details$aggregation)
      expect_equal(payload$details$n_rows, 256L)
    })

    it("omits the details key when none are given", {
      db_path <- with_sqlite_telemetry()
      telemetry_utils$track_error("boom", "ctx", session = fake_session())
      payload <- jsonlite$fromJSON(read_error_rows(db_path)$details)
      expect_false("details" %in% names(payload))
    })
  })

  describe("telemetry_utils$create_session_telemetry", {
    it("exposes details on its track_error too", {
      st <- telemetry_utils$create_session_telemetry(fake_session())
      expect_equal(names(formals(st$track_error)), c("error_message", "error_context", "details"))
    })

    it("forwards its bound session and the details to the error row", {
      db_path <- with_sqlite_telemetry()
      sess <- fake_session()
      st <- telemetry_utils$create_session_telemetry(sess)

      st$track_error("client boom", "client_js:app.js:4", details = list(line = 4L))

      rows <- read_error_rows(db_path)
      expect_equal(nrow(rows), 1L)
      expect_equal(rows$session, sess$token)
      expect_equal(jsonlite$fromJSON(rows$details)$details$line, 4L)
    })
  })

  describe("logging_utils$create_session_logger()$error_enhanced", {
    it("forwards the session and details to the telemetry error row", {
      db_path <- with_sqlite_telemetry()
      sess <- fake_session()
      logger <- logging_utils$create_session_logger(sess)

      logger$error_enhanced(
        "Error in FitCurves: subscript out of bounds",
        simpleError("subscript out of bounds"),
        context = "demand_curve_fitting",
        user_action = "demand model calculation",
        details = list(equation = "koff", constrainq0 = 1)
      )

      rows <- read_error_rows(db_path)
      expect_equal(nrow(rows), 1L)
      expect_equal(rows$session, sess$token)
      payload <- jsonlite$fromJSON(rows$details)
      expect_equal(payload$context, "demand_curve_fitting")
      expect_equal(payload$details$equation, "koff")
    })

    it("still records the session token when no details are given", {
      db_path <- with_sqlite_telemetry()
      sess <- fake_session()
      logger <- logging_utils$create_session_logger(sess)
      logger$error_enhanced("plain failure", context = "demand_descriptives")
      rows <- read_error_rows(db_path)
      expect_equal(rows$session, sess$token)
    })
  })
})
