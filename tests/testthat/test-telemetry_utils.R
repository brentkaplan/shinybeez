box::use(
  testthat[...],
)

box::use(
  app / logic / telemetry_utils,
)

describe("telemetry_utils", {
  describe("track_validation", {
    it("is a function with correct formals", {
      expect_true(is.function(telemetry_utils$track_validation))
      fmls <- names(formals(telemetry_utils$track_validation))
      expect_equal(fmls, c("module", "outcome", "check_name", "reason", "session"))
    })

    it("returns invisibly when telemetry is disabled", {
      result <- telemetry_utils$track_validation(
        "demand", "failure", "check_data", "bad columns"
      )
      expect_null(result)
    })
  })

  describe("track_configuration", {
    it("is a function with correct formals", {
      expect_true(is.function(telemetry_utils$track_configuration))
      fmls <- names(formals(telemetry_utils$track_configuration))
      expect_equal(fmls, c("module", "config", "session"))
    })

    it("returns invisibly when telemetry is disabled", {
      result <- telemetry_utils$track_configuration(
        "demand", config = list(equation = "hs", k = "2")
      )
      expect_null(result)
    })
  })

  describe("create_session_telemetry", {
    it("includes track_validation and track_configuration", {
      mock_session <- list(token = "test-session-123")
      st <- telemetry_utils$create_session_telemetry(mock_session)

      expect_true(is.function(st$track_validation))
      expect_true(is.function(st$track_configuration))
      expect_true(is.function(st$track_event))
      expect_true(is.function(st$track_navigation))
      expect_true(is.function(st$track_model_fitting))
      expect_true(is.function(st$track_data_upload))
      expect_true(is.function(st$track_export))
      expect_true(is.function(st$track_error))
    })
  })

  describe("track_model_fitting", {
    it("accepts all expected parameters", {
      fmls <- names(formals(telemetry_utils$track_model_fitting))
      expect_equal(fmls, c("model_type", "parameters", "status", "session"))
    })
  })

  describe("track_data_upload", {
    it("accepts file_info and session", {
      fmls <- names(formals(telemetry_utils$track_data_upload))
      expect_equal(fmls, c("file_info", "session"))
    })
  })

  describe("track_export", {
    it("accepts all expected parameters", {
      fmls <- names(formals(telemetry_utils$track_export))
      expect_equal(fmls, c("export_type", "module", "file_format", "row_count", "session"))
    })
  })

  describe("init_telemetry app_name (env tagging)", {
    # Enable telemetry against a throwaway SQLite DB, set SHINYBEEZ_ENV, and read back the
    # app_name the constructed Telemetry object carries (it is written onto every event_log row).
    init_with_env <- function(shinybeez_env) {
      # Reset the cached telemetry object after the test so it does not leak into siblings
      # (the disabled path in init_telemetry() nulls the cache).
      withr::defer(
        withr::with_envvar(
          # R_CONFIG_ACTIVE="default" so the disabled path fires regardless of ambient profile
          # (the production/development profiles hard-code telemetry enabled, which would re-init the cache).
          c(R_CONFIG_ACTIVE = "default", TELEMETRY_ENABLED = "FALSE"),
          telemetry_utils$init_telemetry()
        ),
        envir = parent.frame()
      )
      withr::with_envvar(
        c(
          R_CONFIG_ACTIVE = "default",
          TELEMETRY_ENABLED = "TRUE",
          TELEMETRY_STORAGE = "sqlite",
          TELEMETRY_DB_PATH = tempfile(fileext = ".sqlite"),
          SHINYBEEZ_ENV = shinybeez_env
        ),
        telemetry_utils$init_telemetry()
      )
    }

    it("tags app_name with SHINYBEEZ_ENV when set", {
      tel <- init_with_env("develop")
      expect_false(is.null(tel))
      expect_equal(tel$app_name, "develop")
    })

    it("defaults app_name to 'production' when SHINYBEEZ_ENV is unset", {
      tel <- init_with_env(NA) # NA => with_envvar unsets the variable
      expect_equal(tel$app_name, "production")
    })

    it("defaults app_name to 'production' when SHINYBEEZ_ENV is set but empty", {
      tel <- init_with_env("") # set-but-empty must still default (Sys.getenv default only fires when unset)
      expect_equal(tel$app_name, "production")
    })
  })
})

# Regression tests for shinybeez-analytics#7: the session_start / session_end payloads built in
# app/main.R used to fail inside track_event's tryCatch on every session (reactive value read
# outside a reactive consumer; difftime not JSON-serializable), so no session_start/session_end
# rows were ever written.
describe("session lifecycle payloads (shinybeez-analytics#7)", {
  describe("session_start_data", {
    it("reads clientData outside a reactive context without error", {
      mock_session <- list(
        clientData = shiny::reactiveValues(url_search = "?demo=1", url_hostname = "shinybeez.app")
      )
      payload <- telemetry_utils$session_start_data(mock_session)
      expect_equal(payload, list(url_search = "?demo=1", url = "shinybeez.app"))
    })

    it("serializes to JSON", {
      mock_session <- list(
        clientData = shiny::reactiveValues(url_search = "", url_hostname = "localhost")
      )
      json <- jsonlite::toJSON(telemetry_utils$session_start_data(mock_session), auto_unbox = TRUE)
      expect_equal(as.character(json), '{"url_search":"","url":"localhost"}')
    })
  })

  describe("session_duration_secs", {
    it("returns a bare numeric number of seconds, not a difftime", {
      start <- as.POSIXct("2026-08-29 00:00:00", tz = "UTC")
      end <- start + 90
      dur <- telemetry_utils$session_duration_secs(start, end)
      expect_type(dur, "double")
      expect_false(inherits(dur, "difftime"))
      expect_equal(dur, 90)
    })

    it("serializes to a JSON number (difftime does not)", {
      start <- as.POSIXct("2026-08-29 00:00:00", tz = "UTC")
      end <- start + 12.5
      expect_error(
        jsonlite::toJSON(list(session_duration = difftime(end, start, units = "secs")), auto_unbox = TRUE),
        "difftime"
      )
      json <- jsonlite::toJSON(
        list(session_duration = telemetry_utils$session_duration_secs(start, end)),
        auto_unbox = TRUE
      )
      expect_equal(as.character(json), '{"session_duration":12.5}')
    })
  })

  describe("track_event writes a session_end row", {
    it("stores numeric session_duration and last_tab in event_log", {
      skip_if_not_installed("shiny.telemetry")
      skip_if_not_installed("RSQLite")
      db_path <- tempfile(fileext = ".sqlite")
      withr::defer(
        withr::with_envvar(
          c(R_CONFIG_ACTIVE = "default", TELEMETRY_ENABLED = "FALSE"),
          telemetry_utils$init_telemetry()
        )
      )
      withr::with_envvar(
        c(
          R_CONFIG_ACTIVE = "default",
          TELEMETRY_ENABLED = "TRUE",
          TELEMETRY_STORAGE = "sqlite",
          TELEMETRY_DB_PATH = db_path,
          SHINYBEEZ_ENV = "test"
        ),
        {
          telemetry_utils$init_telemetry()
          start <- Sys.time() - 30
          # session = NULL: shiny.telemetry only accepts a real ShinySession/session_proxy or NULL
          telemetry_utils$track_event(
            "session_end",
            list(
              session_duration = telemetry_utils$session_duration_secs(start),
              last_tab = "Demand"
            ),
            session = NULL
          )
        }
      )

      con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
      withr::defer(DBI::dbDisconnect(con))
      rows <- DBI::dbGetQuery(con, "SELECT type, details FROM event_log WHERE type = 'session_end'")
      expect_equal(nrow(rows), 1)
      details <- jsonlite::fromJSON(rows$details)
      expect_type(details$session_duration, "double")
      expect_gte(details$session_duration, 30)
      expect_equal(details$last_tab, "Demand")
    })
  })
})
