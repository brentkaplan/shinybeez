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
