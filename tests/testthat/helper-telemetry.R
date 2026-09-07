# Telemetry round-trip helpers. Two traps, both silent (see memory note
# project_telemetry_test_gotchas): helper-integration.R sets TELEMETRY_ENABLED=FALSE
# process-wide and is_telemetry_enabled() re-reads config on every event, so the env
# vars must stay set for the WHOLE test; and shiny.telemetry needs an R6 ShinySession
# (or session_proxy) to read $token from.

with_sqlite_telemetry <- function(env = parent.frame()) {
  box::use(app / logic / telemetry_utils)
  db_path <- withr::local_tempfile(fileext = ".sqlite", .local_envir = env)
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

read_event_rows <- function(db_path, type) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(
    con,
    sprintf("SELECT session, type, details FROM event_log WHERE type = '%s'", type)
  )
}

# MockShinySession has a token but not the ShinySession class shiny.telemetry checks for.
as_telemetry_session <- function(session) {
  class(session) <- c("ShinySession", class(session))
  session
}

fake_session <- function() as_telemetry_session(shiny::MockShinySession$new())
