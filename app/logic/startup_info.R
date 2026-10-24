#' Startup summary line
#'
#' One log line per R process: the effective config profile, the telemetry backend and
#' whether fits run on daemons. On a hosted platform the log panel is the only place
#' these can be read, and they decide how the app behaves there.
#'
#' Every field is matched against a fixed vocabulary before it is printed, so a
#' mis-set variable (a connection string in TELEMETRY_STORAGE, say) can never reach
#' the log. Anything else prints as "(unrecognised)".

box::use(
  app / logic / async / daemons,
)

known_profiles <- c("default", "development", "production", "shinyapps", "connectcloud", "shinyproxy")
known_backends <- c("sqlite", "postgresql")

# Release version, recorded on every telemetry session_start row. NEWS.md is not shipped in
# the Docker image or the desktop bundle, so the value lives here; a test pins it to the
# first NEWS.md heading so a release bump cannot skip it.
APP_VERSION <- "1.3.0" # nolint: object_name_linter.

#' @export
app_version <- function() APP_VERSION

#' Which build this process is: the hosted web app or the packaged desktop app
#'
#' Read from `platform` in config.yml (the default profile says `web`; only the desktop
#' profile says `desktop`). Anything else is treated as web, the safer of the two for
#' telemetry: desktop-only fields are never attached to a hosted session.
#' @export
platform <- function(value = config::get("platform")) {
  if (identical(value, "desktop")) "desktop" else "web"
}

from_vocabulary <- function(value, vocabulary) {
  if (is.character(value) && length(value) == 1L && value %in% vocabulary) value else "(unrecognised)"
}

#' @export
startup_line <- function(
  config_active = Sys.getenv("R_CONFIG_ACTIVE"),
  telemetry = config::get("telemetry"),
  daemon_count = daemons$daemon_count()
) {
  profile <- if (nzchar(config_active)) from_vocabulary(config_active, known_profiles) else "(unset, so default)"
  backend <- if (isTRUE(telemetry$enabled)) from_vocabulary(telemetry$storage_type, known_backends) else "disabled"
  fits <- if (daemon_count > 0L) "async fits" else "synchronous fits"

  sprintf(
    "startup: R_CONFIG_ACTIVE=%s telemetry=%s daemons=%d (%s)",
    profile, backend, as.integer(daemon_count), fits
  )
}
