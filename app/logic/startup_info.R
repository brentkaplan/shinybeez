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
