#' Async daemon pool
#'
#' One mirai worker pool per R process. Under ShinyProxy a process is one user
#' session, so the default of one daemon is one worker per user; on shared-process
#' hosts (shinyapps.io, Connect Cloud) the pool is shared and tasks queue.
#' `SHINYBEEZ_DAEMONS=0` selects the retained synchronous path — and is the required
#' setting on Connect Cloud until E1's spike validates daemons there (roadmap B4). Under the
#' `shinyapps` and `connectcloud` profiles it is also the default when the variable is unset.

box::use(
  mirai,
)

read_env_number <- function(name, default, min_value) {
  raw <- Sys.getenv(name, unset = "")
  if (!nzchar(raw)) return(default)
  value <- suppressWarnings(as.numeric(raw))
  if (is.na(value) || value < min_value) default else value
}

# Shared-process hosts where daemons are not validated yet. shinyapps.io cannot be given
# environment variables at deploy time, so the safe setting has to be the default there.
hosted_profiles <- c("shinyapps", "connectcloud")

default_daemon_count <- function() {
  if (Sys.getenv("R_CONFIG_ACTIVE") %in% hosted_profiles) 0 else 1
}

#' @export
daemon_count <- function() {
  as.integer(read_env_number("SHINYBEEZ_DAEMONS", default = default_daemon_count(), min_value = 0))
}

#' @export
fit_timeout_ms <- function() {
  read_env_number("SHINYBEEZ_FIT_TIMEOUT_MS", default = 600000, min_value = 1)
}

#' @export
rss_limit_mb <- function() {
  read_env_number("SHINYBEEZ_DAEMON_RSS_LIMIT_MB", default = 900, min_value = 1)
}

#' @export
async_enabled <- function() {
  daemon_count() > 0L
}

#' @export
start_daemons <- function(n = daemon_count(), box_path = getOption("box.path", getwd())) {
  if (n == 0L) return(invisible(0L))
  mirai$daemons(n, dispatcher = TRUE)
  mirai$everywhere(
    {
      options(box.path = box_path)
      suppressPackageStartupMessages({
        library(beezdemand)
        library(beezdiscounting)
      })
    },
    box_path = box_path
  )
  invisible(as.integer(n))
}

#' @export
stop_daemons <- function() {
  invisible(mirai$daemons(0))
}

connected_daemons <- function() {
  status <- tryCatch(mirai$status(), error = function(e) NULL)
  n <- status$connections
  if (is.null(n)) 0L else as.integer(n)
}

#' @export
pool_idle <- function() {
  status <- tryCatch(mirai$status(), error = function(e) NULL)
  counts <- status$mirai
  if (is.null(counts)) return(TRUE)
  isTRUE(counts[["executing"]] == 0) && isTRUE(counts[["awaiting"]] == 0)
}

#' @export
recycle_daemons <- function(if_idle = TRUE) {
  if (if_idle && !pool_idle()) return(invisible(FALSE))
  stop_daemons()
  start_daemons()
  invisible(TRUE)
}

#' @export
ensure_daemons <- function() {
  if (!async_enabled() || connected_daemons() > 0L) return(invisible(FALSE))
  start_daemons()
  invisible(TRUE)
}
