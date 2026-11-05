#' Per-process consent registry: session token -> consent level
#'
#' The storage gate (`telemetry_utils.R`) asks this module whether a row may be written.
#' Three states matter:
#'
#' - the level is known: the row is written or dropped by its tier (`telemetry_schema`);
#' - the level is not known yet (the browser has not sent its stored choice; the first
#'   `navigation` row fires before it can): the row waits in a small pending buffer, which the
#'   consent handshake drains once the level arrives. A buffer older than `PENDING_MAX_AGE_S`
#'   is discarded (fail closed);
#' - the session has ended: the token is tombstoned for `TOMBSTONE_S` so the logout rows
#'   shiny.telemetry writes after our own `session_end` are dropped, not buffered forever.
#'
#' Design: docs/superpowers/specs/2026-10-24-analytics-consent-tiers-design.md §5.

box::use(
  app / logic / telemetry_schema,
)

#' @export
PENDING_CAP <- 50L # nolint: object_name_linter.
#' @export
PENDING_MAX_AGE_S <- 10 # nolint: object_name_linter.
#' @export
TOMBSTONE_S <- 60 # nolint: object_name_linter.

.state <- new.env(parent = emptyenv())

#' Forget everything (tests).
#' @export
reset <- function() {
  .state$levels <- new.env(parent = emptyenv())
  .state$buffers <- new.env(parent = emptyenv())
  .state$closed <- new.env(parent = emptyenv())
  invisible(NULL)
}
reset()

valid_token <- function(token) {
  is.character(token) && length(token) == 1L && !is.na(token) && nzchar(token)
}

#' Record a session's consent level
#' @return The previous level, or NULL when the token was unknown.
#' @export
set_level <- function(token, level) {
  prev <- get_level(token)
  assign(token, level, envir = .state$levels)
  if (exists(token, envir = .state$closed, inherits = FALSE)) {
    rm(list = token, envir = .state$closed)
  }
  prev
}

#' @export
get_level <- function(token) {
  if (!valid_token(token) || !exists(token, envir = .state$levels, inherits = FALSE)) {
    return(NULL)
  }
  get(token, envir = .state$levels, inherits = FALSE)
}

#' Forget a session and tombstone its token
#' @export
end_session <- function(token, now = Sys.time()) {
  if (!valid_token(token)) {
    return(invisible(NULL))
  }
  if (exists(token, envir = .state$levels, inherits = FALSE)) {
    rm(list = token, envir = .state$levels)
  }
  if (exists(token, envir = .state$buffers, inherits = FALSE)) {
    rm(list = token, envir = .state$buffers)
  }
  assign(token, now, envir = .state$closed)
  invisible(NULL)
}

#' What the gate should do with a row
#'
#' @return One of `"write"`, `"buffer"`, `"drop_consent"`, `"drop_no_token"`, `"drop_closed"`.
#' @export
decide <- function(token, type, now = Sys.time()) {
  if (!valid_token(token)) {
    return("drop_no_token")
  }
  if (exists(token, envir = .state$closed, inherits = FALSE)) {
    return("drop_closed")
  }
  level <- get_level(token)
  if (is.null(level)) {
    return("buffer")
  }
  if (telemetry_schema$level_allows(level, type)) "write" else "drop_consent"
}

#' Hold a row until the token's level is known
#' @return TRUE if stored, FALSE if the token's buffer is full.
#' @export
buffer <- function(token, values, now = Sys.time()) {
  if (!valid_token(token)) {
    return(FALSE)
  }
  entry <- if (exists(token, envir = .state$buffers, inherits = FALSE)) {
    get(token, envir = .state$buffers, inherits = FALSE)
  } else {
    list(rows = list(), times = numeric())
  }
  if (length(entry$rows) >= PENDING_CAP) {
    return(FALSE)
  }
  entry$rows[[length(entry$rows) + 1L]] <- values
  entry$times <- c(entry$times, as.numeric(now))
  assign(token, entry, envir = .state$buffers)
  TRUE
}

#' Take and clear a token's buffered rows, in insertion order
#' @export
drain <- function(token) {
  if (!valid_token(token) || !exists(token, envir = .state$buffers, inherits = FALSE)) {
    return(list())
  }
  rows <- get(token, envir = .state$buffers, inherits = FALSE)$rows
  rm(list = token, envir = .state$buffers)
  rows
}

#' Discard stale buffered rows and old tombstones
#' @return Integer count of buffered rows removed.
#' @export
expire <- function(now = Sys.time(), max_age_s = PENDING_MAX_AGE_S, tombstone_s = TOMBSTONE_S) {
  cutoff <- as.numeric(now) - max_age_s
  removed <- 0L
  for (token in ls(.state$buffers)) {
    entry <- get(token, envir = .state$buffers, inherits = FALSE)
    stale <- entry$times < cutoff
    removed <- removed + sum(stale)
    if (all(stale)) {
      rm(list = token, envir = .state$buffers)
    } else if (any(stale)) {
      entry$rows <- entry$rows[!stale]
      entry$times <- entry$times[!stale]
      assign(token, entry, envir = .state$buffers)
    }
  }
  tomb_cutoff <- as.numeric(now) - tombstone_s
  for (token in ls(.state$closed)) {
    if (as.numeric(get(token, envir = .state$closed, inherits = FALSE)) < tomb_cutoff) {
      rm(list = token, envir = .state$closed)
    }
  }
  removed
}
