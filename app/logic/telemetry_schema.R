#' Consent tiers and the event schema for telemetry rows
#'
#' Two tables that every telemetry row is checked against before it is stored:
#'
#' - `event_tier` says which consent level a row type needs (`none` < `diagnostic` < `usage`).
#'   The storage gate in `telemetry_utils.R` consults it for every write, including the rows
#'   shiny.telemetry writes on its own (`input`, `login`, `logout`), which never pass through
#'   `track_event()`.
#' - `event_schema` says which keys a type may carry and what kind of value each key holds.
#'   `sanitize_event()` applies it before JSON serialisation so no payload can carry a user's
#'   column names, cell values or file names, whichever helper built it.
#'
#' Design: docs/superpowers/specs/2026-10-24-analytics-consent-tiers-design.md §4 and §10.

box::use(
  rhino[log],
)

#' @export
LEVELS <- c("none", "diagnostic", "usage") # nolint: object_name_linter.

#' Position of a consent level in the order none < diagnostic < usage
#'
#' @param level A level name. Anything outside `LEVELS` (including NULL) ranks as `none`.
#' @return Integer 0, 1 or 2.
#' @export
level_rank <- function(level) {
  if (is.null(level) || length(level) != 1L || !level %in% LEVELS) {
    return(0L)
  }
  match(level, LEVELS) - 1L
}

# Type -> the lowest level that may write it. `consent_change` is the consent record itself
# and is written at every level, including `none`.
event_tier <- c(
  consent_change = "none",
  session_start = "diagnostic",
  session_end = "diagnostic",
  error = "diagnostic",
  validation_outcome = "diagnostic",
  performance = "diagnostic",
  navigation = "usage",
  input = "usage",
  login = "usage",
  logout = "usage",
  browser_version = "usage",
  model_fitting = "usage",
  configuration_snapshot = "usage",
  data_upload = "usage",
  result_export = "usage",
  reshape = "usage",
  input_interaction = "usage"
)

.warned_types <- new.env(parent = emptyenv())

#' Consent level a row type needs
#'
#' @param type Event type string.
#' @return `"none"`, `"diagnostic"` or `"usage"`. An unknown type is `usage` (the most
#'   restrictive useful default) and is logged once per process.
#' @export
tier_of <- function(type) {
  if (is.character(type) && length(type) == 1L && !is.na(type) && type %in% names(event_tier)) {
    return(event_tier[[type]])
  }
  key <- if (is.character(type) && length(type) == 1L && !is.na(type)) type else "<invalid>"
  if (!exists(key, envir = .warned_types, inherits = FALSE)) {
    assign(key, TRUE, envir = .warned_types)
    try(log$warn("telemetry: unknown event type '{key}' treated as usage tier"), silent = TRUE)
  }
  "usage"
}

#' Whether a session at `level` may write a row of `type`
#'
#' @export
level_allows <- function(level, type) {
  tier <- tier_of(type)
  if (identical(tier, "none")) {
    return(TRUE)
  }
  level_rank(level) >= level_rank(tier)
}

#' Remove user-provided text from a message
#'
#' Validation and error messages quote the offending header or value in double quotes
#' (`Could not parse "age"`). Every double-quoted substring becomes `"…"` and the result is
#' truncated, so a message keeps its shape for grouping but none of the user's strings.
#'
#' @param x A character scalar (anything else is returned unchanged).
#' @param max_chars Length limit after scrubbing.
#' @export
scrub_user_strings <- function(x, max_chars = 500L) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    return(x)
  }
  out <- gsub('"[^"]*"', '"…"', x)
  if (nchar(out) > max_chars) {
    out <- substr(out, 1L, max_chars)
  }
  out
}

# ---------------------------------------------------------------------------------------
# Event schema
# ---------------------------------------------------------------------------------------

nav_values <- c("Welcome", "Demand", "Discounting", "MixedEffectsDemand")
module_values <- c("demand", "discounting", "mixed_effects")

enum <- function(values) list(kind = "enum", values = values)
text <- function(max_chars) list(kind = "text", max_chars = max_chars)
number <- list(kind = "number")
flag <- list(kind = "flag")
time <- list(kind = "time")
nested <- list(kind = "nested")

event_schema <- list(
  session_start = list(
    url_search = text(200L), url = text(100L),
    consent = enum(LEVELS),
    consent_source = enum(c("stored", "default", "default_eu", "changed")),
    app_version = text(20L), platform = enum(c("web", "desktop")),
    r_version = text(20L), os = text(40L)
  ),
  session_end = list(
    session_duration = number, last_tab = enum(nav_values), consent = enum(LEVELS)
  ),
  consent_change = list(
    from = enum(c(LEVELS, "unset")), to = enum(LEVELS), source = enum(c("banner", "settings"))
  ),
  navigation = list(tab = enum(nav_values), timestamp = time),
  error = list(
    error_message = text(500L), context = text(100L), details = nested, timestamp = time
  ),
  validation_outcome = list(
    module = enum(module_values), outcome = enum(c("success", "failure")),
    check_name = text(40L), reason = text(500L), reason_code = text(40L), timestamp = time,
    n_cols = number, n_rows = number, n_numeric_cols = number,
    delim = enum(c("comma", "semicolon", "tab", "unknown")),
    first_col_id_like = flag, any_id_like = flag, has_qualtrics_meta = flag
  ),
  performance = list(
    operation = text(80L), duration_ms = number, status = enum(c("success", "error")),
    timestamp = time
  ),
  model_fitting = list(
    model_type = enum(module_values),
    status = enum(c("started", "completed", "failed", "cancelled", "timeout")),
    parameters = nested, timestamp = time
  ),
  configuration_snapshot = list(module = enum(module_values), timestamp = time),
  data_upload = list(
    file_size = number, rows = number, cols = number,
    file_type = enum(c("csv", "tsv", "txt", "xlsx", "xls", "other")), reshaped = flag
  ),
  result_export = list(
    export_type = text(20L), module = enum(c(module_values, "unknown")),
    file_format = text(10L), row_count = number, timestamp = time
  ),
  reshape = list(
    target = enum(module_values), outcome = enum(c("opened", "confirmed", "cancelled")),
    layout = enum(c("wide", "long")),
    n_series = number, n_cols_in = number, n_rows_out = number, n_dropped = number,
    x_source = text(20L), timestamp = time
  ),
  input_interaction = list(
    input_id = text(80L), input_type = text(40L), module = text(40L), timestamp = time
  )
)

# Types whose keys outside the schema are not dropped but passed through the nested rule
# (their payload is a caller-supplied settings list with module-specific keys).
open_types <- "configuration_snapshot"

nested_text_keys <- c("error", "message", "reason")
nested_token_pattern <- "^[A-Za-z0-9_.-]{1,40}$"

is_scalar <- function(x) is.atomic(x) && length(x) == 1L && !is.na(x)

# Returns the value to keep, or NULL to drop the key.
coerce_value <- function(value, rule) {
  switch(rule$kind,
    number = if (is.numeric(value) && length(value) == 1L) value else NULL,
    flag = if (is.logical(value) && length(value) == 1L && !is.na(value)) value else NULL,
    enum = if (is_scalar(value) && as.character(value) %in% rule$values) {
      as.character(value)
    } else {
      "other"
    },
    text = if (is.character(value) && length(value) == 1L) {
      scrub_user_strings(value, rule$max_chars)
    } else {
      NULL
    },
    time = if (inherits(value, "POSIXt") || (is.character(value) && length(value) == 1L)) {
      value
    } else {
      NULL
    },
    nested = NULL
  )
}

# The nested rule: numbers, flags and NULLs pass; a string passes when it is a code-shaped
# token, or (under a text key) when scrubbed; everything else is dropped.
nested_value <- function(key, value) {
  if (is.null(value)) {
    return(list(keep = TRUE, value = NULL))
  }
  if (is.numeric(value) && length(value) == 1L) {
    return(list(keep = TRUE, value = value))
  }
  if (is.logical(value) && length(value) == 1L) {
    return(list(keep = TRUE, value = value))
  }
  if (is.character(value) && length(value) == 1L && !is.na(value)) {
    if (key %in% nested_text_keys) {
      return(list(keep = TRUE, value = scrub_user_strings(value, 200L)))
    }
    if (grepl(nested_token_pattern, value)) {
      return(list(keep = TRUE, value = value))
    }
  }
  list(keep = FALSE, value = NULL)
}

sanitize_nested <- function(payload, prefix) {
  out <- list()
  dropped <- character()
  for (key in names(payload)) {
    res <- nested_value(key, payload[[key]])
    if (res$keep) {
      out[key] <- list(res$value)
    } else {
      dropped <- c(dropped, paste0(prefix, key))
    }
  }
  list(data = out, dropped = dropped)
}

#' Check an event payload against the schema
#'
#' @param type Event type.
#' @param event_data Named list as built by a `track_*()` helper.
#' @return `list(data = <sanitised list>, dropped = <character keys that were removed>)`.
#'   An unknown type is returned unchanged (the storage gate still applies its tier).
#' @export
sanitize_event <- function(type, event_data = list()) {
  schema <- event_schema[[type]]
  if (is.null(schema) || length(event_data) == 0L) {
    return(list(data = event_data, dropped = character()))
  }
  out <- list()
  dropped <- character()
  for (key in names(event_data)) {
    value <- event_data[[key]]
    rule <- schema[[key]]
    if (is.null(rule)) {
      if (type %in% open_types) {
        res <- nested_value(key, value)
        if (res$keep) {
          out[key] <- list(res$value)
        } else {
          dropped <- c(dropped, key)
        }
      } else {
        dropped <- c(dropped, key)
      }
      next
    }
    if (is.null(value)) {
      out[key] <- list(NULL)
      next
    }
    if (identical(rule$kind, "nested")) {
      if (is.list(value)) {
        res <- sanitize_nested(value, paste0(key, "."))
        out[[key]] <- res$data
        dropped <- c(dropped, res$dropped)
      } else {
        dropped <- c(dropped, key)
      }
      next
    }
    kept <- coerce_value(value, rule)
    if (is.null(kept)) {
      dropped <- c(dropped, key)
    } else {
      out[[key]] <- kept
    }
  }
  list(data = out, dropped = dropped)
}
