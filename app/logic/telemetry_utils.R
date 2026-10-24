#' Telemetry Utilities for Shinybeez
#'
#' This module provides telemetry tracking capabilities using shiny.telemetry
#' with flexible storage backends (SQLite, PostgreSQL) for containerized deployments.

box::use(
  jsonlite[toJSON],
  rhino[log],
  shiny[isolate],
  glue,
  R6,
  DBI,
)

box::use(
  app / logic / consent_registry,
  app / logic / startup_info,
  app / logic / telemetry_schema,
)

# Global telemetry object
.telemetry_env <- new.env(parent = emptyenv())
.telemetry_env$telemetry <- NULL
.telemetry_env$config <- NULL

#' Initialize telemetry system
#'
#' Sets up telemetry tracking with appropriate storage backend based on configuration
#' @export
init_telemetry <- function() {
  close_telemetry()
  .telemetry_env$config <- config::get("telemetry")

  if (!.telemetry_env$config$enabled) {
    log$info("Telemetry is disabled via configuration")
    .telemetry_env$telemetry <- NULL
    return(NULL)
  }

  # Check if shiny.telemetry is available
  if (!requireNamespace("shiny.telemetry", quietly = TRUE)) {
    log$warn(
      "shiny.telemetry package not available.",
      " Install with: remotes::install_github('Appsilon/shiny.telemetry')"
    )
    return(NULL)
  }

  # Create data storage based on configuration
  data_storage <- create_data_storage(.telemetry_env$config)

  if (is.null(data_storage)) {
    log$error("Failed to create telemetry data storage")
    return(NULL)
  }

  # Tag each event with the deployment environment so production and staging rows are
  # distinguishable in the shared telemetry DB. Sys.getenv()'s default only fires when the
  # variable is unset, so guard against a set-but-empty SHINYBEEZ_ENV as well.
  app_env <- Sys.getenv("SHINYBEEZ_ENV", "production")
  if (!nzchar(app_env)) {
    app_env <- "production"
  }

  # Initialize telemetry object
  .telemetry_env$telemetry <- shiny.telemetry::Telemetry$new(
    app_name = app_env,
    data_storage = data_storage
  )

  # The class, not the configured type: a postgresql request falls back to SQLite.
  log$info("Telemetry system initialized with {class(data_storage)[1]} storage")
  .telemetry_env$telemetry
}

#' Raise the SQLite busy timeout on a telemetry storage connection
#'
#' RSQLite leaves `busy_timeout` at 0, so a write that meets a lock fails immediately with
#' SQLITE_BUSY and does not retry. `track_event()` catches that error and only logs a warning,
#' so the event is lost without the user ever seeing anything wrong. Production and staging
#' containers share one telemetry database file, which makes writer-vs-writer contention
#' possible; a few seconds of retry removes it.
#'
#' shiny.telemetry exposes no hook for connection pragmas, so this reaches the storage object's
#' private connection deliberately. That is version-fragile by nature, so the call is defensive
#' and returns FALSE rather than erroring: telemetry must never take the app down.
#'
#' Telemetry writes happen synchronously on the R event loop, so this timeout is a latency
#' ceiling per write, not a free retry: with a lock held continuously, a module that emits
#' several events in a row multiplies it. The default is therefore kept well above the
#' milliseconds a single small INSERT holds the lock, but low enough that the pathological
#' case stays tolerable. The nightly backup takes a read transaction on this file (sub-second
#' at current size, and it runs at 03:15), which is the other contender this must ride out.
#'
#' @param storage A shiny.telemetry data storage object
#' @param timeout_ms Milliseconds SQLite retries a locked database before giving up.
#'   Must be a finite positive number; anything else is refused, because a zero or negative
#'   value silently restores the original fail-instantly behaviour.
#' @return TRUE if the timeout was applied, otherwise FALSE. Never throws.
#' @export
set_sqlite_busy_timeout <- function(storage, timeout_ms = 2000) {
  tryCatch({
    if (!is.numeric(timeout_ms) || length(timeout_ms) != 1L ||
          !is.finite(timeout_ms) || timeout_ms <= 0) {
      return(FALSE)
    }
    enclos <- storage$.__enclos_env__
    if (is.null(enclos)) {
      return(FALSE)
    }
    con <- enclos$private$db_con
    if (!inherits(con, "DBIConnection") || !DBI::dbIsValid(con)) {
      return(FALSE)
    }
    DBI::dbExecute(con, sprintf("PRAGMA busy_timeout = %d", as.integer(timeout_ms)))
    TRUE
  }, error = function(e) {
    # The handler must not become the thing that throws.
    try(log$warn("Could not set SQLite busy timeout: {e$message}"), silent = TRUE)
    FALSE
  })
}

#' Switch a telemetry SQLite storage to write-ahead logging
#'
#' Several app containers (the pre-warmed seat, the active seats, staging) share one
#' `telemetry.sqlite`. In SQLite's default rollback-journal mode a reader's SHARED lock blocks
#' every writer's COMMIT until `busy_timeout` runs out, and a connection that wants to write
#' while another holds RESERVED gets SQLITE_BUSY at once, whatever the timeout. Both surfaced
#' during the 2026-09-15 container storm as "database is locked" and "Failed to create sqlite
#' storage". WAL lets readers and one writer proceed together and removes the lock-upgrade
#' deadlock.
#'
#' `journal_mode=WAL` is persisted in the database file, so the first container to run this
#' after a deploy flips it for every later connection; `synchronous=NORMAL` is per connection
#' and is the WAL-safe level (a power loss can drop the last transactions, never corrupt the
#' file). Call after `set_sqlite_busy_timeout()`: the mode switch needs a moment with no other
#' writer and, with a timeout set, waits for one instead of failing.
#'
#' @param storage A shiny.telemetry data storage object
#' @return TRUE if the connection reports WAL afterwards, otherwise FALSE. Never throws.
#' @export
set_sqlite_wal <- function(storage) {
  tryCatch({
    enclos <- storage$.__enclos_env__
    if (is.null(enclos)) {
      return(FALSE)
    }
    con <- enclos$private$db_con
    if (!inherits(con, "DBIConnection") || !DBI::dbIsValid(con)) {
      return(FALSE)
    }
    # This pragma answers with the mode now in force, so read it back rather than trust it.
    mode <- DBI::dbGetQuery(con, "PRAGMA journal_mode = WAL")[[1]]
    if (!identical(tolower(as.character(mode)), "wal")) {
      return(FALSE)
    }
    DBI::dbExecute(con, "PRAGMA synchronous = NORMAL")
    TRUE
  }, error = function(e) {
    # The handler must not become the thing that throws.
    try(log$warn("Could not switch SQLite to WAL: {e$message}"), silent = TRUE)
    FALSE
  })
}

# ---------------------------------------------------------------------------------------
# Consent gate, shared by every storage class
# ---------------------------------------------------------------------------------------

drop_reasons <- c("error", "consent", "expired", "no_token", "closed")

new_counters <- function() {
  env <- new.env(parent = emptyenv())
  for (reason in drop_reasons) assign(reason, 0L, envir = env)
  env
}

bump <- function(counters, reason, by = 1L) {
  assign(reason, get(reason, envir = counters) + as.integer(by), envir = counters)
}

counters_vector <- function(counters) {
  vapply(drop_reasons, function(r) get(r, envir = counters), integer(1))
}

#' Decide whether a storage write may proceed
#'
#' Called by every storage class's private `write()` before it touches its backend. The
#' session token on the row selects the consent level from `consent_registry`; a token with
#' no level yet parks the row in the pending buffer, a token for an ended session or a missing
#' token drops it, and a level below the row's tier drops it. Every drop is counted on
#' `counters` under its reason.
#'
#' @param values The row shiny.telemetry built (`time`, `app_name`, `session`, `type`, `details`).
#' @param counters An environment from `new_counters()`.
#' @return `"write"` when the backend write should happen, `"handled"` otherwise.
gate_decision <- function(values, counters) {
  bump(counters, "expired", consent_registry$expire())
  decision <- consent_registry$decide(values$session, values$type)
  switch(decision,
    write = "write",
    buffer = {
      if (!consent_registry$buffer(values$session, values)) {
        bump(counters, "expired")
      }
      "handled"
    },
    drop_consent = {
      bump(counters, "consent")
      "handled"
    },
    drop_no_token = {
      bump(counters, "no_token")
      "handled"
    },
    drop_closed = {
      bump(counters, "closed")
      "handled"
    }
  )
}

#' SQLite telemetry storage whose writes survive other containers' commits
#'
#' Several containers share one `telemetry.sqlite`. shiny.telemetry writes through
#' `RSQLite::dbWriteTable(append = TRUE)`, which opens a *deferred* transaction, reads the
#' schema, then inserts. In WAL mode a connection that read under one snapshot and then finds
#' another writer committed in between gets `SQLITE_BUSY_SNAPSHOT` ("database is locked") at
#' once; SQLite never consults `busy_timeout` for that case. The package's own login write and
#' input observer (both registered by `Telemetry$start_session()`) let that error escape, and
#' Shiny ends the user's session. This was the 2026-09-17 load-test failure at 3+ sessions.
#'
#' The override takes the write lock up front with `BEGIN IMMEDIATE`, which *does* honour
#' `busy_timeout`, so the inherited write runs with no snapshot to lose. If the lock cannot be
#' had within the timeout, or anything else goes wrong, the event is dropped, counted and
#' logged; it is never raised. A user session must not end because a telemetry row failed.
#'
#' Every write first passes the consent gate (`gate_decision()`): this is the one point that
#' sees the rows shiny.telemetry writes on its own as well as the app's `track_*()` rows.
#' `dropped_writes()` reports the counts by reason for tests and health checks.
DataStorageSQLiteResilient <- R6$R6Class( # nolint: object_name_linter.
  "DataStorageSQLiteResilient",
  inherit = shiny.telemetry::DataStorageSQLite,
  public = list(
    initialize = function(...) {
      private$counters <- new_counters()
      super$initialize(...)
    },
    #' @description Writes dropped on this connection since it was opened, by reason:
    #'   `error` (backend failure), `consent`, `expired` (pending buffer), `no_token`, `closed`.
    dropped_writes = function() {
      counters_vector(private$counters)
    },
    #' @description Re-submit rows the consent registry buffered before the level was known.
    replay = function(values_list) {
      for (values in values_list) {
        private$write(values, self$event_bucket)
      }
      invisible(NULL)
    }
  ),
  private = list(
    counters = NULL,
    write = function(values, bucket) {
      if (!identical(gate_decision(values, private$counters), "write")) {
        return(invisible(FALSE))
      }
      con <- private$db_con
      in_txn <- FALSE
      ok <- tryCatch(
        {
          DBI::dbExecute(con, "BEGIN IMMEDIATE")
          in_txn <- TRUE
          super$write(values, bucket)
          DBI::dbExecute(con, "COMMIT")
          TRUE
        },
        error = function(e) {
          if (in_txn) {
            try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
          }
          bump(private$counters, "error")
          msg <- conditionMessage(e)
          # The handler must not become the thing that throws.
          try(log$warn("telemetry write dropped: {msg}"), silent = TRUE)
          FALSE
        }
      )
      invisible(ok)
    }
  )
)

#' Create the guarded SQLite storage
#'
#' @param config Telemetry configuration
create_sqlite_storage <- function(config) {
  # Ensure data directory exists
  db_path <- config$sqlite$db_path
  db_dir <- dirname(db_path)
  if (!dir.exists(db_dir)) {
    dir.create(db_dir, recursive = TRUE)
  }

  storage <- DataStorageSQLiteResilient$new(db_path = db_path)
  # Prod and staging write to the same file; without this a contended write is dropped.
  if (!set_sqlite_busy_timeout(storage)) {
    log$warn("SQLite busy timeout not applied; contended telemetry writes may be lost")
  }
  # Several containers hold this file open at once; WAL keeps their reads from blocking
  # each other's writes. Timeout first so the mode switch waits for a quiet moment.
  if (!set_sqlite_wal(storage)) {
    log$warn("SQLite WAL mode not applied; concurrent containers may hit 'database is locked'")
  }
  storage
}

#' Create data storage backend based on configuration
#'
#' @param config Telemetry configuration
create_data_storage <- function(config) {
  storage_type <- config$storage_type

  tryCatch({
    switch(storage_type,
      "sqlite" = create_sqlite_storage(config),
      "postgresql" = {
        # Fail closed. shiny.telemetry's PostgreSQL storage writes unguarded, so a failed or
        # slow write would end the user's session, and no database is provisioned.
        log$warn(
          "PostgreSQL telemetry storage is not supported (unguarded writes); ",
          "using SQLite instead."
        )
        create_sqlite_storage(config)
      },
      {
        log$error("Unknown storage type: {storage_type}")
        NULL
      }
    )
  }, error = function(e) {
    log$error("Failed to create {storage_type} storage: {e$message}")
    NULL
  })
}

#' Close the cached telemetry storage connection, if any
#'
#' Replacing or nulling the cached Telemetry object without this leaves its SQLite
#' connection open until R garbage-collects it, which RSQLite reports as
#' "call dbDisconnect() when finished working with a connection".
#'
#' @return Invisibly NULL. Never throws.
close_telemetry <- function() {
  if (!is.null(.telemetry_env$telemetry)) {
    try(.telemetry_env$telemetry$data_storage$close(), silent = TRUE)
  }
  .telemetry_env$telemetry <- NULL
  invisible(NULL)
}

#' Get telemetry object
#'
#' Initialises telemetry on first use, then returns the cached object, so callers
#' that run per page load (main$ui) share one storage connection per process.
#'
#' @export
get_telemetry <- function() {
  if (is.null(.telemetry_env$telemetry)) {
    init_telemetry()
  }
  .telemetry_env$telemetry
}

#' Check if telemetry is enabled and available
#'
#' @export
is_telemetry_enabled <- function() {
  config <- config::get("telemetry")
  config$enabled && !is.null(get_telemetry())
}

#' Track custom event
#'
#' @param event_name Name of the event
#' @param event_data Additional data to track with the event
#' @param session Shiny session object
#' @export
track_event <- function(event_name, event_data = list(), session = NULL) {
  if (!is_telemetry_enabled()) {
    return(invisible(NULL))
  }

  telemetry <- get_telemetry()
  if (is.null(telemetry)) {
    return(invisible(NULL))
  }

  tryCatch({
    # Every payload is checked against the event schema first: unknown keys go, user
    # strings are scrubbed, enums outside their set become "other".
    clean <- telemetry_schema$sanitize_event(event_name, event_data)
    if (length(clean$dropped) > 0) {
      dropped_keys <- paste(clean$dropped, collapse = ", ")
      log$debug("telemetry {event_name}: dropped keys {dropped_keys}")
    }
    # Serialize event data to JSON string for the details param
    details <- if (length(clean$data) > 0) {
      toJSON(clean$data, auto_unbox = TRUE, null = "null")
    } else {
      NULL
    }

    # Call the correct shiny.telemetry API method
    telemetry$log_custom_event(
      event_type = event_name,
      details = details,
      session = session
    )

    log$debug("Tracked telemetry event: {event_name}")
  }, error = function(e) {
    log$warn("Failed to track telemetry event {event_name}: {e$message}")
  })
}

# ---------------------------------------------------------------------------------------
# Consent entry points (called by app/view/consent.R and app/main.R)
# ---------------------------------------------------------------------------------------

#' Arguments for shiny.telemetry's `start_session()` at the usage level
#'
#' Kept in one place so a test can pin them. `track_values = FALSE` because the package's
#' own `input` rows never pass through the event schema; `browser_version`, `track_errors`
#' and `track_anonymous_user` are off because the app records its own versions and errors
#' and must set no identifying cookie. The username is the session token, so `login` rows
#' carry nothing a cookie would.
#' @export
usage_start_args <- function(session) {
  list(
    track_inputs = TRUE,
    track_values = FALSE,
    login = TRUE,
    logout = TRUE,
    browser_version = FALSE,
    track_errors = FALSE,
    track_anonymous_user = FALSE,
    session = session,
    username = session$token
  )
}

#' Apply a session's consent level
#'
#' Idempotent: the same level twice does nothing. Otherwise, in this order: record the level;
#' write the canonical `session_start` (once per session, as soon as the level is above
#' `none`); write `consent_change` when a level was already known; release the rows the gate
#' buffered before the level arrived (they keep their earlier timestamps; the consumer's
#' session start is `min(time)`); and at `usage`, start shiny.telemetry's input/login/logout
#' observers, once per session.
#'
#' @param session Shiny session (needs `$token` and `$userData`).
#' @param level One of `none`, `diagnostic`, `usage`.
#' @param source Where the level came from: `stored`, `default`, `default_eu`, `banner`,
#'   `settings`. Recorded as `consent_source` on `session_start` (a change after a known
#'   level is recorded as `changed`) and as `source` on `consent_change`.
#' @param platform `web` or `desktop`; the default reads config.yml.
#' @return Invisibly, the previous level (NULL the first time).
#' @export
set_session_consent <- function(session, level, source, platform = startup_info$platform()) {
  token <- session$token
  current <- consent_registry$get_level(token)
  if (identical(current, level)) {
    return(invisible(current))
  }
  prev <- consent_registry$set_level(token, level)
  if (!is_telemetry_enabled()) {
    return(invisible(prev))
  }
  telemetry <- get_telemetry()

  if (level != "none" && !isTRUE(session$userData$.session_started)) {
    consent_source <- if (is.null(prev)) source else "changed"
    if (identical(consent_source, "changed")) {
      session$userData$.start_time <- Sys.time()
    }
    track_event(
      "session_start",
      c(
        session_start_data(session),
        list(
          consent = level,
          consent_source = consent_source,
          app_version = startup_info$app_version(),
          platform = platform
        )
      ),
      session
    )
    session$userData$.session_started <- TRUE
  }

  if (!is.null(prev)) {
    track_event("consent_change", list(from = prev, to = level, source = source), session)
  }

  tryCatch(
    telemetry$data_storage$replay(consent_registry$drain(token)),
    error = function(e) log$warn("Failed to replay buffered telemetry rows: {e$message}")
  )

  if (level == "usage" && !isTRUE(session$userData$.telemetry_usage_started)) {
    tryCatch(
      {
        do.call(telemetry$start_session, usage_start_args(session))
        session$userData$.telemetry_usage_started <- TRUE
      },
      error = function(e) log$warn("Failed to start telemetry session observers: {e$message}")
    )
  }
  invisible(prev)
}

#' Forget a session's consent at session end
#'
#' Call after the `session_end` row is written. The token is tombstoned so rows written by
#' callbacks that run later (shiny.telemetry's logout) are dropped, not buffered.
#' @export
end_session_consent <- function(session) {
  consent_registry$end_session(session$token)
  invisible(NULL)
}

#' The session's current consent level, `none` when not yet known
#' @export
current_consent <- function(session) {
  level <- consent_registry$get_level(session$token)
  if (is.null(level)) "none" else level
}

#' Build the session_start event payload
#'
#' `session$clientData` is a reactiveValues; reading it at the top level of the server function
#' (outside any reactive consumer) errors, so the read is isolated here.
#' See shinybeez-analytics#7.
#'
#' @param session Shiny session object
#' @return Named list with `url_search` and `url` (hostname)
#' @export
session_start_data <- function(session) {
  isolate(list(
    url_search = session$clientData$url_search,
    url = session$clientData$url_hostname
  ))
}

#' Session duration in seconds as a plain numeric
#'
#' `difftime` objects have no `jsonlite::toJSON` method, so `track_event()` would fail on them.
#' See shinybeez-analytics#7.
#'
#' @param start_time POSIXct session start time
#' @param end_time POSIXct end time (defaults to now)
#' @return Numeric seconds (no `difftime` class)
#' @export
session_duration_secs <- function(start_time, end_time = Sys.time()) {
  as.numeric(difftime(end_time, start_time, units = "secs"))
}

#' Track navigation event
#'
#' @param tab_name Name of the tab navigated to
#' @param session Shiny session object
#' @export
track_navigation <- function(tab_name, session = NULL) {
  track_event(
    event_name = "navigation",
    event_data = list(
      tab = tab_name,
      timestamp = Sys.time()
    ),
    session = session
  )
}

#' Track model fitting event
#'
#' @param model_type Type of model being fitted
#' @param parameters Model parameters
#' @param status Status of the fitting (started, completed, failed)
#' @param session Shiny session object
#' @export
track_model_fitting <- function(
  model_type, parameters = list(), status = "started", session = NULL
) {
  track_event(
    event_name = "model_fitting",
    event_data = list(
      model_type = model_type,
      status = status,
      parameters = parameters,
      timestamp = Sys.time()
    ),
    session = session
  )
}

#' Track data upload event
#'
#' @param file_info Information about uploaded file: `size`, `type`, `rows`, `cols`, and an
#'   optional `reshaped` logical flag marking a file that went through the wide-to-long
#'   mapper (`app/view/wide_mapper.R`) before it was stored, rather than uploaded already in
#'   a template shape. Missing or non-`TRUE` values are recorded as `FALSE`.
#' @param session Shiny session object
#' @export
track_data_upload <- function(file_info = list(), session = NULL) {
  # Remove sensitive file content, keep only metadata
  safe_file_info <- list(
    file_size = file_info$size,
    file_type = file_info$type,
    rows = file_info$rows,
    cols = file_info$cols,
    reshaped = isTRUE(file_info$reshaped)
  )

  track_event(
    event_name = "data_upload",
    event_data = safe_file_info,
    session = session
  )
}

#' Track user interaction with inputs
#'
#' @param input_id ID of the input
#' @param input_type Type of input (selectInput, numericInput, etc.)
#' @param module Module name where interaction occurred
#' @param session Shiny session object
#' @export
track_input_interaction <- function(
  input_id, input_type = "unknown", module = NULL, session = NULL
) {
  config <- config::get("telemetry")

  # Only track input interactions if configured to do so
  if (!config$track_inputs) {
    return(invisible(NULL))
  }

  track_event(
    event_name = "input_interaction",
    event_data = list(
      input_id = input_id,
      input_type = input_type,
      module = module,
      timestamp = Sys.time()
    ),
    session = session
  )
}

#' Track performance metrics
#'
#' @param operation_name Name of the operation
#' @param duration_ms Duration in milliseconds
#' @param additional_metrics Additional performance data
#' @param session Shiny session object
#' @export
track_performance <- function(
  operation_name, duration_ms, additional_metrics = list(), session = NULL
) {
  track_event(
    event_name = "performance",
    event_data = c(
      list(
        operation = operation_name,
        duration_ms = duration_ms,
        timestamp = Sys.time()
      ),
      additional_metrics
    ),
    session = session
  )
}

#' Track validation outcome
#'
#' @param module Module where validation occurred (e.g., "demand", "discounting", "mixed_effects")
#' @param outcome "success" or "failure"
#' @param check_name Which check triggered the outcome (e.g., "check_data", "file_size", "file_read")
#' @param reason Error message or reason for failure (NULL on success)
#' @param session Shiny session object
#' @param shape Optional list from upload_shape(): counts and flags, never names or values
#' @param reason_code Stable code for the failing check (`validate$reason_code()`); the
#'   `reason` text is scrubbed of the user's strings, the code is what analytics groups by.
#' @export
track_validation <- function(
  module, outcome, check_name = NULL, reason = NULL, session = NULL, shape = list(),
  reason_code = NULL
) {
  track_event(
    event_name = "validation_outcome",
    event_data = c(
      list(
        module = module,
        outcome = outcome,
        check_name = check_name,
        reason = if (is.null(reason)) NULL else as.character(reason),
        reason_code = reason_code,
        timestamp = Sys.time()
      ),
      shape
    ),
    session = session
  )
}

#' Track configuration snapshot at model fit time
#'
#' @param module Module name (e.g., "demand", "discounting", "mixed_effects")
#' @param config Named list of current user settings
#' @param session Shiny session object
#' @export
track_configuration <- function(module, config = list(), session = NULL) {
  track_event(
    event_name = "configuration_snapshot",
    event_data = c(
      list(module = module, timestamp = Sys.time()),
      config
    ),
    session = session
  )
}

# TRUE for a single, non-NA, non-empty string.
is_nonempty_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

#' Module that owns a plot download
#'
#' Plot downloads are reported by a document-level JS click handler. The module is
#' recovered from the filename prefix each render_ggplot() call sets. esquisse
#' renders its download links without a filename in the `download` attribute, so
#' when the filename gives no answer the namespaced id of the clicked link (for
#' example `app-mixed_effects_demand-mixed_model_plot-export_png`) is used instead.
#' @param filename Download filename (may be NULL or empty).
#' @param id Id of the clicked download link (may be NULL or empty).
#' @return One of `"demand"`, `"mixed_effects"`, `"discounting"`, `"unknown"`.
#' @export
plot_download_module <- function(filename = NULL, id = NULL) {
  prefixes <- c(
    "shinybeez-mixed-effects" = "mixed_effects",
    "shinybeez-discounting" = "discounting",
    "shinybeez-demand" = "demand"
  )
  if (is_nonempty_string(filename)) {
    for (prefix in names(prefixes)) {
      if (startsWith(filename, prefix)) {
        return(prefixes[[prefix]])
      }
    }
  }
  if (is_nonempty_string(id)) {
    # Alternation order matters: mixed_effects_demand must be tried before demand.
    hit <- regmatches(id, regexpr("(^|-)(mixed_effects_demand|discounting|demand)-", id))
    if (length(hit) == 1L) {
      segments <- c(
        mixed_effects_demand = "mixed_effects",
        discounting = "discounting",
        demand = "demand"
      )
      return(segments[[gsub("^-|-$", "", hit)]])
    }
  }
  "unknown"
}

# esquisse names its export links `...-export_<format>` (or `export-<format>` in the "More options" modal).
plot_export_id_pattern <- "export[_-](png|pdf|svg|jpeg|bmp|eps|tiff|pptx)$"

#' Whether a clicked download is a plot export
#'
#' The document-level download click handler also fires for non-plot `a[download]`
#' links (Export All xlsx, long-format CSV). A download counts as a plot export when
#' its filename carries the `shinybeez-` prefix every render_ggplot() call sets, or
#' when the clicked link id is an esquisse `export_<format>` / `export-<format>` link.
#' @param filename Download filename (may be NULL or empty).
#' @param id Id of the clicked download link (may be NULL or empty).
#' @return `TRUE` or `FALSE`.
#' @export
is_plot_download <- function(filename = NULL, id = NULL) {
  if (is_nonempty_string(filename) && startsWith(filename, "shinybeez-")) {
    return(TRUE)
  }
  is_nonempty_string(id) && grepl(plot_export_id_pattern, id)
}

#' File format of a plot download
#'
#' Reads the extension from the download filename when there is one, otherwise the
#' `export_<format>` (or `export-<format>`) suffix of the esquisse download link id.
#' @param filename Download filename (may be NULL or empty).
#' @param id Id of the clicked download link (may be NULL or empty).
#' @return Lower-case format such as `"png"`, or `"unknown"`.
#' @export
plot_download_format <- function(filename = NULL, id = NULL) {
  if (is_nonempty_string(filename) && grepl(".", filename, fixed = TRUE)) {
    ext <- tolower(sub("^.*\\.", "", filename))
    if (nzchar(ext)) {
      return(ext)
    }
  }
  if (is_nonempty_string(id)) {
    hit <- regmatches(id, regexec(plot_export_id_pattern, id))[[1]]
    if (length(hit) == 2L) {
      return(tolower(hit[[2]]))
    }
  }
  "unknown"
}

#' Track export/download event
#'
#' @param export_type Type of export (e.g., "csv", "excel", "xlsx", "png", "svg")
#' @param module Module where export occurred (e.g., "demand", "discounting", "mixed_effects")
#' @param file_format File format of the export
#' @param row_count Number of rows exported (if applicable)
#' @param session Shiny session object
#' @export
track_export <- function(
  export_type, module = NULL, file_format = NULL,
  row_count = NULL, session = NULL
) {
  track_event(
    event_name = "result_export",
    event_data = list(
      export_type = export_type,
      module = module,
      file_format = file_format,
      row_count = row_count,
      timestamp = Sys.time()
    ),
    session = session
  )
}

#' Track error events
#'
#' @param error_message Error message
#' @param error_context Context where error occurred
#' @param session Shiny session object
#' @export
#' @param details Optional named list of caller context (for example the fit spec
#'   and data size) nested under `details` in the event JSON. Counts only, never
#'   raw data.
track_error <- function(error_message, error_context = NULL, session = NULL, details = NULL) {
  event_data <- list(
    error_message = error_message,
    context = error_context,
    timestamp = Sys.time()
  )
  # Nested rather than flattened so caller-supplied keys can never collide with
  # the top-level schema the analytics consumer reads (error_message, context).
  if (!is.null(details)) {
    event_data$details <- details
  }
  track_event(
    event_name = "error",
    event_data = event_data,
    session = session
  )
}

#' Track the wide-to-long mapper lifecycle
#'
#' @param target Module name, same vocabulary as track_validation(): "demand",
#'   "mixed_effects" or "discounting"
#' @param outcome "opened", "confirmed" or "cancelled"
#' @param summary Optional list: n_series, x_source, n_cols_in, n_rows_out, n_dropped.
#'   Never column names or cell values.
#' @export
track_reshape <- function(target, outcome, summary = list(), session = NULL) {
  track_event(
    event_name = "reshape",
    event_data = c(
      list(target = target, outcome = outcome, timestamp = Sys.time()),
      summary
    ),
    session = session
  )
}

#' Get telemetry data for analysis
#'
#' @param start_date Start date for data retrieval
#' @param end_date End date for data retrieval
#' @export
get_telemetry_data <- function(start_date = Sys.Date() - 30, end_date = Sys.Date()) {
  if (!is_telemetry_enabled()) {
    log$warn("Telemetry is not enabled")
    return(NULL)
  }

  telemetry <- get_telemetry()
  if (is.null(telemetry)) {
    return(NULL)
  }

  tryCatch({
    telemetry$data_storage$read_event_data(
      start_date = as.character(start_date),
      end_date = as.character(end_date)
    )
  }, error = function(e) {
    log$error("Failed to retrieve telemetry data: {e$message}")
    NULL
  })
}

#' Create session-aware telemetry tracker
#'
#' Returns a list of telemetry functions that automatically include session information
#'
#' @param session Shiny session object
#' @export
create_session_telemetry <- function(session) {
  list(
    track_event = function(event_name, event_data = list()) {
      track_event(event_name, event_data, session)
    },
    track_navigation = function(tab_name) {
      track_navigation(tab_name, session)
    },
    track_model_fitting = function(model_type, parameters = list(), status = "started") {
      track_model_fitting(model_type, parameters, status, session)
    },
    track_data_upload = function(file_info = list()) {
      track_data_upload(file_info, session)
    },
    track_input_interaction = function(input_id, input_type = "unknown", module = NULL) {
      track_input_interaction(input_id, input_type, module, session)
    },
    track_performance = function(operation_name, duration_ms, additional_metrics = list()) {
      track_performance(operation_name, duration_ms, additional_metrics, session)
    },
    track_error = function(error_message, error_context = NULL, details = NULL) {
      track_error(error_message, error_context, session, details)
    },
    track_export = function(export_type, module = NULL, file_format = NULL, row_count = NULL) {
      track_export(export_type, module, file_format, row_count, session)
    },
    track_validation = function(
      module, outcome, check_name = NULL, reason = NULL, shape = list(), reason_code = NULL
    ) {
      track_validation(module, outcome, check_name, reason, session, shape, reason_code)
    },
    track_reshape = function(target, outcome, summary = list()) {
      track_reshape(target, outcome, summary, session)
    },
    track_configuration = function(module, config = list()) {
      track_configuration(module, config, session)
    },
    set_consent = function(level, source) {
      set_session_consent(session, level, source)
    },
    consent = function() {
      current_consent(session)
    }
  )
}
