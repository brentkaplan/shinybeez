#' Telemetry Utilities for Shinybeez
#'
#' This module provides telemetry tracking capabilities using shiny.telemetry
#' with flexible storage backends (SQLite, PostgreSQL) for containerized deployments.

box::use(
  rhino[log],
  glue
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
  .telemetry_env$config <- config::get("telemetry")

  if (!.telemetry_env$config$enabled) {
    log$info("Telemetry is disabled via configuration")
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

  # Initialize telemetry object
  .telemetry_env$telemetry <- shiny.telemetry::Telemetry$new(
    data_storage = data_storage
  )

  log$info("Telemetry system initialized with {.telemetry_env$config$storage_type} backend")
  .telemetry_env$telemetry
}

#' Create data storage backend based on configuration
#'
#' @param config Telemetry configuration
create_data_storage <- function(config) {
  storage_type <- config$storage_type

  tryCatch({
    switch(storage_type,
      "sqlite" = {
        # Ensure data directory exists
        db_path <- config$sqlite$db_path
        db_dir <- dirname(db_path)
        if (!dir.exists(db_dir)) {
          dir.create(db_dir, recursive = TRUE)
        }

        shiny.telemetry::DataStorageSQLite$new(db_path = db_path)
      },
      "postgresql" = {
        shiny.telemetry::DataStoragePostgreSQL$new(
          host = config$postgresql$host,
          port = config$postgresql$port,
          dbname = config$postgresql$dbname,
          user = config$postgresql$user,
          password = config$postgresql$password
        )
      },
      "mariadb" = {
        shiny.telemetry::DataStorageMariaDB$new(
          host = config$postgresql$host,
          port = config$postgresql$port,
          dbname = config$postgresql$dbname,
          user = config$postgresql$user,
          password = config$postgresql$password
        )
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

#' Get telemetry object
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
    # Add session info if available
    if (!is.null(session)) {
      event_data$session_token <- session$token
    }

    # Track the event
    telemetry$track_event(
      event_name = event_name,
      event_data = event_data
    )

    log$debug("Tracked telemetry event: {event_name}")
  }, error = function(e) {
    log$warn("Failed to track telemetry event {event_name}: {e$message}")
  })
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
    model_type, parameters = list(), status = "started", session = NULL) {
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
#' @param file_info Information about uploaded file
#' @param session Shiny session object
#' @export
track_data_upload <- function(file_info = list(), session = NULL) {
  # Remove sensitive file content, keep only metadata
  safe_file_info <- list(
    file_size = file_info$size,
    file_type = file_info$type,
    rows = file_info$rows,
    cols = file_info$cols
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
    input_id, input_type = "unknown", module = NULL, session = NULL) {
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
    operation_name, duration_ms, additional_metrics = list(), session = NULL) {
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

#' Track error events
#'
#' @param error_message Error message
#' @param error_context Context where error occurred
#' @param session Shiny session object
#' @export
track_error <- function(error_message, error_context = NULL, session = NULL) {
  track_event(
    event_name = "error",
    event_data = list(
      error_message = error_message,
      context = error_context,
      timestamp = Sys.time()
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
    track_error = function(error_message, error_context = NULL) {
      track_error(error_message, error_context, session)
    }
  )
}
