#' Centralized Logging Utilities for Shinybeez
#' 
#' This module provides standardized logging functions that extend Rhino's
#' built-in logging capabilities with structured logging, performance monitoring,
#' and specialized logging for different event types.

box::use(
  rhino[log],
  glue,
  jsonlite,
  logger,
  utils[capture.output]
)

# Initialize logging configuration using environment instead of locked binding
.logging_env <- new.env(parent = emptyenv())
.logging_env$config <- NULL

#' Initialize logging system
#' 
#' Sets up the logging configuration and creates necessary log files
#' @export
init_logging <- function() {
  .logging_env$config <- config::get("logging")
  
  # Ensure log directories exist
  log_files <- c(
    .logging_env$config$app_log_file,
    .logging_env$config$error_log_file,
    .logging_env$config$performance_log_file,
    .logging_env$config$activity_log_file
  )
  
  for (log_file in log_files) {
    log_dir <- dirname(log_file)
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE)
    }
  }
  
  # Use a safer log message without JSON in glue template
  config_summary <- paste(names(.logging_env$config), collapse = ", ")
  log$info("Logging system initialized with config keys: {config_summary}")
}

#' Get logging configuration
#' @export
get_logging_config <- function() {
  if (is.null(.logging_env$config)) {
    init_logging()
  }
  .logging_env$config
}

#' Write to category-specific log files
#' 
#' @param category Log category
#' @param level Log level
#' @param formatted_message Formatted log message
#' @param log_entry Full log entry data
write_to_category_log <- function(category, level, formatted_message, log_entry) {
  config <- get_logging_config()
  
  # Determine which log file to write to based on category
  log_file <- switch(category,
    "error" = config$error_log_file,
    "performance" = config$performance_log_file,
    "user_activity" = config$activity_log_file,
    "data_processing" = config$app_log_file,
    "model_fitting" = config$app_log_file,
    "session_lifecycle" = config$activity_log_file,
    "module_init" = config$app_log_file,
    config$app_log_file  # default
  )
  
  # Create timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Create log line
  log_line <- paste0(timestamp, " [", toupper(level), "] ", formatted_message, "\n")
  
  # Write to file (append mode)
  tryCatch({
    cat(log_line, file = log_file, append = TRUE)
  }, error = function(e) {
    # If writing to specific log fails, at least log the error to main log
    log$warn("Failed to write to category log {log_file}: {e$message}")
  })
}

#' Enhanced logging function with structured data
#' 
#' @param level Log level (info, warn, error, debug, etc.)
#' @param message Log message
#' @param category Category of the log (e.g., "user_action", "performance", "error")
#' @param session_id Shiny session ID
#' @param user_id User identifier (if available)
#' @param additional_data List of additional structured data
#' @export
log_structured <- function(level = "info", message, category = "general", 
                          session_id = NULL, user_id = NULL, additional_data = list()) {
  
  # Create structured log entry
  log_entry <- list(
    timestamp = Sys.time(),
    level = toupper(level),
    category = category,
    message = message,
    session_id = session_id,
    user_id = user_id
  )
  
  # Add additional data
  log_entry <- c(log_entry, additional_data)
  
  # Format message safely without JSON in glue template
  session_info <- if (is.null(session_id)) "N/A" else session_id
  data_summary <- if (length(additional_data) > 0) {
    paste(names(additional_data), collapse = ", ")
  } else {
    "none"
  }
  
  formatted_message <- paste0(
    "[", category, "] ", message, 
    " | Session: ", session_info,
    " | Data keys: ", data_summary
  )
  
  # Log using appropriate level (main log)
  switch(tolower(level),
    "fatal" = log$fatal(formatted_message),
    "error" = log$error(formatted_message),
    "warn" = log$warn(formatted_message),
    "success" = log$success(formatted_message),
    "info" = log$info(formatted_message),
    "debug" = log$debug(formatted_message),
    "trace" = log$trace(formatted_message),
    log$info(formatted_message)  # default
  )
  
  # Also write to category-specific log files
  write_to_category_log(category, level, formatted_message, log_entry)
}

#' Log user activity
#' 
#' @param action Description of the user action
#' @param session_id Shiny session ID
#' @param input_id Input ID that triggered the action (if applicable)
#' @param input_value Value of the input (if applicable and safe to log)
#' @param module Module name where action occurred
#' @export
log_user_activity <- function(action, session_id = NULL, input_id = NULL, 
                             input_value = NULL, module = NULL) {
  
  additional_data <- list(
    input_id = input_id,
    input_value = if (is.null(input_value) || length(input_value) > 10) "[complex_value]" else input_value,
    module = module
  )
  
  log_structured(
    level = "info",
    message = action,
    category = "user_activity",
    session_id = session_id,
    additional_data = additional_data
  )
}

#' Log performance metrics
#' 
#' @param operation_name Name of the operation being measured
#' @param duration_ms Duration in milliseconds
#' @param session_id Shiny session ID
#' @param additional_metrics Additional performance metrics
#' @export
log_performance <- function(operation_name, duration_ms, session_id = NULL, 
                           additional_metrics = list()) {
  
  config <- get_logging_config()
  
  # Only log if performance logging is enabled and duration exceeds threshold
  if (config$log_performance && duration_ms >= config$performance_threshold_ms) {
    
    additional_data <- c(
      list(
        operation = operation_name,
        duration_ms = duration_ms,
        threshold_ms = config$performance_threshold_ms
      ),
      additional_metrics
    )
    
    level <- if (duration_ms > config$performance_threshold_ms * 2) "warn" else "info"
    
    log_structured(
      level = level,
      message = glue::glue("Performance: {operation_name} took {duration_ms}ms"),
      category = "performance",
      session_id = session_id,
      additional_data = additional_data
    )
  }
}

#' Log errors with enhanced context
#' 
#' @param error_message Error message
#' @param error_object Error object (if available)
#' @param session_id Shiny session ID
#' @param context Additional context about where/when error occurred
#' @param user_action What the user was trying to do when error occurred
#' @export
log_error_enhanced <- function(error_message, error_object = NULL, session_id = NULL,
                              context = NULL, user_action = NULL) {
  
  additional_data <- list(
    error_class = if (!is.null(error_object)) class(error_object)[1] else NULL,
    error_call = if (!is.null(error_object)) deparse(error_object$call)[1] else NULL,
    context = context,
    user_action = user_action,
    stack_trace = if (!is.null(error_object)) capture.output(traceback()) else NULL
  )
  
  log_structured(
    level = "error",
    message = error_message,
    category = "error",
    session_id = session_id,
    additional_data = additional_data
  )
}

#' Performance monitoring wrapper function
#' 
#' Wraps a function call and automatically logs performance metrics
#' 
#' @param operation_name Name for the operation being monitored
#' @param func Function to execute
#' @param session_id Shiny session ID
#' @param ... Arguments to pass to the function
#' @export
with_performance_logging <- function(operation_name, func, session_id = NULL, ...) {
  start_time <- Sys.time()
  
  result <- tryCatch({
    func(...)
  }, error = function(e) {
    duration_ms <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000
    
    log_error_enhanced(
      error_message = glue::glue("Error in {operation_name}: {e$message}"),
      error_object = e,
      session_id = session_id,
      context = operation_name
    )
    
    log_performance(
      operation_name = paste0(operation_name, "_FAILED"),
      duration_ms = duration_ms,
      session_id = session_id,
      additional_metrics = list(status = "error")
    )
    
    stop(e)
  })
  
  end_time <- Sys.time()
  duration_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000
  
  log_performance(
    operation_name = operation_name,
    duration_ms = duration_ms,
    session_id = session_id,
    additional_metrics = list(status = "success")
  )
  
  result
}

#' Log data processing events
#' 
#' @param operation Data processing operation (e.g., "file_upload", "data_validation")
#' @param data_info Information about the data being processed
#' @param session_id Shiny session ID
#' @param status Status of the operation ("started", "completed", "failed")
#' @export
log_data_processing <- function(operation, data_info = list(), session_id = NULL, status = "completed") {
  
  log_structured(
    level = if (status == "failed") "error" else "info",
    message = glue::glue("Data processing: {operation} - {status}"),
    category = "data_processing",
    session_id = session_id,
    additional_data = c(
      list(
        operation = operation,
        status = status
      ),
      data_info
    )
  )
}

#' Log model fitting events
#' 
#' @param model_type Type of model being fitted
#' @param parameters Model parameters
#' @param session_id Shiny session ID
#' @param status Status of the fitting ("started", "completed", "failed", "converged")
#' @param metrics Model fit metrics (if available)
#' @export
log_model_fitting <- function(model_type, parameters = list(), session_id = NULL, 
                             status = "completed", metrics = list()) {
  
  level <- switch(status,
    "failed" = "error",
    "started" = "debug",
    "info"
  )
  
  log_structured(
    level = level,
    message = glue::glue("Model fitting: {model_type} - {status}"),
    category = "model_fitting",
    session_id = session_id,
    additional_data = c(
      list(
        model_type = model_type,
        status = status
      ),
      parameters = list(parameters),
      metrics = list(metrics)
    )
  )
}

#' Create a session-aware logging context
#' 
#' Returns a list of logging functions that automatically include session information
#' 
#' @param session Shiny session object
#' @export
create_session_logger <- function(session) {
  session_id <- session$token
  
  list(
    info = function(message, category = "general", additional_data = list()) {
      log_structured("info", message, category, session_id, additional_data = additional_data)
    },
    warn = function(message, category = "general", additional_data = list()) {
      log_structured("warn", message, category, session_id, additional_data = additional_data)
    },
    error = function(message, category = "general", additional_data = list()) {
      log_structured("error", message, category, session_id, additional_data = additional_data)
    },
    debug = function(message, category = "general", additional_data = list()) {
      log_structured("debug", message, category, session_id, additional_data = additional_data)
    },
    user_activity = function(action, input_id = NULL, input_value = NULL, module = NULL) {
      log_user_activity(action, session_id, input_id, input_value, module)
    },
    performance = function(operation_name, duration_ms, additional_metrics = list()) {
      log_performance(operation_name, duration_ms, session_id, additional_metrics)
    },
    error_enhanced = function(error_message, error_object = NULL, context = NULL, user_action = NULL) {
      log_error_enhanced(error_message, error_object, session_id, context, user_action)
    },
    data_processing = function(operation, data_info = list(), status = "completed") {
      log_data_processing(operation, data_info, session_id, status)
    },
    model_fitting = function(model_type, parameters = list(), status = "completed", metrics = list()) {
      log_model_fitting(model_type, parameters, session_id, status, metrics)
    },
    with_performance = function(operation_name, func, ...) {
      with_performance_logging(operation_name, func, session_id, ...)
    }
  )
}
