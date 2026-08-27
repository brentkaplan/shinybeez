#' ExtendedTask wrapper for daemon-backed fits.
#'
#' `make_fit_task()` returns a small object the views use: invoke with a spec and
#' a data frame, read status/result reactively, cancel, and classify the outcome.
#' With `async = FALSE` the worker runs inline (the retained synchronous path).

box::use(
  mirai,
  shiny,
)
box::use(
  app / logic / async / daemons,
)

#' @export
classify_outcome <- function(status, handle = NULL) {
  if (!identical(status, "error")) return(status)
  value <- handle$data
  if (!is.null(value) && mirai$is_error_value(value) && is.numeric(value)) {
    code <- as.integer(value)
    if (identical(code, 20L)) return("cancelled")
    if (identical(code, 5L)) return("timeout")
  }
  "error"
}

#' @export
make_fit_task <- function(worker, timeout_ms = daemons$fit_timeout_ms(), async = daemons$async_enabled()) {
  handle <- NULL
  started_at <- shiny$reactiveVal(NULL)

  extended <- shiny$ExtendedTask$new(function(spec, data) {
    if (!async) return(worker(spec, data))
    daemons$ensure_daemons()
    handle <<- mirai$mirai(
      worker(spec, data),
      worker = worker, spec = spec, data = data,
      .timeout = timeout_ms
    )
    handle
  })

  list(
    task = extended,
    invoke = function(spec, data) {
      # One fit at a time: ExtendedTask would queue a second invocation, but the
      # views keep a single pending-metadata slot, so refuse instead of queueing.
      if (identical(extended$status(), "running")) return(invisible(FALSE))
      started_at(Sys.time())
      extended$invoke(spec, data)
      invisible(TRUE)
    },
    status = extended$status,
    result = extended$result,
    started_at = started_at,
    cancel = function() {
      if (!is.null(handle) && mirai$unresolved(handle)) mirai$stop_mirai(handle)
      invisible(NULL)
    },
    outcome = function() classify_outcome(extended$status(), handle),
    error_message = function() {
      tryCatch({
        extended$result()
        NULL
      }, error = function(e) conditionMessage(e))
    }
  )
}
