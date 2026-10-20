#' Aesthetic Render Gate
#'
#' Holds a plot render until the browser has applied the aesthetic selections the server just
#' pushed (colour, linetype, facet, shape).
#'
#' Why it exists: on a refit the defaults observer calls `updateSelectInput()`, but the new
#' values only reach `input$...` after a browser round trip. A plot that renders in the same
#' flush reads the old inputs and draws with the old mapping, and a `bindEvent`'d plot reactive
#' is not invalidated again when the echo arrives.
#'
#' How it is used: call `push()` from the observer that sends the updates (priority 10, before
#' the `updateSelectInput()` calls, so the gate is shut when the render runs in that flush) and
#' put `shiny$req(gate$open())` in the render expression. Keep the gate OUTSIDE any
#' `bindCache`'d reactive: `bindCache` caches `req()` errors, which would poison the cache key
#' built from the stale inputs.
#'
#' The gate opens when the echo matches the pushed selections, or when `timeout_ms` has passed
#' (a selection the browser never echoes must not leave the plot blank). An Update Plot click
#' deliberately does NOT open it: the click invalidates the `bindEvent`'d plot reactive, and
#' opening the gate early would evaluate it with the stale inputs and cache that result, since
#' the late echo cannot invalidate it. Left shut, the render evaluates the still-invalidated
#' plot once the echo has synced the inputs.

box::use(
  shiny,
)

box::use(
  app / logic / mixed_effects / plotting,
)

#' Create a gate. Call from a module server function (it creates observers).
#'
#' @param read_current Function returning `list(color, linetype, facet, shape)`, the live
#'   selections. Called reactively by the echo observer and isolated by `push()`.
#' @param timeout_ms How long the gate may stay shut waiting for the echo.
#' @return `list(push, open, pending)`: `push(expected)` shuts the gate unless `expected`
#'   already matches the live selections; `open()` is a reactive TRUE/FALSE; `pending()` is a
#'   reactive giving the awaited selections, or NULL when the gate is open.
#' @export
new <- function(read_current, timeout_ms = 3000) {
  # NULL when open, otherwise list(id, expected).
  state <- shiny$reactiveVal(NULL)
  pushes <- 0L
  armed_id <- 0L

  push <- function(expected) {
    if (plotting$aesthetics_in_sync(expected, shiny$isolate(read_current()))) {
      state(NULL)
    } else {
      pushes <<- pushes + 1L
      state(list(id = pushes, expected = expected))
    }
    invisible(NULL)
  }

  # Echo: open as soon as the live selections match what was pushed.
  shiny$observe({
    st <- state()
    shiny$req(st)
    if (plotting$aesthetics_in_sync(st$expected, read_current())) state(NULL)
  })

  # Timeout: depends on the gate state only. The first run for a push arms the timer; the run
  # the timer causes for the same push opens the gate.
  shiny$observe({
    st <- state()
    shiny$req(st)
    if (identical(armed_id, st$id)) {
      state(NULL)
    } else {
      armed_id <<- st$id
      shiny$invalidateLater(timeout_ms)
    }
  })

  list(
    push = push,
    open = shiny$reactive(is.null(state())),
    pending = shiny$reactive(if (is.null(state())) NULL else state()$expected)
  )
}
