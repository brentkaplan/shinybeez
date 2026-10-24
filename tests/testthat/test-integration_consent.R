# Browser side of the consent handshake, in a real Chrome: the banner on a first visit, the
# Google tag loaded only at the usage level, a choice persisted in localStorage and honoured
# on reload, and a blocked localStorage treated as "no choice yet".

box::use(
  testthat[...],
  DBI,
  RSQLite,
  jsonlite,
)

consent_env <- function(db_path) {
  c(
    R_CONFIG_ACTIVE = "default",
    TELEMETRY_ENABLED = "TRUE",
    TELEMETRY_STORAGE = "sqlite",
    TELEMETRY_DB_PATH = db_path,
    SHINYBEEZ_ENV = "test",
    GA_ENABLED = "TRUE",
    GA_MEASUREMENT_ID = "G-TEST"
  )
}

rows_of <- function(db_path, type = NULL) {
  if (!file.exists(db_path)) return(data.frame())
  con <- DBI$dbConnect(RSQLite$SQLite(), db_path)
  on.exit(DBI$dbDisconnect(con), add = TRUE)
  sql <- "SELECT rowid, session, type, details FROM event_log"
  if (!is.null(type)) sql <- paste0(sql, " WHERE type = '", type, "'")
  DBI$dbGetQuery(con, paste(sql, "ORDER BY rowid"))
}

wait_for_rows <- function(db_path, type, n, timeout_s = 10) {
  deadline <- Sys.time() + timeout_s
  while (Sys.time() < deadline) {
    if (nrow(rows_of(db_path, type)) >= n) return(invisible(TRUE))
    Sys.sleep(0.2)
  }
  invisible(FALSE)
}

has_ga_script <- function(app) {
  isTRUE(app$get_js("document.querySelector('script[src*=\"googletagmanager\"]') !== null"))
}

describe("consent in the browser", {
  db <- NULL
  app <- NULL
  port <- httpuv::randomPort()

  it("boots with telemetry and GA configured", {
    db <<- tempfile(fileext = ".sqlite")
    app <<- create_app_driver(env = consent_env(db), port = port)
    expect_false(is.null(app))
    withr::defer(if (!is.null(app)) try(app$stop(), silent = TRUE), envir = teardown_env())
    expect_identical(app$get_js("window.SHINYBEEZ.gaId"), "G-TEST")
  })

  it("shows the banner on a first visit and loads GA only for a usage default", {
    require_app(app)
    expect_true(isTRUE(app$get_js("document.querySelector('.consent-banner') !== null")))
    handshake <- app$get_value(input = "consent_level")
    expect_true(handshake$source %in% c("default", "default_eu"))
    expect_identical(has_ga_script(app), identical(handshake$source, "default"))
    expect_true(wait_for_rows(db, "session_start", 1))
    start <- jsonlite$fromJSON(rows_of(db, "session_start")$details[[1]])
    expect_identical(start$consent, handshake$level)
    expect_identical(start$consent_source, handshake$source)
  })

  it("clicking Nothing persists none, disables GA and stops telemetry rows", {
    require_app(app)
    nav_before <- nrow(rows_of(db, "navigation"))
    app$click("app-consent-nothing")
    app$wait_for_idle(duration = 500)
    stored <- jsonlite$fromJSON(app$get_js("localStorage.getItem('shinybeez.consent')"))
    expect_identical(stored$level, "none")
    expect_identical(stored$policy, 1L)
    expect_true(isTRUE(app$get_js("window['ga-disable-G-TEST'] === true")))
    expect_true(isTRUE(app$get_js("document.querySelector('.consent-banner') === null")))
    expect_true(wait_for_rows(db, "consent_change", 1))
    change <- jsonlite$fromJSON(rows_of(db, "consent_change")$details[[1]])
    expect_identical(change$to, "none")
    expect_identical(change$source, "banner")
    navigate_to_tab(app, "Demand")
    navigate_to_tab(app, "Welcome")
    expect_identical(nrow(rows_of(db, "navigation")), nav_before)
  })

  it("honours a stored diagnostic choice on the next visit without a banner", {
    require_app(app)
    app$run_js("localStorage.setItem('shinybeez.consent', JSON.stringify({level:'diagnostic', policy:1, ts:1}))")
    # A second visit: same Chrome, same origin (pinned port), so the same localStorage.
    app$stop()
    app <<- create_app_driver(env = consent_env(db), port = port)
    expect_true(isTRUE(app$get_js("document.querySelector('.consent-banner') === null")))
    expect_false(has_ga_script(app))
    expect_true(wait_for_rows(db, "session_start", 2))
    starts <- rows_of(db, "session_start")
    last <- jsonlite$fromJSON(starts$details[[nrow(starts)]])
    expect_identical(last$consent, "diagnostic")
    expect_identical(last$consent_source, "stored")
  })

  it("treats a throwing localStorage as no choice and shows the banner again", {
    require_app(app)
    # Block storage on the live page, then replay the connect handshake (a jQuery event, as
    # Shiny fires it): the stored diagnostic choice must be unreadable and count as none made.
    app$run_js(paste(
      "Object.defineProperty(window, 'localStorage', { get() { throw new Error('blocked'); } });",
      "$(document).trigger('shiny:connected');"
    ))
    app$wait_for_idle(duration = 500)
    handshake <- app$get_value(input = "consent_level")
    expect_true(handshake$source %in% c("default", "default_eu"))
    expect_true(isTRUE(app$get_js("document.querySelector('.consent-banner') !== null")))
  })
})
