# Connect Cloud log, 2026-10-19 08:03:56: "Warning: call dbDisconnect() when finished
# working with a connection". main$ui() called init_telemetry() on every page load, and
# each call replaced the cached Telemetry object without closing its SQLite connection;
# the orphan surfaced as that warning when R garbage-collected it.

box::use(
  testthat[...],
)

box::use(
  app / logic / telemetry_utils,
  app / main,
)

sqlite_env <- function(db_path) {
  c(
    R_CONFIG_ACTIVE = "default",
    TELEMETRY_ENABLED = "TRUE",
    TELEMETRY_STORAGE = "sqlite",
    TELEMETRY_DB_PATH = db_path,
    SHINYBEEZ_ENV = "test"
  )
}

storage_con <- function(tel) {
  tel$data_storage$.__enclos_env__$private$db_con
}

reset_telemetry <- function(env = parent.frame()) {
  withr::defer(
    withr::with_envvar(
      c(R_CONFIG_ACTIVE = "default", TELEMETRY_ENABLED = "FALSE"),
      telemetry_utils$init_telemetry()
    ),
    envir = env
  )
}

describe("telemetry connection reuse", {
  it("closes the previous storage connection when re-initialised", {
    skip_if_not_installed("shiny.telemetry")
    skip_if_not_installed("RSQLite")
    reset_telemetry()
    withr::local_envvar(sqlite_env(tempfile(fileext = ".sqlite")))

    first <- telemetry_utils$init_telemetry()
    old_con <- storage_con(first)
    expect_true(DBI::dbIsValid(old_con))

    telemetry_utils$init_telemetry()
    expect_false(DBI::dbIsValid(old_con))
  })

  it("keeps one Telemetry object across page loads", {
    skip_if_not_installed("shiny.telemetry")
    skip_if_not_installed("RSQLite")
    reset_telemetry()
    withr::local_envvar(sqlite_env(tempfile(fileext = ".sqlite")))
    # ui() reads its markdown and assets relative to the app root.
    withr::local_dir(test_path("..", ".."))

    main$ui("app")
    first <- telemetry_utils$get_telemetry()
    main$ui("app")
    expect_identical(telemetry_utils$get_telemetry(), first)
    expect_true(DBI::dbIsValid(storage_con(first)))
  })
})
