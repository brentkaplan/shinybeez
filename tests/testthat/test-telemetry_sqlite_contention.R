# Several app containers write one telemetry.sqlite. RSQLite's dbWriteTable(append) opens a
# deferred transaction, reads (dbExistsTable, dbListFields) and then inserts; in WAL mode a
# writer that committed between that read and the insert makes SQLite return
# SQLITE_BUSY_SNAPSHOT at once, without consulting busy_timeout. shiny.telemetry's login write
# and its input observer (both registered by start_session) do not catch it, so Shiny ends
# the user's session. Seen on the 2026-09-17 load test at 3+ concurrent sessions (analytics
# repo, docs/loadtest-2026-09-17.md section 1).
#
# Contract under test: a telemetry write never raises into the session; with the write lock
# free most of the time every write lands; with the lock held past busy_timeout the write is
# dropped, counted, and the connection is left usable.

box::use(
  testthat[...],
  DBI,
  RSQLite,
  callr,
  withr,
)

box::use(
  app / logic / telemetry_utils,
)

# Background R processes that append to `db_path` the way an untouched shiny.telemetry
# container does (deferred transaction), for `secs` seconds, pausing `gap_ms` between writes so
# the lock is free most of the time (a tight loop starves busy_timeout instead). Each process
# touches `<db_path>.ready<i>` after its first commit; wait on those before driving sessions.
# Returns the process handles; each resolves to the number of commits it made.
start_contending_writers <- function(db_path, n = 3, secs = 4, gap_ms = 5) {
  lapply(seq_len(n), function(i) {
    callr::r_bg(
      function(db_path, secs, gap_ms, ready_file) {
        con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
        on.exit(DBI::dbDisconnect(con))
        DBI::dbExecute(con, "PRAGMA busy_timeout = 2000")
        row <- data.frame(
          time = Sys.time(), app_name = "contender", session = "bg",
          type = "input", details = "{}"
        )
        deadline <- Sys.time() + secs
        n_ok <- 0L
        while (Sys.time() < deadline) {
          ok <- tryCatch(
            {
              DBI::dbWriteTable(con, "event_log", row, append = TRUE, row.names = FALSE)
              TRUE
            },
            error = function(e) FALSE
          )
          if (ok) {
            n_ok <- n_ok + 1L
            if (n_ok == 1L) file.create(ready_file)
          }
          Sys.sleep(gap_ms / 1000)
        }
        n_ok
      },
      args = list(
        db_path = db_path, secs = secs, gap_ms = gap_ms,
        ready_file = paste0(db_path, ".ready", i)
      ),
      package = FALSE
    )
  })
}

# One background process that holds the SQLite write lock for `secs` seconds. Touches
# `<db_path>.held` once the lock is taken.
hold_write_lock <- function(db_path, secs = 4) {
  callr::r_bg(
    function(db_path, secs, held_file) {
      con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
      on.exit(DBI::dbDisconnect(con))
      DBI::dbExecute(con, "BEGIN IMMEDIATE")
      file.create(held_file)
      Sys.sleep(secs)
      DBI::dbExecute(con, "COMMIT")
      TRUE
    },
    args = list(db_path = db_path, secs = secs, held_file = paste0(db_path, ".held")),
    package = FALSE
  )
}

wait_for_files <- function(paths, timeout_s = 10) {
  deadline <- Sys.time() + timeout_s
  while (!all(file.exists(paths))) {
    if (Sys.time() > deadline) stop("timed out waiting for ", paste(paths, collapse = ", "))
    Sys.sleep(0.05)
  }
  invisible(TRUE)
}

count_rows <- function(db_path, where) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, paste("SELECT COUNT(*) AS n FROM event_log WHERE", where))$n
}

describe("telemetry SQLite storage under concurrent writers", {
  it("never raises into the session and loses no write while other processes commit", {
    db_path <- with_sqlite_telemetry()
    telemetry <- telemetry_utils$get_telemetry()
    expect_false(is.null(telemetry))

    writers <- start_contending_writers(db_path)
    withr::defer(for (w in writers) if (w$is_alive()) w$kill())
    wait_for_files(paste0(db_path, ".ready", seq_along(writers)))

    sessions <- lapply(1:4, function(i) fake_session())
    # start_session writes a login row and registers the package's input observer.
    # Each setInputs() flushes the reactive graph, which runs that observer and writes one
    # `input` row per changed input. With MockShinySession an observer error is raised
    # here rather than silently ending the session.
    expect_no_error({
      for (s in sessions) {
        # MockShinySession warns that session$request is not realistic; irrelevant here.
        suppressWarnings(telemetry$start_session(session = s, track_values = FALSE))
      }
      for (k in 1:60) {
        for (s in sessions) {
          s$setInputs(probe = k)
        }
      }
    })
    for (s in sessions) expect_false(s$isClosed())
    for (w in writers) {
      w$wait()
      # The file really was contended: each contender committed at least once. (Not a
      # rate: a CI runner's disk managed only ~14 commits per writer in 4 s.)
      expect_gte(w$get_result(), 1L)
    }

    expect_equal(telemetry$data_storage$dropped_writes(), 0L)
    expect_equal(count_rows(db_path, "type = 'login' AND app_name = 'develop'"), 4L)
    expect_equal(count_rows(db_path, "type = 'input' AND app_name = 'develop'"), 4L * 60L)
  })

  it("drops and counts a write when the lock is held past busy_timeout, then recovers", {
    db_path <- with_sqlite_telemetry()
    telemetry <- telemetry_utils$get_telemetry()
    sess <- fake_session()

    holder <- hold_write_lock(db_path, secs = 4)
    withr::defer(if (holder$is_alive()) holder$kill())
    wait_for_files(paste0(db_path, ".held"))

    t0 <- Sys.time()
    expect_no_error(telemetry$log_custom_event("probe", list(k = 1L), session = sess))
    waited <- as.numeric(Sys.time() - t0, units = "secs")
    expect_gte(waited, 1.5) # it did wait for busy_timeout
    expect_lt(waited, 3.5) # and did not block the session for the whole hold
    expect_equal(telemetry$data_storage$dropped_writes(), 1L)

    holder$wait()
    expect_true(holder$get_result())
    # The failed write left no open transaction behind: the next write lands.
    expect_no_error(telemetry$log_custom_event("probe", list(k = 2L), session = sess))
    expect_equal(telemetry$data_storage$dropped_writes(), 1L)
    expect_equal(count_rows(db_path, "type = 'probe'"), 1L)
  })

  it("keeps busy_timeout and WAL on the connection shiny.telemetry writes with", {
    db_path <- with_sqlite_telemetry()
    telemetry <- telemetry_utils$get_telemetry()
    con <- telemetry$data_storage$.__enclos_env__$private$db_con
    expect_true(DBI::dbIsValid(con))
    expect_equal(tolower(DBI::dbGetQuery(con, "PRAGMA journal_mode")[[1]]), "wal")
    expect_equal(DBI::dbGetQuery(con, "PRAGMA busy_timeout")[[1]], 2000L)
  })
})
