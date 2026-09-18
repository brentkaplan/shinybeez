box::use(
  testthat[...],
  withr[with_envvar],
  mirai,
)
box::use(
  app / logic / async / daemons,
)

describe("daemon_count", {
  it("defaults to 1 when the env var is unset", {
    with_envvar(c(SHINYBEEZ_DAEMONS = NA), expect_identical(daemons$daemon_count(), 1L))
  })
  it("reads a positive integer from SHINYBEEZ_DAEMONS", {
    with_envvar(c(SHINYBEEZ_DAEMONS = "3"), expect_identical(daemons$daemon_count(), 3L))
  })
  it("treats 0 as the synchronous path", {
    with_envvar(c(SHINYBEEZ_DAEMONS = "0"), {
      expect_identical(daemons$daemon_count(), 0L)
      expect_false(daemons$async_enabled())
    })
  })
  it("falls back to 1 on garbage or negative values", {
    with_envvar(c(SHINYBEEZ_DAEMONS = "many"), expect_identical(daemons$daemon_count(), 1L))
    with_envvar(c(SHINYBEEZ_DAEMONS = "-2"), expect_identical(daemons$daemon_count(), 1L))
  })
})

describe("fit_timeout_ms / rss_limit_mb", {
  it("default to 600000 ms and 900 MB", {
    with_envvar(c(SHINYBEEZ_FIT_TIMEOUT_MS = NA, SHINYBEEZ_DAEMON_RSS_LIMIT_MB = NA), {
      expect_equal(daemons$fit_timeout_ms(), 600000)
      expect_equal(daemons$rss_limit_mb(), 900)
    })
  })
  it("read overrides", {
    with_envvar(c(SHINYBEEZ_FIT_TIMEOUT_MS = "1500", SHINYBEEZ_DAEMON_RSS_LIMIT_MB = "1200"), {
      expect_equal(daemons$fit_timeout_ms(), 1500)
      expect_equal(daemons$rss_limit_mb(), 1200)
    })
  })
})

describe("start_daemons / stop_daemons", {
  it("is a no-op returning 0 when n = 0", {
    expect_identical(daemons$start_daemons(0L), 0L)
  })
  it("starts one daemon with both packages loaded and box.path set", {
    daemons$start_daemons(1L)
    on.exit(daemons$stop_daemons(), add = TRUE)
    expect_true(mirai$mirai("beezdemand" %in% loadedNamespaces())[])
    expect_true(mirai$mirai("beezdiscounting" %in% loadedNamespaces())[])
    expect_identical(mirai$mirai(getOption("box.path"))[], getOption("box.path"))
    expect_true(daemons$pool_idle())
  })
})

describe("recycle_daemons / ensure_daemons", {
  it("restarts the pool when idle, yielding a new worker process", {
    daemons$start_daemons(1L)
    on.exit(daemons$stop_daemons(), add = TRUE)
    pid_before <- mirai$mirai(Sys.getpid())[]
    expect_true(daemons$recycle_daemons())
    pid_after <- mirai$mirai(Sys.getpid())[]
    expect_false(identical(pid_before, pid_after))
  })
  it("ensure_daemons starts the pool only when none is connected", {
    daemons$stop_daemons()
    on.exit(daemons$stop_daemons(), add = TRUE)
    expect_true(daemons$ensure_daemons())
    expect_false(daemons$ensure_daemons())
  })
  it("refuses to recycle while a task is executing or awaiting", {
    daemons$start_daemons(1L)
    on.exit(daemons$stop_daemons(), add = TRUE)
    m <- mirai$mirai(Sys.sleep(2))
    Sys.sleep(0.2)
    expect_false(daemons$recycle_daemons())
    expect_true(mirai$unresolved(m))
    m[]
    expect_true(daemons$recycle_daemons())
  })
})
