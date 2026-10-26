# The consent registry is the per-process table the storage gate consults: session token ->
# consent level. Rows that arrive before the level is known (the first navigation row fires
# before the browser has sent its stored choice) wait in a short pending buffer instead of
# being dropped; rows for a session that has ended are dropped as closed rather than
# buffered forever.

box::use(
  testthat[...],
)

box::use(
  app / logic / consent_registry,
)

values_for <- function(token, type = "navigation") {
  list(time = Sys.time(), app_name = "test", session = token, type = type, details = "{}")
}

describe("decide()", {
  it("drops a row with no token", {
    consent_registry$reset()
    expect_identical(consent_registry$decide(NULL, "error"), "drop_no_token")
    expect_identical(consent_registry$decide("", "error"), "drop_no_token")
  })

  it("buffers a row whose token has no level yet", {
    consent_registry$reset()
    expect_identical(consent_registry$decide("tok-1", "navigation"), "buffer")
  })

  it("writes a diagnostic row at diagnostic", {
    consent_registry$reset()
    consent_registry$set_level("tok-1", "diagnostic")
    expect_identical(consent_registry$decide("tok-1", "error"), "write")
  })

  it("drops a usage row at diagnostic for consent", {
    consent_registry$reset()
    consent_registry$set_level("tok-1", "diagnostic")
    expect_identical(consent_registry$decide("tok-1", "navigation"), "drop_consent")
  })

  it("writes consent_change even at none", {
    consent_registry$reset()
    consent_registry$set_level("tok-1", "none")
    expect_identical(consent_registry$decide("tok-1", "consent_change"), "write")
    expect_identical(consent_registry$decide("tok-1", "error"), "drop_consent")
  })
})

describe("set_level() and get_level()", {
  it("returns the previous level, NULL the first time", {
    consent_registry$reset()
    expect_null(consent_registry$set_level("tok-1", "usage"))
    expect_identical(consent_registry$set_level("tok-1", "none"), "usage")
    expect_identical(consent_registry$get_level("tok-1"), "none")
    expect_null(consent_registry$get_level("tok-2"))
  })
})

describe("buffer() and drain()", {
  it("keeps rows in insertion order and empties on drain", {
    consent_registry$reset()
    expect_true(consent_registry$buffer("tok-1", values_for("tok-1", "navigation")))
    expect_true(consent_registry$buffer("tok-1", values_for("tok-1", "error")))
    rows <- consent_registry$drain("tok-1")
    expect_length(rows, 2L)
    expect_identical(vapply(rows, function(r) r$type, character(1)), c("navigation", "error"))
    expect_length(consent_registry$drain("tok-1"), 0L)
  })

  it("refuses the 51st row for one token", {
    consent_registry$reset()
    for (i in seq_len(consent_registry$PENDING_CAP)) {
      expect_true(consent_registry$buffer("tok-1", values_for("tok-1")))
    }
    expect_false(consent_registry$buffer("tok-1", values_for("tok-1")))
    expect_length(consent_registry$drain("tok-1"), consent_registry$PENDING_CAP)
  })
})

describe("expire()", {
  it("removes buffered rows older than the limit and reports the count", {
    consent_registry$reset()
    t0 <- as.POSIXct("2026-10-24 10:00:00", tz = "UTC")
    consent_registry$buffer("tok-1", values_for("tok-1"), now = t0)
    consent_registry$buffer("tok-1", values_for("tok-1"), now = t0)
    consent_registry$buffer("tok-2", values_for("tok-2"), now = t0 + 5)
    expect_identical(consent_registry$expire(now = t0 + 11), 2L)
    expect_length(consent_registry$drain("tok-1"), 0L)
    expect_length(consent_registry$drain("tok-2"), 1L)
  })

  it("leaves a fresh buffer alone", {
    consent_registry$reset()
    t0 <- Sys.time()
    consent_registry$buffer("tok-1", values_for("tok-1"), now = t0)
    expect_identical(consent_registry$expire(now = t0 + 1), 0L)
  })
})

describe("end_session()", {
  it("drops later rows as closed, then forgets the token after the tombstone expires", {
    consent_registry$reset()
    t0 <- as.POSIXct("2026-10-24 10:00:00", tz = "UTC")
    consent_registry$set_level("tok-1", "usage")
    consent_registry$buffer("tok-1", values_for("tok-1"), now = t0)
    consent_registry$end_session("tok-1", now = t0)
    expect_null(consent_registry$get_level("tok-1"))
    expect_length(consent_registry$drain("tok-1"), 0L)
    expect_identical(consent_registry$decide("tok-1", "logout", now = t0 + 1), "drop_closed")
    consent_registry$expire(now = t0 + 61)
    expect_identical(consent_registry$decide("tok-1", "logout", now = t0 + 61), "buffer")
  })
})
