# The consent module is the only place a session's consent level is decided: it receives the
# browser's handshake (stored choice or platform default), shows the banner when there was no
# stored choice, applies the user's buttons, and tells the browser what to persist.

box::use(
  testthat[...],
  shiny,
)

box::use(
  app / view / consent,
)

# A stand-in for create_session_telemetry()'s tracker that records set_consent() calls.
fake_tracker <- function() {
  env <- new.env(parent = emptyenv())
  env$calls <- list()
  env$level <- "none"
  list(
    set_consent = function(level, source) {
      env$calls[[length(env$calls) + 1L]] <- list(level = level, source = source)
      env$level <- level
      invisible(NULL)
    },
    consent = function() env$level,
    calls = function() env$calls
  )
}

# Records what would be sent to the browser instead of session$sendCustomMessage(), which the
# mock session does not implement.
fake_client <- function() {
  env <- new.env(parent = emptyenv())
  env$sent <- list()
  list(
    notify = function(level, policy) env$sent[[length(env$sent) + 1L]] <- list(level = level, policy = policy),
    sent = function() env$sent
  )
}

handshake <- function(session, level, source, ts = as.numeric(Sys.time()) * 1000) {
  session$rootScope()$setInputs(
    consent_level = list(level = level, source = source, policy = consent$CURRENT_POLICY, ts = ts)
  )
}

describe("consent$server handshake", {
  it("applies a stored choice and shows no banner", {
    tracker <- fake_tracker()
    shiny$testServer(consent$server, args = list(session_telemetry = tracker, client = fake_client()), {
      handshake(session, "diagnostic", "stored")
      expect_identical(tracker$calls()[[1]], list(level = "diagnostic", source = "stored"))
      expect_false(show_banner())
    })
  })

  it("applies the platform default and shows the banner with OK for a usage default", {
    tracker <- fake_tracker()
    shiny$testServer(consent$server, args = list(session_telemetry = tracker, client = fake_client()), {
      handshake(session, "usage", "default")
      expect_identical(tracker$calls()[[1]], list(level = "usage", source = "default"))
      expect_true(show_banner())
      html <- as.character(banner_tag())
      expect_match(html, ">OK<", fixed = TRUE)
      expect_match(html, "Diagnostics only", fixed = TRUE)
      expect_match(html, "Nothing", fixed = TRUE)
      expect_match(html, "Details", fixed = TRUE)
      expect_match(html, "maintained by one researcher", fixed = TRUE)
      expect_match(html, "decide what I build next", fixed = TRUE)
      expect_match(html, "column names and file names are never sent", fixed = TRUE)
      expect_match(html, 'data-consent-level="none"', fixed = TRUE)
    })
  })

  it("offers Allow usage data when the default is diagnostic", {
    tracker <- fake_tracker()
    shiny$testServer(consent$server, args = list(session_telemetry = tracker, client = fake_client()), {
      handshake(session, "diagnostic", "default_eu")
      expect_identical(tracker$calls()[[1]]$source, "default_eu")
      expect_match(as.character(banner_tag()), "Allow usage data", fixed = TRUE)
    })
  })

  it("adds the desktop sentence on the desktop platform", {
    tracker <- fake_tracker()
    shiny$testServer(
      consent$server,
      args = list(session_telemetry = tracker, client = fake_client(), platform = "desktop"),
      {
        handshake(session, "usage", "default")
        expect_match(as.character(banner_tag()), "never leave your computer", fixed = TRUE)
      }
    )
  })
})

describe("consent$server choices", {
  it("Nothing applies none from the banner, hides it and tells the browser", {
    tracker <- fake_tracker()
    client <- fake_client()
    shiny$testServer(consent$server, args = list(session_telemetry = tracker, client = client), {
      handshake(session, "usage", "default")
      session$setInputs(nothing = 1)
      calls <- tracker$calls()
      expect_identical(calls[[length(calls)]], list(level = "none", source = "banner"))
      expect_false(show_banner())
      expect_identical(client$sent()[[1]], list(level = "none", policy = consent$CURRENT_POLICY))
    })
  })

  it("Diagnostics only and the accept button apply their levels", {
    tracker <- fake_tracker()
    shiny$testServer(consent$server, args = list(session_telemetry = tracker, client = fake_client()), {
      handshake(session, "diagnostic", "default_eu")
      session$setInputs(accept = 1)
      expect_identical(tracker$consent(), "usage")
      session$setInputs(diagnostic = 1)
      expect_identical(tracker$consent(), "diagnostic")
    })
  })

  it("ignores a stale stored handshake after a choice in this session, applies a fresh one", {
    tracker <- fake_tracker()
    shiny$testServer(consent$server, args = list(session_telemetry = tracker, client = fake_client()), {
      handshake(session, "usage", "default")
      session$setInputs(nothing = 1)
      n <- length(tracker$calls())
      handshake(session, "usage", "stored", ts = 1)
      expect_length(tracker$calls(), n)
      handshake(session, "usage", "stored", ts = as.numeric(Sys.time()) * 1000 + 60000)
      expect_length(tracker$calls(), n + 1L)
      expect_identical(tracker$consent(), "usage")
    })
  })

  it("Save in the settings modal applies the chosen level with source settings", {
    tracker <- fake_tracker()
    client <- fake_client()
    shiny$testServer(
      consent$server,
      args = list(session_telemetry = tracker, client = client, remove_modal = function() NULL),
      {
        handshake(session, "usage", "stored")
        session$setInputs(level = "diagnostic", save = 1)
        calls <- tracker$calls()
        expect_identical(calls[[length(calls)]], list(level = "diagnostic", source = "settings"))
        expect_identical(client$sent()[[1]]$level, "diagnostic")
      }
    )
  })

  it("opens the settings modal from the nav shield, the banner link and the Welcome button", {
    tracker <- fake_tracker()
    client <- fake_client()
    opened <- 0L
    shiny$testServer(
      consent$server,
      args = list(
        session_telemetry = tracker, client = client,
        show_modal = function(ui) opened <<- opened + 1L
      ),
      {
        handshake(session, "usage", "stored")
        session$setInputs(open = 1)
        session$setInputs(details = 1)
        session$rootScope()$setInputs(consent_open = 123)
        expect_identical(opened, 3L)
      }
    )
  })
})

describe("consent UI", {
  it("renders a shield nav item with an accessible label", {
    html <- as.character(consent$nav_ui("consent"))
    expect_match(html, "shield", fixed = TRUE)
    expect_match(html, "Privacy settings", fixed = TRUE)
    expect_match(html, 'id="consent-open"', fixed = TRUE)
  })

  it("renders the banner output slot", {
    expect_match(as.character(consent$banner_ui("consent")), 'id="consent-banner"', fixed = TRUE)
  })

  it("exports the policy version as an integer", {
    expect_identical(consent$CURRENT_POLICY, 2L)
  })
})

describe("consent$server policy and settings persistence", {
  it("treats a stored choice from an older policy as a default and re-shows the banner", {
    tracker <- fake_tracker()
    shiny$testServer(consent$server, args = list(session_telemetry = tracker, client = fake_client()), {
      session$rootScope()$setInputs(
        consent_level = list(level = "usage", source = "stored", policy = consent$CURRENT_POLICY - 1L, ts = 5)
      )
      expect_identical(tracker$calls()[[1]], list(level = "usage", source = "default"))
      expect_true(show_banner())
    })
  })

  it("marks the Save button with the radio it reads, so the browser can persist at click time", {
    tracker <- fake_tracker()
    captured <- NULL
    shiny$testServer(
      consent$server,
      args = list(
        session_telemetry = tracker, client = fake_client(),
        show_modal = function(ui) captured <<- as.character(ui)
      ),
      {
        handshake(session, "usage", "stored")
        session$setInputs(open = 1)
        # testServer namespaces the module ids itself; only the suffixes are stable.
        expect_match(captured, 'data-consent-radio="[^"]*level"')
        expect_match(captured, 'id="[^"]*save"')
      }
    )
  })

  it("prints the policy version as one sentence", {
    tracker <- fake_tracker()
    captured <- NULL
    shiny$testServer(
      consent$server,
      args = list(
        session_telemetry = tracker, client = fake_client(),
        show_modal = function(ui) captured <<- as.character(ui)
      ),
      {
        handshake(session, "usage", "stored")
        session$setInputs(open = 1)
        expect_match(captured, "Policy version 2\\. ", fixed = FALSE)
        expect_no_match(captured, "Policy version 2 \\.")
      }
    )
  })

  it("explains why the data is collected before the choices", {
    tracker <- fake_tracker()
    captured <- NULL
    shiny$testServer(
      consent$server,
      args = list(
        session_telemetry = tracker, client = fake_client(),
        show_modal = function(ui) captured <<- as.character(ui)
      ),
      {
        handshake(session, "usage", "stored")
        session$setInputs(open = 1)
        expect_match(captured, "decide what I build next", fixed = TRUE)
        expect_lt(
          regexpr("decide what I build next", captured, fixed = TRUE),
          regexpr("Usage and diagnostics (recommended)", captured, fixed = TRUE)
        )
      }
    )
  })

  it("lists usage first as recommended, and sells diagnostics as cookie-free", {
    tracker <- fake_tracker()
    captured <- NULL
    shiny$testServer(
      consent$server,
      args = list(
        session_telemetry = tracker, client = fake_client(),
        show_modal = function(ui) captured <<- as.character(ui)
      ),
      {
        handshake(session, "usage", "stored")
        session$setInputs(open = 1)
        usage_at <- regexpr("Usage and diagnostics (recommended)", captured, fixed = TRUE)
        diagnostic_at <- regexpr("Diagnostics only:", captured, fixed = TRUE)
        none_at <- regexpr("Nothing:", captured, fixed = TRUE)
        expect_true(usage_at > 0 && usage_at < diagnostic_at && diagnostic_at < none_at)
        expect_match(captured, "advertising features off", fixed = TRUE)
        expect_match(captured, "No Google Analytics, no cookies", fixed = TRUE)
        expect_match(captured, "Nothing: nothing leaves this session", fixed = TRUE)
      }
    )
  })
})
