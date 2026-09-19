# .github/deploy-connect-cloud.R takes its target and the hosted app's variables from
# the environment, so the (public) repo names no account or content id. These tests
# cover the two decisions it makes before touching the network: refuse to deploy with
# anything required missing, and forward only allow-listed app variables.
deploy <- new.env()
sys.source(
  testthat::test_path("..", "..", ".github", "deploy-connect-cloud.R"),
  envir = deploy
)

complete_env <- c(
  CONNECT_CLOUD_CLIENT_ID = "id",
  CONNECT_CLOUD_CLIENT_SECRET = "secret",
  CONNECT_CLOUD_ACCOUNT = "account",
  CONNECT_CLOUD_CONTENT_ID = "content",
  SHINYBEEZ_DAEMONS = "0",
  R_CONFIG_ACTIVE = "connectcloud"
)

describe("missing_required()", {
  it("is empty when everything required is set", {
    expect_identical(deploy$missing_required(complete_env), character(0))
  })

  it("names each required variable that is unset or empty", {
    for (name in names(complete_env)) {
      expect_identical(deploy$missing_required(complete_env[names(complete_env) != name]), name)
      emptied <- complete_env
      emptied[[name]] <- ""
      expect_identical(deploy$missing_required(emptied), name)
    }
  })
})

describe("forwarded_env_names()", {
  it("forwards only allow-listed app variables that are set", {
    env <- c(complete_env, GA_MEASUREMENT_ID = "", PATH = "/usr/bin")
    expect_identical(deploy$forwarded_env_names(env), c("SHINYBEEZ_DAEMONS", "R_CONFIG_ACTIVE"))
  })

  it("never forwards a deploy credential or the target", {
    expect_length(intersect(deploy$app_env_names, names(complete_env)[1:4]), 0L)
    expect_false(any(grepl("^CONNECT_CLOUD_", deploy$app_env_names)))
  })
})

# Without R_CONFIG_ACTIVE the hosted app would run the default profile: autoreload on, file
# logging. And the postgresql backend is not deployable yet: its TELEMETRY_DB_* variables are
# deliberately not forwarded, so selecting it would point the app at localhost.
describe("invalid_settings()", {
  it("accepts the hosted profiles and the sqlite backend", {
    expect_identical(deploy$invalid_settings(complete_env), character(0))
    expect_identical(
      deploy$invalid_settings(c(R_CONFIG_ACTIVE = "shinyapps", TELEMETRY_STORAGE = "sqlite")),
      character(0)
    )
  })

  it("rejects a profile that is not a hosted one", {
    for (profile in c("default", "development", "production", "shinyproxy", "Connectcloud")) {
      problems <- deploy$invalid_settings(c(R_CONFIG_ACTIVE = profile))
      expect_length(problems, 1L)
      expect_match(problems, "R_CONFIG_ACTIVE", fixed = TRUE)
    }
  })

  it("rejects any telemetry backend other than sqlite, without echoing the value", {
    problems <- deploy$invalid_settings(
      c(R_CONFIG_ACTIVE = "connectcloud", TELEMETRY_STORAGE = "postgresql://u:hunter2@h/db")
    )
    expect_length(problems, 1L)
    expect_match(problems, "TELEMETRY_STORAGE", fixed = TRUE)
    expect_no_match(problems, "hunter2", fixed = TRUE)
  })
})

# rsconnect 1.11.0's Connect Cloud client has getContent() but no getApplication(), so
# deployApp(appId = ) fails before it uploads anything (rstudio/rsconnect#1367, fixed
# upstream and unreleased). The script adds the upstream method until a release has it.
describe("with_get_application()", {
  fake_client <- function() {
    list(getContent = function(id) list(id = id, title = "My App Title"))
  }

  it("adds a getApplication() that looks the content up by id and never renames it", {
    expect_false("rsconnect" %in% .packages())
    client <- deploy$with_get_application(fake_client())
    application <- client$getApplication("abc", "unknown")
    expect_identical(application$id, "abc")
    expect_identical(application$title, "My App Title")
    expect_identical(application$name, rsconnect::generateAppName("My App Title", unique = FALSE))
  })

  it("leaves a client that already has getApplication() alone", {
    theirs <- function(...) "theirs"
    client <- deploy$with_get_application(c(fake_client(), list(getApplication = theirs)))
    expect_identical(client$getApplication, theirs)
  })
})

describe("patch_cloud_client()", {
  it("makes rsconnect's own Connect Cloud client answer getApplication()", {
    ns <- asNamespace("rsconnect")
    original <- get("connectCloudClient", envir = ns)
    withr::defer(utils::assignInNamespace("connectCloudClient", original, ns = "rsconnect"))

    deploy$patch_cloud_client()
    client <- get("connectCloudClient", envir = ns)(list(), list())
    expect_true(is.function(client$getApplication))
    expect_true(is.function(client$getContent))
  })
})
