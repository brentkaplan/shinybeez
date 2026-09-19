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
  SHINYBEEZ_DAEMONS = "0"
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
    env <- c(complete_env, R_CONFIG_ACTIVE = "connectcloud", GA_MEASUREMENT_ID = "", PATH = "/usr/bin")
    expect_identical(deploy$forwarded_env_names(env), c("SHINYBEEZ_DAEMONS", "R_CONFIG_ACTIVE"))
  })

  it("never forwards a deploy credential or the target", {
    expect_length(intersect(deploy$app_env_names, names(complete_env)[1:4]), 0L)
    expect_false(any(grepl("^CONNECT_CLOUD_", deploy$app_env_names)))
  })
})
