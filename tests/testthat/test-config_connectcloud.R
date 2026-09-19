# The connectcloud profile is the shinyapps profile for Posit Connect Cloud: same GA
# property, console-only logging, ephemeral SQLite telemetry. The one difference is
# that telemetry storage_type comes from TELEMETRY_STORAGE (shinyapps hardcodes it),
# so moving Connect Cloud to a durable backend later is a platform variable, not a
# code change.

config_file <- file.path(project_root, "config.yml")

get_profile <- function(profile, value = NULL, env = character()) {
  withr::with_envvar(env, config::get(value = value, config = profile, file = config_file))
}

describe("connectcloud config profile", {
  it("matches the shinyapps profile when TELEMETRY_STORAGE is unset", {
    env <- c(TELEMETRY_STORAGE = NA, TELEMETRY_ENABLED = NA)
    expect_equal(get_profile("connectcloud", env = env), get_profile("shinyapps", env = env), check.attributes = FALSE)
  })

  it("reads the telemetry backend from TELEMETRY_STORAGE", {
    env <- c(TELEMETRY_STORAGE = "postgresql")
    expect_identical(get_profile("connectcloud", "telemetry", env)$storage_type, "postgresql")
    expect_identical(get_profile("shinyapps", "telemetry", env)$storage_type, "sqlite")
  })

  it("keeps the SQLite file in the app directory, not under data/", {
    # data/ is excluded from the bundle (.rscignore).
    expect_identical(get_profile("connectcloud", "telemetry")$sqlite$db_path, "telemetry.sqlite")
  })
})
