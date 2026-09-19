# The connectcloud profile is the shinyapps profile for Posit Connect Cloud: console-only
# logging and ephemeral SQLite telemetry. It differs in two places, both so that the
# platform's variables decide rather than this (public) file: Google Analytics comes from
# GA_ENABLED / GA_MEASUREMENT_ID, and telemetry storage_type from TELEMETRY_STORAGE.

config_file <- file.path(project_root, "config.yml")

get_profile <- function(profile, value = NULL, env = character()) {
  withr::with_envvar(env, config::get(value = value, config = profile, file = config_file))
}

describe("connectcloud config profile", {
  it("matches the shinyapps profile apart from Google Analytics", {
    env <- c(TELEMETRY_STORAGE = NA, TELEMETRY_ENABLED = NA)
    connectcloud <- get_profile("connectcloud", env = env)
    shinyapps <- get_profile("shinyapps", env = env)
    connectcloud$google_analytics <- NULL
    shinyapps$google_analytics <- NULL
    expect_equal(connectcloud, shinyapps, check.attributes = FALSE)
  })

  it("takes Google Analytics from the environment and carries no id of its own", {
    off <- get_profile("connectcloud", "google_analytics", c(GA_ENABLED = NA, GA_MEASUREMENT_ID = NA))
    expect_false(off$enabled)
    expect_identical(off$measurement_id, "")

    on <- get_profile("connectcloud", "google_analytics", c(GA_ENABLED = "TRUE", GA_MEASUREMENT_ID = "G-TEST"))
    expect_true(on$enabled)
    expect_identical(on$measurement_id, "G-TEST")
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
