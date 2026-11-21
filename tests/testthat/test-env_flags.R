box::use(
  testthat[...],
)

box::use(
  app / logic / env_flags,
)

# On/off environment variables were read with as.logical(), which knows "TRUE" and "true"
# but returns NA for "1", "yes" and "on". TELEMETRY_ENABLED=1 in a deploy file therefore
# made `config$enabled && ...` fail instead of turning telemetry on.

describe("parse_flag()", {
  it("reads 1, true, yes, on and t as TRUE in any case", {
    for (value in c("1", "true", "TRUE", "True", "yes", "YES", "on", "On", "t", "T", " true ")) {
      expect_identical(env_flags$parse_flag(value, default = FALSE), TRUE, info = value)
    }
  })

  it("reads 0, false, no, off and f as FALSE in any case", {
    for (value in c("0", "false", "FALSE", "False", "no", "NO", "off", "Off", "f", "F", " 0 ")) {
      expect_identical(env_flags$parse_flag(value, default = TRUE), FALSE, info = value)
    }
  })

  it("falls back to the default for an unset, empty or unrecognised value", {
    for (value in list(NA_character_, "", "  ", "maybe", "2", "enabled")) {
      expect_identical(env_flags$parse_flag(value, default = TRUE), TRUE, info = value)
      expect_identical(env_flags$parse_flag(value, default = FALSE), FALSE, info = value)
    }
  })
})

describe("flag_warnings()", {
  it("is empty when every flag is unset or recognised", {
    withr::local_envvar(TELEMETRY_ENABLED = "1", GA_ENABLED = NA, LOG_TO_CONSOLE = "off")
    expect_identical(env_flags$flag_warnings(), character(0))
  })

  it("names a flag set to an unrecognised or empty value, and the default used", {
    withr::local_envvar(TELEMETRY_ENABLED = "maybe", GA_ENABLED = "")
    warnings <- env_flags$flag_warnings()
    expect_length(warnings, 2L)
    expect_match(warnings[1], "GA_ENABLED", fixed = TRUE)
    expect_match(warnings[1], "off", fixed = TRUE)
    expect_match(warnings[2], "TELEMETRY_ENABLED", fixed = TRUE)
    expect_match(warnings[2], "on", fixed = TRUE)
  })

  it("never echoes the value, which could be a pasted secret", {
    withr::local_envvar(TELEMETRY_ENABLED = "hunter2")
    expect_no_match(env_flags$flag_warnings(), "hunter2", fixed = TRUE)
  })
})

describe("config.yml on/off flags", {
  config_file <- file.path(project_root, "config.yml")
  # The development and production profiles set telemetry on outright and do not read
  # TELEMETRY_ENABLED.
  profiles <- c("default", "shinyapps", "connectcloud", "shinyproxy")
  read <- function(profile) config::get(config = profile, file = config_file)

  it("turns telemetry on for 1, yes and on, and off for 0, no and off, in each profile that reads it", {
    for (profile in profiles) {
      for (value in c("1", "yes", "on", "TRUE", "true")) {
        withr::local_envvar(TELEMETRY_ENABLED = value)
        expect_identical(read(profile)$telemetry$enabled, TRUE, info = paste(profile, value))
      }
      for (value in c("0", "no", "off", "FALSE", "false")) {
        withr::local_envvar(TELEMETRY_ENABLED = value)
        expect_identical(read(profile)$telemetry$enabled, FALSE, info = paste(profile, value))
      }
    }
  })

  it("keeps telemetry on, its default, for an unrecognised or empty value", {
    for (value in c("maybe", "")) {
      withr::local_envvar(TELEMETRY_ENABLED = value)
      expect_identical(read("shinyproxy")$telemetry$enabled, TRUE, info = value)
    }
  })

  it("reads every other on/off flag the same way, each with its own default", {
    # name in the environment, where it lands in the config, value when unset
    flags <- list(
      list("GA_ENABLED", c("google_analytics", "enabled"), FALSE),
      list("LOG_TO_CONSOLE", c("logging", "log_to_console"), TRUE),
      list("LOG_PERFORMANCE", c("logging", "log_performance"), TRUE),
      list("TELEMETRY_TRACK_INPUTS", c("telemetry", "track_inputs"), FALSE),
      list("TELEMETRY_ANONYMIZE", c("telemetry", "anonymize_sessions"), TRUE)
    )
    for (flag in flags) {
      for (case in list(list("1", TRUE), list("off", FALSE), list("maybe", flag[[3]]))) {
        withr::local_envvar(stats::setNames(case[[1]], flag[[1]]))
        got <- read("default")
        for (key in flag[[2]]) got <- got[[key]]
        expect_identical(got, case[[2]], info = paste(flag[[1]], case[[1]]))
      }
    }
  })
})
