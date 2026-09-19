box::use(
  app/logic/startup_info,
)

# One line at process start that answers "which profile, which telemetry backend,
# sync or async fits" from a hosted platform's log panel, where nothing else can be
# inspected. It must never carry a credential, so every field is reduced to a value
# from a fixed vocabulary before it is printed.

describe("startup_line()", {
  it("reports profile, telemetry backend and daemon mode", {
    line <- startup_info$startup_line(
      config_active = "shinyapps",
      telemetry = list(enabled = TRUE, storage_type = "sqlite"),
      daemon_count = 0L
    )
    expect_identical(
      line,
      "startup: R_CONFIG_ACTIVE=shinyapps telemetry=sqlite daemons=0 (synchronous fits)"
    )
  })

  it("says which profile an unset or empty R_CONFIG_ACTIVE falls back to", {
    line <- startup_info$startup_line("", list(enabled = TRUE, storage_type = "sqlite"), 1L)
    expect_match(line, "R_CONFIG_ACTIVE=(unset, so default)", fixed = TRUE)
    expect_match(line, "daemons=1 (async fits)", fixed = TRUE)
  })

  it("reports disabled telemetry whatever the storage type says", {
    line <- startup_info$startup_line("production", list(enabled = FALSE, storage_type = "postgresql"), 2L)
    expect_match(line, "telemetry=disabled", fixed = TRUE)
  })

  it("never echoes a value outside the known vocabulary", {
    secret <- "postgresql://app:hunter2@db.example.com/telemetry"
    line <- startup_info$startup_line(
      config_active = secret,
      telemetry = list(enabled = TRUE, storage_type = secret),
      daemon_count = 1L
    )
    expect_no_match(line, "hunter2", fixed = TRUE)
    expect_match(line, "R_CONFIG_ACTIVE=(unrecognised)", fixed = TRUE)
    expect_match(line, "telemetry=(unrecognised)", fixed = TRUE)
  })

  it("accepts every profile defined in config.yml", {
    # Only the top-level keys matter; keep the !expr values as text rather than evaluate them.
    profiles <- names(yaml::read_yaml(
      file.path(project_root, "config.yml"),
      handlers = list(expr = function(x) x)
    ))
    expect_true(length(profiles) >= 5L)
    for (profile in profiles) {
      line <- startup_info$startup_line(profile, list(enabled = FALSE), 0L)
      expect_match(line, paste0("R_CONFIG_ACTIVE=", profile, " "), fixed = TRUE)
    }
  })
})
