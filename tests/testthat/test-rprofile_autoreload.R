# .Rprofile decides shiny.autoreload from R_CONFIG_ACTIVE. It used to turn it on for
# everything except `shinyproxy` and `production`, so the hosted `shinyapps` profile
# shipped shiny-autoreload.js and a file watcher to a public app. Autoreload is now on
# only for the profiles people develop under. `default` has to stay on: .env.example,
# docker-compose.dev.yml and the telemetry test helper all run under it.
#
# .Rprofile is read once, at startup, so every case needs its own R process.

autoreload_in_fresh_process <- function(r_config_active) {
  # NA unsets the variable in the child, which is not the same case as "".
  out <- withr::with_envvar(
    c(R_CONFIG_ACTIVE = r_config_active),
    withr::with_dir(
      project_root,
      system2(
        file.path(R.home("bin"), "Rscript"),
        c("-e", shQuote("cat('AUTORELOAD=', isTRUE(getOption('shiny.autoreload')), '\\n', sep = '')")),
        stdout = TRUE,
        stderr = FALSE
      )
    )
  )
  marker <- grep("^AUTORELOAD=", out, value = TRUE)
  stopifnot(length(marker) == 1L)
  identical(marker, "AUTORELOAD=TRUE")
}

describe(".Rprofile shiny.autoreload", {
  cases <- list(
    list(value = NA, label = "unset", expected = TRUE),
    list(value = "", label = "empty", expected = TRUE),
    list(value = "default", label = "default", expected = TRUE),
    list(value = "development", label = "development", expected = TRUE),
    list(value = "production", label = "production", expected = FALSE),
    list(value = "shinyproxy", label = "shinyproxy", expected = FALSE),
    list(value = "shinyapps", label = "shinyapps", expected = FALSE),
    list(value = "connectcloud", label = "connectcloud", expected = FALSE)
  )

  for (case in cases) {
    it(sprintf("is %s when R_CONFIG_ACTIVE is %s", case$expected, case$label), {
      expect_identical(autoreload_in_fresh_process(case$value), case$expected)
    })
  }
})
