# Suppress renv sync check in production (Docker library is immutable)
if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyproxy")) {
  options(renv.config.synchronized.check = FALSE)
}

if (file.exists("renv")) {
  source("renv/activate.R")
} else {
  # The `renv` directory is automatically skipped when deploying with rsconnect.
  message("No 'renv' directory found; renv won't be activated.")
}

# Allow absolute module imports (relative to the app root).
options(box.path = getwd())

# Auto reloading via Rhino 1.7, for the profiles people develop under and nothing else.
# An allow-list, so a new hosted profile (shinyapps, connectcloud, ...) is off by default.
options(shiny.autoreload = Sys.getenv("R_CONFIG_ACTIVE") %in% c("", "default", "development"))

options(renv.config.snapshot.preflight = FALSE)
options(renv.config.snapshot.auto = FALSE)

# Shiny ports
options(shiny.host = "0.0.0.0")
options(shiny.port = 3838)
