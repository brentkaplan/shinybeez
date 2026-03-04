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

# Enable auto reloading via Rhino 1.7 (dev only — no value in Docker containers)
if (!identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyproxy") &&
    !identical(Sys.getenv("R_CONFIG_ACTIVE"), "production")) {
  options(shiny.autoreload = TRUE)
}

options(renv.config.snapshot.preflight = FALSE)
options(renv.config.snapshot.auto = FALSE)

# Shiny ports
options(shiny.host = "0.0.0.0")
options(shiny.port = 3838)
