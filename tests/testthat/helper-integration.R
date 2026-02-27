# Integration test helpers for shinytest2
# Loaded automatically by testthat before test files

# Only load shinytest2 if available (avoids errors on systems without it)
if (requireNamespace("shinytest2", quietly = TRUE)) {
  library(shinytest2)
}

# ---------------------------------------------------------------------------
# Skip helpers
# ---------------------------------------------------------------------------

skip_if_not_integration <- function() {
  testthat::skip_if_not(
    requireNamespace("shinytest2", quietly = TRUE),
    message = "shinytest2 not available"
  )
}

skip_if_not_full_tests <- function() {
  testthat::skip_if_not(
    identical(Sys.getenv("SHINYBEEZ_FULL_TESTS"), "true"),
    message = "Set SHINYBEEZ_FULL_TESTS=true for full integration tests"
  )
}

require_app <- function(app) {
  if (is.null(app)) skip("App driver not available")
}

# ---------------------------------------------------------------------------
# Path helpers
# ---------------------------------------------------------------------------

fixture_path <- function(filename) {
  file.path(find_project_root(), "tests", "testthat", "fixtures", filename)
}

example_path <- function(filename) {
  file.path(find_project_root(), "app", "static", "data", "examples", filename)
}

# ---------------------------------------------------------------------------
# Namespace ID helper
# ---------------------------------------------------------------------------

ns_id <- function(...) {
  paste(c("app", ...), collapse = "-")
}

# ---------------------------------------------------------------------------
# Verified namespace ID constants
# ---------------------------------------------------------------------------

ids <- list(
  # Navigation (root scope, NOT namespaced)
  nav = "nav",


  # --- Demand tab ---
  demand = list(
    upload = ns_id("demand", "upload_demand", "upload"),
    group = ns_id("demand", "group"),
    equation = ns_id("demand", "equation"),
    k = ns_id("demand", "k"),
    k_output = ns_id("demand", "k_value"),
    analysis_type = ns_id("demand", "analysis_type"),
    calculate = ns_id("demand", "calculate_demand"),
    calculate_output = ns_id("demand", "calculate")
  ),

  # --- Discounting tab ---
  discounting = list(
    upload = ns_id("discounting", "discounting", "upload"),
    calc_type = ns_id("discounting", "calc_discounting"),
    equation = ns_id("discounting", "equation"),
    equation_output = ns_id("discounting", "dd_eq"),
    analysis_type = ns_id("discounting", "analysis_type"),
    analysis_type_output = ns_id("discounting", "dd_method"),
    imputation = ns_id("discounting", "imputation"),
    imputation_output = ns_id("discounting", "mcq_imputation"),
    trans = ns_id("discounting", "trans"),
    trans_output = ns_id("discounting", "mcq_trans"),
    calculate = ns_id("discounting", "calculate_discounting"),
    calculate_output = ns_id("discounting", "calculate")
  ),

  # --- Mixed Effects tab ---
  mixed = list(
    upload = ns_id("mixed_effects_demand", "upload_mixed_effects_demand", "upload"),
    model_choice = ns_id("mixed_effects_demand", "model_choice"),
    id_var = ns_id("mixed_effects_demand", "id_variable_choice"),
    x_var = ns_id("mixed_effects_demand", "x_variable_choice"),
    y_var = ns_id("mixed_effects_demand", "y_variable_choice"),
    factor1 = ns_id("mixed_effects_demand", "factor1_choice"),
    factor2 = ns_id("mixed_effects_demand", "factor2_choice"),
    random_effects = ns_id("mixed_effects_demand", "random_effects_spec"),
    covariance = ns_id("mixed_effects_demand", "covariance_structure"),
    run = ns_id("mixed_effects_demand", "run_mixed_model")
  ),

  # --- Info modal ---
  info = list(
    trigger = ns_id("info", "info")
  )
)

# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------

wait_for_calc <- function(app, sleep_secs = 5) {
  Sys.sleep(sleep_secs)
  app$wait_for_idle(duration = 2000)
}

# ---------------------------------------------------------------------------
# AppDriver factory
# ---------------------------------------------------------------------------

create_app_driver <- function(...) {
  skip_if_not_integration()

  # Disable telemetry and GA in the subprocess
  Sys.setenv(
    TELEMETRY_ENABLED = "FALSE",
    GA_ENABLED = "FALSE",
    NOT_CRAN = "true"
  )

  # plotly loads on first request, so Page.navigate can exceed chromote's
  # default 10s timeout. Raise it to match load_timeout.
  options(chromote.timeout = 30)

  tryCatch(
    shinytest2::AppDriver$new(
      app_dir = find_project_root(),
      name = "shinybeez",
      height = 900,
      width = 1200,
      load_timeout = 30000,
      timeout = 15000,
      seed = 12345,
      check_names = FALSE,
      # Override .Rprofile's shiny.port=3838 with a random port
      options = list(
        shiny.testmode = TRUE,
        shiny.port = httpuv::randomPort(),
        shiny.host = "127.0.0.1"
      ),
      ...
    ),
    error = function(e) {
      testthat::skip(paste(
        "`shinytest2::AppDriver` can not be initialized as Chrome failed:",
        conditionMessage(e)
      ))
    }
  )
}
