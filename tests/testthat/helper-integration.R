# Integration test helpers for shinytest2
# Loaded automatically by testthat before test files

# Only load shinytest2 if available (avoids errors on systems without it)
if (requireNamespace("shinytest2", quietly = TRUE)) {
  library(shinytest2)
}

# ---------------------------------------------------------------------------
# Chrome CI flags
# ---------------------------------------------------------------------------

if (nzchar(Sys.getenv("CI")) || nzchar(Sys.getenv("CHROMOTE_CHROME"))) {
  chromote::set_chrome_args(c(
    chromote::get_chrome_args(),
    "--no-sandbox",
    "--disable-gpu",
    "--disable-dev-shm-usage"
  ))
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
# Result assertions
# ---------------------------------------------------------------------------

# Always scope result assertions to the results container. `get_html()` uses
# querySelectorAll() and returns EVERY match, so an unscoped ".datatables"
# selector combined with `any(grepl(...))` is satisfied by the raw data preview
# table -- meaning such an assertion passes whether or not the model produced
# anything, and would keep passing if results rendering broke entirely.
#
# Scoping alone is not sufficient, for two reasons:
#   1. DataTables renders an empty state as `<td class="dataTables_empty">No
#      data available in table</td>`, so a `<td` match does not imply data.
#   2. The container keeps the PREVIOUS run's table, so a re-run that silently
#      fails to fire still leaves populated, correctly-columned HTML behind.
# Hence these helpers count real data rows, and callers pass the row count they
# expect for that particular analysis so a stale table cannot satisfy them.

# Count data rows inside `#result_id`, excluding DataTables' empty-state row.
# Returns -1 when the container itself is absent.
results_row_count <- function(app, result_id) {
  js <- sprintf(
    paste0(
      "(function(){var c=document.getElementById('%s');if(!c)return -1;",
      "var rows=c.querySelectorAll('tbody tr');var n=0;",
      "for(var i=0;i<rows.length;i++){",
      "if(!rows[i].querySelector('td.dataTables_empty'))n++;}",
      "return n;})()"
    ),
    result_id
  )
  as.integer(app$get_js(js))
}

# Exact column-header text for `#result_id`. Substring matching over raw HTML is
# not enough: "Alpha" also matches the AlphaSE / AlphaLow / AlphaHigh columns, so
# dropping the Alpha column entirely would still pass a grepl() check.
results_headers <- function(app, result_id) {
  js <- sprintf(
    paste0(
      "(function(){var c=document.getElementById('%s');if(!c)return [];",
      "return Array.prototype.map.call(c.querySelectorAll('thead th'),",
      "function(th){return th.textContent.trim();});})()"
    ),
    result_id
  )
  unlist(app$get_js(js))
}

# Assert the results table at `#result_id` rendered real data rows.
# `n_rows` pins the exact expected count, which is what distinguishes a fresh
# result from the previous analysis's leftovers; `min_rows` is the looser form
# for cases where the count is not fixed by the fixture.
expect_results_table <- function(app, result_id, n_rows = NULL, min_rows = 1L,
                                 info = NULL) {
  actual <- results_row_count(app, result_id)
  testthat::expect_false(
    identical(actual, -1L),
    label = paste0("results container #", result_id, " exists")
  )
  if (!is.null(n_rows)) {
    testthat::expect_equal(
      actual, as.integer(n_rows),
      info = info %||% paste0(
        "expected exactly ", n_rows, " data rows in ", result_id,
        " (guards against a stale table from the previous run)"
      )
    )
  } else {
    testthat::expect_gte(actual, as.integer(min_rows))
  }
  invisible(actual)
}

# Demand results additionally carry the fitted demand parameters. Exact header
# membership keeps this meaningful: a table that rendered but lost a parameter
# column is a failure worth catching.
expect_demand_results <- function(app, result_id, n_rows = NULL, min_rows = 1L,
                                  cols = c("Q0d", "Alpha", "Omaxd", "Pmaxd", "EV")) {
  actual <- expect_results_table(
    app, result_id, n_rows = n_rows, min_rows = min_rows
  )
  headers <- results_headers(app, result_id)
  for (col in cols) {
    testthat::expect_true(
      col %in% headers,
      info = paste0(
        "expected demand column header: ", col,
        " (present: ", paste(headers, collapse = ", "), ")"
      )
    )
  }
  invisible(actual)
}

# ---------------------------------------------------------------------------
# Condition-based wait helpers
# ---------------------------------------------------------------------------

wait_for_datatable <- function(app, timeout_ms = 15000) {
  app$wait_for_js(
    "document.querySelector('.datatables table tbody tr') !== null",
    timeout = timeout_ms
  )
  app$wait_for_idle(duration = 500, timeout = timeout_ms)
}

wait_for_result_rows <- function(app, result_id, entries_text, timeout_ms = 20000) {
  # Wait until the DataTables info line inside `#result_id` reports the given
  # "of N entries" text. renderDT(server = FALSE) initialises DataTables in the
  # browser a beat after Shiny goes idle, so wait_for_output() alone can return
  # before the info line exists. Scope to the results container so the raw data
  # table's own info line ("of 4 entries") can never satisfy this.
  js <- sprintf(
    paste0(
      "(function(){var c=document.getElementById('%s');",
      "if(!c)return false;var e=c.querySelector('.dataTables_info');",
      "return !!(e && e.innerText.indexOf('%s')>=0);})()"
    ),
    result_id, entries_text
  )
  app$wait_for_js(js, timeout = timeout_ms)
}

wait_for_output <- function(app, output_id, timeout_ms = 15000) {
  # Idle-based detection first: wait until Shiny has been idle for 1s. This is
  # more robust than a JS element check alone for outputs wrapped in renderUI
  # (e.g., discounting results inside uiOutput → DTOutput chain).
  app$wait_for_idle(duration = 1000, timeout = timeout_ms)

  # Then confirm the requested output actually rendered something. Previously
  # `output_id` was accepted and never used, so this returned as soon as Shiny
  # went idle -- including when the target output was still empty. Callers that
  # then asserted against an unscoped selector could pass on an unrelated
  # element (e.g. the upload preview table). Kept deliberately generic: callers
  # pass table containers and text summaries alike, so assert only that the
  # element exists and is non-empty -- the calling test is responsible for
  # asserting the content is fresh and correct (see expect_results_table()).
  #
  # A missing or malformed id is an error, not a reason to silently fall back to
  # idle-only waiting -- that fallback is the exact defect this helper fixes.
  if (!is.character(output_id) || length(output_id) != 1L ||
        is.na(output_id) || !nzchar(output_id)) {
    stop("wait_for_output() requires a single non-empty output id", call. = FALSE)
  }
  js <- sprintf(
    paste0(
      "(function(){var e=document.getElementById('%s');",
      "return !!(e && e.innerHTML.trim().length > 0);})()"
    ),
    output_id
  )
  app$wait_for_js(js, timeout = timeout_ms)
}

wait_for_notification <- function(app, type = "error", timeout_ms = 10000) {
  selector <- paste0(".shiny-notification-", type)
  js <- sprintf("document.querySelector('%s') !== null", selector)
  app$wait_for_js(js, timeout = timeout_ms)
  app$wait_for_idle(duration = 500, timeout = timeout_ms)
}

wait_for_input <- function(app, input_id, timeout_ms = 5000) {
  app$wait_for_value(input = input_id, timeout = timeout_ms)
  app$wait_for_idle(duration = 500, timeout = timeout_ms)
}

navigate_to_tab <- function(app, tab_value) {
  app$set_inputs(!!ids$nav := tab_value)
  app$wait_for_idle(duration = 500)
}

upload_and_wait <- function(app, input_id, file_path, timeout_ms = 15000) {
  args <- stats::setNames(list(file_path), input_id)
  do.call(app$upload_file, args)
  wait_for_datatable(app, timeout_ms = timeout_ms)
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
