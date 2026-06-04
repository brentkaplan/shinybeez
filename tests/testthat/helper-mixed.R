# Test helper: fit one small, real beezdemand_nlme model from the
# mixed-effects-minimal.csv fixture, memoized so the wrapper tests
# (comparisons + EMMs) can reuse a single fit instead of refitting.
# Uses the real package (no mocks) to exercise the 0.3.0 API end-to-end.

.mixed_fixture_env <- new.env(parent = emptyenv())

# Load the minimal mixed-effects fixture (monkey, x, y, y_ll4, drug, dose).
load_mixed_fixture <- function() {
  utils::read.csv(
    testthat::test_path("fixtures", "mixed-effects-minimal.csv"),
    stringsAsFactors = FALSE
  )
}

# Fit (once) a small NLME demand model: zero-bounded exponential form, drug as
# the single factor, Q0 + alpha random effects, pdDiag covariance. Mirrors the
# app's NLME path (y_ll4 is the ZBE-transformed consumption column).
fit_mixed_fixture <- function() {
  if (is.null(.mixed_fixture_env$fit)) {
    dat <- load_mixed_fixture()
    .mixed_fixture_env$fit <- suppressMessages(
      beezdemand::fit_demand_mixed(
        data = dat,
        y_var = "y_ll4",
        x_var = "x",
        id_var = "monkey",
        factors = "drug",
        equation_form = "zben",
        random_effects = Q0 + alpha ~ 1,
        covariance_structure = "pdDiag"
      )
    )
  }
  .mixed_fixture_env$fit
}

# Fit (once) a two-factor model crossing the within-subject drug factor with a
# synthetic between-subject `site` factor, so the contrast_by stratification
# path through run_demand_comparisons() can be exercised directly.
fit_mixed_fixture_2factor <- function() {
  if (is.null(.mixed_fixture_env$fit2)) {
    dat <- load_mixed_fixture()
    dat$site <- ifelse(dat$monkey %in% c("A", "B"), "north", "south")
    .mixed_fixture_env$fit2 <- suppressMessages(
      beezdemand::fit_demand_mixed(
        data = dat,
        y_var = "y_ll4",
        x_var = "x",
        id_var = "monkey",
        factors = c("drug", "site"),
        factor_interaction = TRUE,
        equation_form = "zben",
        random_effects = Q0 + alpha ~ 1,
        covariance_structure = "pdDiag"
      )
    )
  }
  .mixed_fixture_env$fit2
}

# Fit (once) a single-factor model with a synthetic continuous covariate (one
# value per subject), so the `at` covariate-conditioning path through both
# wrappers can be exercised directly.
fit_mixed_fixture_covariate <- function() {
  if (is.null(.mixed_fixture_env$fitc)) {
    dat <- load_mixed_fixture()
    dat$bodyweight <- unname(c(A = 8.2, B = 7.4, C = 9.1)[dat$monkey])
    .mixed_fixture_env$fitc <- suppressMessages(
      beezdemand::fit_demand_mixed(
        data = dat,
        y_var = "y_ll4",
        x_var = "x",
        id_var = "monkey",
        factors = "drug",
        continuous_covariates = "bodyweight",
        equation_form = "zben",
        random_effects = Q0 + alpha ~ 1,
        covariance_structure = "pdDiag"
      )
    )
  }
  .mixed_fixture_env$fitc
}
