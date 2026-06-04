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
