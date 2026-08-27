#' Worker functions executed inside mirai daemons.
#'
#' Rules (tested): the function environment is `globalenv()` so serialising the
#' function does not drag this module along; every call is `pkg::fn()` or a
#' `box::use()` inside the body; inputs are a plain spec list plus a data frame;
#' formulas travel as strings; results are plain R objects (no external pointers).

#' @export
as_worker <- function(fn) {
  environment(fn) <- globalenv()
  # Source references (attached regardless of options("keep.source") by some
  # parsers, e.g. testthat's test-file loader) can be nested on every
  # sub-expression of the body, not just the function object, and can pin the
  # entire source file to the closure -- balloon its serialised size. Strip
  # them recursively so the worker stays small wherever it is defined.
  utils::removeSource(fn)
}

#' @export
fit_mixed_worker <- as_worker(function(spec, data) {
  started <- Sys.time()
  fit <- beezdemand::fit_demand_mixed(
    data = data,
    y_var = spec$y_var,
    x_var = spec$x_var,
    id_var = spec$id_var,
    factors = spec$factors,
    factor_interaction = isTRUE(spec$factor_interaction),
    equation_form = spec$equation_form,
    k = spec$k,
    collapse_levels = spec$collapse_levels,
    random_effects = stats::as.formula(spec$random_effects),
    covariance_structure = spec$covariance_structure,
    nlme_control = do.call(nlme::nlmeControl, spec$nlme_control),
    start_value_method = spec$start_value_method,
    continuous_covariates = spec$continuous_covariates
  )
  fit$tmb_obj <- NULL
  rss <- tryCatch(
    as.numeric(system(sprintf("ps -o rss= -p %d", Sys.getpid()), intern = TRUE)) / 1024,
    error = function(e) NA_real_
  )
  list(
    fit = fit,
    duration_ms = as.numeric(difftime(Sys.time(), started, units = "secs")) * 1000,
    worker_rss_mb = rss
  )
})

#' @export
fit_demand_fixed_worker <- as_worker(function(spec, data) {
  started <- Sys.time()
  box::use(app / logic / demand / fitting)
  fit <- if (isTRUE(spec$is_grouped)) {
    fitting$fit_demand_grouped(data, eq = spec$eq, agg = spec$agg, k = spec$k, constrainq0 = spec$constrainq0)
  } else {
    fitting$fit_demand_ungrouped(data, eq = spec$eq, agg = spec$agg, k = spec$k, constrainq0 = spec$constrainq0)
  }
  rss <- tryCatch(
    as.numeric(system(sprintf("ps -o rss= -p %d", Sys.getpid()), intern = TRUE)) / 1024,
    error = function(e) NA_real_
  )
  list(
    fit = fit,
    duration_ms = as.numeric(difftime(Sys.time(), started, units = "secs")) * 1000,
    worker_rss_mb = rss
  )
})
