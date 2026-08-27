box::use(
  testthat[...],
  mirai,
)
box::use(
  app / logic / async / daemons,
  app / logic / async / workers,
  app / logic / demand / fitting,
)

mixed_spec <- function() {
  list(
    y_var = "y_ll4", x_var = "x", id_var = "monkey", factors = "drug", factor_interaction = FALSE,
    equation_form = "zben", k = NULL, collapse_levels = NULL, random_effects = "Q0 + alpha ~ 1",
    covariance_structure = "pdDiag",
    nlme_control = list(maxIter = 50, pnlsMaxIter = 7, msMaxIter = 50, tolerance = 1e-6,
                        pnlsTol = 0.001, minScale = 0.001, niterEM = 25),
    start_value_method = "pooled_nls", continuous_covariates = NULL
  )
}

# Direct comparison fit using exactly the same arguments as mixed_spec(), so
# the parity check below is not confounded by fit_mixed_fixture()'s defaults
# (which use a different nlme_control / start_value_method).
fit_mixed_direct <- function() {
  dat <- load_mixed_fixture()
  spec <- mixed_spec()
  beezdemand::fit_demand_mixed(
    data = dat,
    y_var = spec$y_var,
    x_var = spec$x_var,
    id_var = spec$id_var,
    factors = spec$factors,
    factor_interaction = spec$factor_interaction,
    equation_form = spec$equation_form,
    k = spec$k,
    collapse_levels = spec$collapse_levels,
    random_effects = stats::as.formula(spec$random_effects),
    covariance_structure = spec$covariance_structure,
    nlme_control = do.call(nlme::nlmeControl, spec$nlme_control),
    start_value_method = spec$start_value_method,
    continuous_covariates = spec$continuous_covariates
  )
}

# beezdemand::fit_demand_fixed() (called via fit_demand_ungrouped) expects
# long-format id/x/y columns (its defaults), same as the fixture pivot used
# by test-demand_fitting_contract.R for demand-minimal.csv.
load_demand_long_fixture <- function() {
  wide <- utils::read.csv(
    file.path(getOption("box.path"), "tests/testthat/fixtures/demand-minimal.csv"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  data.frame(
    id = rep(wide[[1]], times = ncol(wide) - 1),
    x = as.numeric(rep(names(wide)[-1], each = nrow(wide))),
    y = as.numeric(unlist(wide[, -1], use.names = FALSE))
  )
}

describe("as_worker", {
  it("resets the function environment to globalenv so it serialises without the module", {
    w <- workers$as_worker(function(spec, data) spec$x)
    expect_identical(environment(w), globalenv())
    expect_lt(length(serialize(w, NULL)), 20000)
  })
})

describe("fit_mixed_worker", {
  it("is small when serialised and lives in globalenv", {
    expect_identical(environment(workers$fit_mixed_worker), globalenv())
    expect_lt(length(serialize(workers$fit_mixed_worker, NULL)), 20000)
  })
  it("returns the same fixed effects as a direct fit_demand_mixed call", {
    dat <- load_mixed_fixture()
    out <- suppressMessages(workers$fit_mixed_worker(mixed_spec(), dat))
    expect_s3_class(out$fit, "beezdemand_nlme")
    expect_gt(out$duration_ms, 0)
    expect_true(is.numeric(out$worker_rss_mb))
    expect_null(out$fit$tmb_obj)
    direct <- suppressMessages(fit_mixed_direct())
    expect_equal(nlme::fixef(out$fit$model), nlme::fixef(direct$model))
  })
  it("produces identical fixed effects when run inside a daemon", {
    daemons$start_daemons(1L)
    on.exit(daemons$stop_daemons(), add = TRUE)
    dat <- load_mixed_fixture()
    w <- workers$fit_mixed_worker
    out <- mirai$mirai(w(spec, data), w = w, spec = mixed_spec(), data = dat)[]
    expect_false(mirai$is_error_value(out))
    direct <- suppressMessages(fit_mixed_direct())
    expect_equal(nlme::fixef(out$fit$model), nlme::fixef(direct$model))
  })
})

describe("fit_demand_fixed_worker", {
  it("is small when serialised and lives in globalenv", {
    expect_identical(environment(workers$fit_demand_fixed_worker), globalenv())
    expect_lt(length(serialize(workers$fit_demand_fixed_worker, NULL)), 20000)
  })
  it("fits ungrouped example data via app/logic/demand/fitting inside a daemon", {
    daemons$start_daemons(1L)
    on.exit(daemons$stop_daemons(), add = TRUE)
    dat <- load_demand_long_fixture()
    spec <- list(is_grouped = FALSE, eq = "koff", agg = "Mean", k = 2, constrainq0 = NULL)
    w <- workers$fit_demand_fixed_worker
    out <- mirai$mirai(w(spec, data), w = w, spec = spec, data = dat)[]
    expect_false(mirai$is_error_value(out))
    expect_true(is.list(out$fit))
    expect_true(all(c("output", "results") %in% names(out$fit)))
    # Parity: the daemon result must match a direct call with the same args.
    direct <- suppressWarnings(fitting$fit_demand_ungrouped(
      dat,
      eq = spec$eq, agg = spec$agg, k = spec$k, constrainq0 = spec$constrainq0
    ))
    expect_equal(out$fit$results, direct$results)
  })
})
