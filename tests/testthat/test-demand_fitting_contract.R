# Real-package contract test for beezdemand::fit_demand_fixed()$results.
#
# app/logic/demand/fitting.R::format_demand_results() makes two hard
# assumptions about the live $results data frame:
#   1. the empirical block `Intensity:Pmaxe` is present and contiguous, so
#      dplyr::select(!(Intensity:Pmaxe)) drops exactly that block; and
#   2. every column named in its 2dp / 4dp rounding lists exists.
# The existing fitting tests check (2) only against a hand-built mock and the
# integration test only checks a table has `<td>` cells -- neither exercises
# the REAL column contract. This test closes that gap by calling the real
# package. A failure here is a genuine beezdemand regression, not a red-green
# step (characterization gate).

box::use(
  testthat[...],
)

box::use(
  app / logic / demand / fitting,
)

# Pivot the wide demand fixture (id, then one column per price) to the long
# id/x/y layout that beezdemand::fit_demand_fixed() consumes.
load_demand_long <- function() {
  wide <- utils::read.csv(
    testthat::test_path("fixtures", "demand-minimal.csv"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  data.frame(
    id = rep(wide[[1]], times = ncol(wide) - 1),
    x = as.numeric(rep(names(wide)[-1], each = nrow(wide))),
    y = as.numeric(unlist(wide[, -1], use.names = FALSE))
  )
}

describe("fit_demand_fixed $results column contract", {
  out <- suppressMessages(
    beezdemand::fit_demand_fixed(
      data = load_demand_long(),
      equation = "koff",
      agg = NULL,
      k = 2
    )
  )
  result_names <- names(out$results)

  it("places the empirical block Intensity:Pmaxe contiguously", {
    empirical_block <- c("Intensity", "BP0", "BP1", "Omaxe", "Pmaxe")
    expect_true(all(empirical_block %in% result_names))

    start <- match("Intensity", result_names)
    end <- match("Pmaxe", result_names)
    expect_false(is.na(start))
    expect_false(is.na(end))
    # The five empirical columns must form a single contiguous run, in order,
    # so dplyr::select(!(Intensity:Pmaxe)) drops exactly that block.
    expect_identical(result_names[start:end], empirical_block)
  })

  it("exposes every rounding-list column format_demand_results references", {
    cols_2dp <- c(
      "Q0d", "K", "R2", "Q0se", "AbsSS", "SdRes",
      "Q0Low", "Q0High", "EV", "Omaxd", "Pmaxd", "Omaxa", "Pmaxa"
    )
    cols_4dp <- c(
      "Alpha", "Alphase", "AlphaLow", "AlphaHigh",
      "alpha_star", "alpha_star_se"
    )
    expect_true(all(cols_2dp %in% result_names))
    expect_true(all(cols_4dp %in% result_names))
  })

  it("orders the empirical block ahead of the model columns", {
    expect_identical(
      result_names[1:6],
      c("id", "Intensity", "BP0", "BP1", "Omaxe", "Pmaxe")
    )
    # Model block starts with Equation, before the Q0d coefficient column.
    expect_lt(match("Equation", result_names), match("Q0d", result_names))
  })

  it("captures the full ordered $results contract", {
    # Snapshot the exact live column vector so any 0.3.0 column add / drop /
    # reorder surfaces as a reviewable diff rather than a silent downstream
    # break. (`converged` is new in 0.3.0; `alpha_star`/`alpha_star_se` are
    # present in the real output but were missing from the legacy mock.)
    expect_identical(
      result_names,
      c(
        "id", "Intensity", "BP0", "BP1", "Omaxe", "Pmaxe",
        "Equation", "Q0d", "K", "Alpha", "R2", "Q0se", "Alphase",
        "alpha_star", "alpha_star_se", "N", "AbsSS", "SdRes",
        "Q0Low", "Q0High", "AlphaLow", "AlphaHigh", "EV",
        "Omaxd", "Pmaxd", "Omaxa", "Pmaxa", "Notes", "converged"
      )
    )
  })

  it("format_demand_results drops the empirical block and keeps the rest", {
    formatted <- fitting$format_demand_results(out)
    fmt_names <- names(formatted)

    expect_false(
      any(c("Intensity", "BP0", "BP1", "Omaxe", "Pmaxe") %in% fmt_names)
    )
    expect_true("id" %in% fmt_names)
    expect_true(
      all(c("Equation", "Q0d", "Alpha", "alpha_star", "EV") %in% fmt_names)
    )
  })
})
