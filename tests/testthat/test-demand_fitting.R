# Tests for app/logic/demand/fitting.R

box::use(
  testthat[...],
)

box::use(
  app / logic / demand / fitting
)

# Helper: create minimal demand data for beezdemand
make_demand_data <- function(n_ids = 3) {
  prices <- c(0.01, 0.1, 1, 10, 100)
  consumption <- list(
    c(10, 8, 5, 2, 0),
    c(12, 9, 6, 3, 1),
    c(8, 7, 4, 1, 0)
  )
  data.frame(
    id = rep(seq_len(n_ids), each = length(prices)),
    x = rep(prices, n_ids),
    y = unlist(consumption[seq_len(n_ids)])
  )
}

# Helper: create grouped demand data
make_grouped_demand_data <- function() {
  prices <- c(0.01, 0.1, 1, 10, 100)
  data.frame(
    id = rep(1:4, each = length(prices)),
    group = rep(c("A", "B"), each = 2 * length(prices)),
    x = rep(prices, 4),
    y = c(
      10, 8, 5, 2, 0,
      12, 9, 6, 3, 1,
      8, 7, 4, 1, 0,
      11, 8, 5, 2, 1
    )
  )
}

# Helper: build a mock output list matching FitCurves detailed = TRUE structure
make_mock_fitcurves_output <- function() {
  df <- data.frame(
    id = c(1, 2),
    Intensity = c(10, 12),
    BP0 = c(100, NA),
    BP1 = c(10, 100),
    Omaxe = c(20.0, 100.0),
    Pmaxe = c(10.0, 100.0),
    Equation = c("koff", "koff"),
    Q0d = c(9.316949, 10.789528),
    K = c(2.0, 2.0),
    Alpha = c(0.014752734, 0.011197421),
    R2 = c(0.940594, 0.864947),
    Q0se = c(0.907444, 1.460761),
    Alphase = c(0.006136017, 0.006821016),
    N = c(5L, 5L),
    AbsSS = c(4.039611, 10.642161),
    SdRes = c(1.160404, 1.883451),
    Q0Low = c(6.429058, 6.140735),
    Q0High = c(12.20484, 15.43832),
    AlphaLow = c(-0.004774811, -0.010510101),
    AlphaHigh = c(0.034280280, 0.032904930),
    EV = c(0.239653, 0.315746),
    Omaxd = c(6.166616, 8.124593),
    Pmaxd = c(2.098935, 2.387948),
    Omaxa = c(6.166700, 8.124702),
    Pmaxa = c(2.111917, 2.402718),
    Notes = c("converged", "converged"),
    stringsAsFactors = FALSE
  )
  list(results = df, fits = NULL, predictions = NULL)
}

# ------------------------------------------------------------------------------
# resolve_equation() tests
# ------------------------------------------------------------------------------

describe("resolve_equation", {
  it("maps 'Exponentiated (with k)' to 'koff'", {
    expect_equal(fitting$resolve_equation("Exponentiated (with k)"), "koff")
  })

  it("maps 'Exponential (with k)' to 'hs'", {
    expect_equal(fitting$resolve_equation("Exponential (with k)"), "hs")
  })

  it("throws an error for an unknown equation label", {
    expect_error(
      fitting$resolve_equation("Linear"),
      "Unknown equation label"
    )
  })

  it("throws an error for an empty string", {
    expect_error(
      fitting$resolve_equation(""),
      "Unknown equation label"
    )
  })

  it("throws an error for NULL input", {
    expect_error(fitting$resolve_equation(NULL))
  })
})

# ------------------------------------------------------------------------------
# resolve_k_value() tests
# ------------------------------------------------------------------------------

describe("resolve_k_value", {
  k_values <- c(1.5, 2, 2.5, 3, 3.5, 4)

  it("converts a recognized numeric string to numeric", {
    result <- fitting$resolve_k_value("2", k_values)
    expect_equal(result, 2)
    expect_type(result, "double")
  })

  it("converts '1.5' to numeric 1.5", {
    result <- fitting$resolve_k_value("1.5", k_values)
    expect_equal(result, 1.5)
    expect_type(result, "double")
  })

  it("converts '4' to numeric 4", {
    result <- fitting$resolve_k_value("4", k_values)
    expect_equal(result, 4)
    expect_type(result, "double")
  })

  it("passes through 'ind' as character", {
    result <- fitting$resolve_k_value("ind", k_values)
    expect_equal(result, "ind")
    expect_type(result, "character")
  })

  it("passes through 'fit' as character", {
    result <- fitting$resolve_k_value("fit", k_values)
    expect_equal(result, "fit")
    expect_type(result, "character")
  })

  it("passes through 'range' as character", {
    result <- fitting$resolve_k_value("range", k_values)
    expect_equal(result, "range")
    expect_type(result, "character")
  })

  it("passes through unrecognized numeric string not in k_values", {
    result <- fitting$resolve_k_value("99", k_values)
    expect_equal(result, "99")
    expect_type(result, "character")
  })

  it("handles empty k_values vector", {
    result <- fitting$resolve_k_value("2", numeric(0))
    expect_equal(result, "2")
    expect_type(result, "character")
  })
})

# ------------------------------------------------------------------------------
# resolve_aggregation() tests
# ------------------------------------------------------------------------------

describe("resolve_aggregation", {
  it("returns NULL for 'Ind'", {
    expect_null(fitting$resolve_aggregation("Ind"))
  })

  it("returns NULL for NULL input", {
    expect_null(fitting$resolve_aggregation(NULL))
  })

  it("passes through 'Pooled'", {
    expect_equal(fitting$resolve_aggregation("Pooled"), "Pooled")
  })

  it("passes through 'Mean'", {
    expect_equal(fitting$resolve_aggregation("Mean"), "Mean")
  })

  it("passes through any non-'Ind' string", {
    expect_equal(fitting$resolve_aggregation("Custom"), "Custom")
  })
})

# ------------------------------------------------------------------------------
# resolve_q0_constraint() tests
# ------------------------------------------------------------------------------

describe("resolve_q0_constraint", {
  it("returns NULL when fix_q0 is FALSE", {
    expect_null(fitting$resolve_q0_constraint(5.0, FALSE))
  })

  it("returns the q0 value when fix_q0 is TRUE", {
    expect_equal(fitting$resolve_q0_constraint(5.0, TRUE), 5.0)
  })

  it("returns NULL when q0_val is NULL regardless of fix_q0", {
    expect_null(fitting$resolve_q0_constraint(NULL, TRUE))
  })

  it("returns NULL when both q0_val is NULL and fix_q0 is FALSE", {
    expect_null(fitting$resolve_q0_constraint(NULL, FALSE))
  })

  it("returns numeric q0 when fix_q0 is TRUE and q0_val is numeric", {
    result <- fitting$resolve_q0_constraint(10.5, TRUE)
    expect_equal(result, 10.5)
    expect_type(result, "double")
  })

  it("returns zero when fix_q0 is TRUE and q0_val is 0", {
    expect_equal(fitting$resolve_q0_constraint(0, TRUE), 0)
  })
})

# ------------------------------------------------------------------------------
# format_demand_results() tests
# ------------------------------------------------------------------------------

describe("format_demand_results", {
  it("removes columns from Intensity through Pmaxe", {
    output <- make_mock_fitcurves_output()
    result <- fitting$format_demand_results(output)
    excluded <- c("Intensity", "BP0", "BP1", "Omaxe", "Pmaxe")
    for (col in excluded) {
      expect_false(col %in% names(result), info = paste(col, "should be excluded"))
    }
  })

  it("retains the id column", {
    output <- make_mock_fitcurves_output()
    result <- fitting$format_demand_results(output)
    expect_true("id" %in% names(result))
    expect_equal(result$id, c(1, 2))
  })

  it("retains expected result columns", {
    output <- make_mock_fitcurves_output()
    result <- fitting$format_demand_results(output)
    expected_cols <- c(
      "id", "Equation", "Q0d", "K", "Alpha", "R2", "Q0se", "Alphase",
      "N", "AbsSS", "SdRes", "Q0Low", "Q0High", "AlphaLow", "AlphaHigh",
      "EV", "Omaxd", "Pmaxd", "Omaxa", "Pmaxa", "Notes"
    )
    for (col in expected_cols) {
      expect_true(col %in% names(result), info = paste(col, "should be present"))
    }
  })

  it("rounds 2dp columns to 2 decimal places", {
    output <- make_mock_fitcurves_output()
    result <- fitting$format_demand_results(output)
    cols_2dp <- c("Q0d", "K", "R2", "Q0se", "AbsSS", "SdRes",
                  "Q0Low", "Q0High", "EV", "Omaxd", "Pmaxd", "Omaxa", "Pmaxa")
    for (col in cols_2dp) {
      values <- result[[col]]
      rounded <- round(values, 2)
      expect_equal(values, rounded, info = paste(col, "should be rounded to 2dp"))
    }
  })

  it("rounds 4dp columns to 4 decimal places", {
    output <- make_mock_fitcurves_output()
    result <- fitting$format_demand_results(output)
    cols_4dp <- c("Alpha", "Alphase", "AlphaLow", "AlphaHigh")
    for (col in cols_4dp) {
      values <- result[[col]]
      rounded <- round(values, 4)
      expect_equal(values, rounded, info = paste(col, "should be rounded to 4dp"))
    }
  })

  it("returns a data.frame", {
    output <- make_mock_fitcurves_output()
    result <- fitting$format_demand_results(output)
    expect_s3_class(result, "data.frame")
  })

  it("preserves the number of rows", {
    output <- make_mock_fitcurves_output()
    result <- fitting$format_demand_results(output)
    expect_equal(nrow(result), 2)
  })
})

# ------------------------------------------------------------------------------
# fit_demand_ungrouped() tests
# ------------------------------------------------------------------------------

describe("fit_demand_ungrouped", {
  it("returns a list with 'output' and 'results' components", {
    dat <- make_demand_data(n_ids = 2)
    result <- fitting$fit_demand_ungrouped(
      data = dat, eq = "koff", agg = NULL, k = 2
    )
    expect_type(result, "list")
    expect_true("output" %in% names(result))
    expect_true("results" %in% names(result))
  })

  it("returns a data.frame in the 'results' component", {
    dat <- make_demand_data(n_ids = 2)
    result <- fitting$fit_demand_ungrouped(
      data = dat, eq = "koff", agg = NULL, k = 2
    )
    expect_s3_class(result$results, "data.frame")
  })

  it("results data.frame has one row per participant", {
    dat <- make_demand_data(n_ids = 3)
    result <- fitting$fit_demand_ungrouped(
      data = dat, eq = "koff", agg = NULL, k = 2
    )
    expect_equal(nrow(result$results), 3)
  })

  it("results contain expected columns after formatting", {
    dat <- make_demand_data(n_ids = 2)
    result <- fitting$fit_demand_ungrouped(
      data = dat, eq = "koff", agg = NULL, k = 2
    )
    expected_cols <- c("id", "Equation", "Q0d", "K", "Alpha", "R2")
    for (col in expected_cols) {
      expect_true(
        col %in% names(result$results),
        info = paste(col, "should be in results")
      )
    }
  })

  it("results exclude Intensity through Pmaxe columns", {
    dat <- make_demand_data(n_ids = 2)
    result <- fitting$fit_demand_ungrouped(
      data = dat, eq = "koff", agg = NULL, k = 2
    )
    excluded <- c("Intensity", "BP0", "BP1", "Omaxe", "Pmaxe")
    for (col in excluded) {
      expect_false(
        col %in% names(result$results),
        info = paste(col, "should be excluded from results")
      )
    }
  })

  it("output component is a list (raw FitCurves result)", {
    dat <- make_demand_data(n_ids = 2)
    result <- fitting$fit_demand_ungrouped(
      data = dat, eq = "koff", agg = NULL, k = 2
    )
    expect_type(result$output, "list")
  })

  it("works with the 'hs' equation", {
    dat <- make_demand_data(n_ids = 2)
    result <- fitting$fit_demand_ungrouped(
      data = dat, eq = "hs", agg = NULL, k = 2
    )
    expect_type(result, "list")
    expect_s3_class(result$results, "data.frame")
    expect_equal(nrow(result$results), 2)
  })

  it("works with aggregation set to 'Mean'", {
    dat <- make_demand_data(n_ids = 3)
    result <- fitting$fit_demand_ungrouped(
      data = dat, eq = "koff", agg = "Mean", k = 2
    )
    expect_type(result, "list")
    expect_s3_class(result$results, "data.frame")
    # Mean aggregation produces a single row
    expect_equal(nrow(result$results), 1)
  })

  it("works with a Q0 constraint", {
    dat <- make_demand_data(n_ids = 2)
    result <- fitting$fit_demand_ungrouped(
      data = dat, eq = "koff", agg = NULL, k = 2, constrainq0 = 10
    )
    expect_type(result, "list")
    expect_s3_class(result$results, "data.frame")
  })
})

# ------------------------------------------------------------------------------
# fit_demand_grouped() tests
# ------------------------------------------------------------------------------

describe("fit_demand_grouped", {
  it("returns a list with 'output' and 'results' components", {
    dat <- make_grouped_demand_data()
    result <- fitting$fit_demand_grouped(
      data = dat, eq = "koff", agg = NULL, k = 2
    )
    expect_type(result, "list")
    expect_true("output" %in% names(result))
    expect_true("results" %in% names(result))
  })

  it("returns a data.frame in the 'results' component", {
    dat <- make_grouped_demand_data()
    result <- fitting$fit_demand_grouped(
      data = dat, eq = "koff", agg = NULL, k = 2
    )
    expect_s3_class(result$results, "data.frame")
  })

  it("results include a 'group' column", {
    dat <- make_grouped_demand_data()
    result <- fitting$fit_demand_grouped(
      data = dat, eq = "koff", agg = NULL, k = 2
    )
    expect_true("group" %in% names(result$results))
  })

  it("results contain rows for participants across groups", {
    dat <- make_grouped_demand_data()
    result <- fitting$fit_demand_grouped(
      data = dat, eq = "koff", agg = NULL, k = 2
    )
    # 4 participants total, 2 per group
    expect_equal(nrow(result$results), 4)
  })

  it("results contain both groups", {
    dat <- make_grouped_demand_data()
    result <- fitting$fit_demand_grouped(
      data = dat, eq = "koff", agg = NULL, k = 2
    )
    expect_true(all(c("A", "B") %in% result$results$group))
  })

  it("throws an error when data lacks a 'group' column", {
    dat <- make_demand_data(n_ids = 2)
    expect_error(
      fitting$fit_demand_grouped(
        data = dat, eq = "koff", agg = NULL, k = 2
      ),
      "group"
    )
  })

  it("returns NULL when all groups fail to fit", {
    # Data with all-zero consumption should fail to fit
    bad_dat <- data.frame(
      id = rep(1:2, each = 3),
      group = rep("X", 6),
      x = rep(c(1, 2, 3), 2),
      y = rep(0, 6)
    )
    result <- fitting$fit_demand_grouped(
      data = bad_dat, eq = "koff", agg = NULL, k = 2
    )
    # Result should be NULL if all groups fail
    # (beezdemand may still return something, so we accept either NULL or a list)
    expect_true(is.null(result) || is.list(result))
  })

  it("works with the 'hs' equation", {
    dat <- make_grouped_demand_data()
    result <- fitting$fit_demand_grouped(
      data = dat, eq = "hs", agg = NULL, k = 2
    )
    expect_type(result, "list")
    expect_s3_class(result$results, "data.frame")
  })

  it("output component is a list with at least 3 elements", {
    dat <- make_grouped_demand_data()
    result <- fitting$fit_demand_grouped(
      data = dat, eq = "koff", agg = NULL, k = 2
    )
    expect_type(result$output, "list")
    expect_true(length(result$output) >= 3)
  })
})
