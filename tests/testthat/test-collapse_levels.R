# Tests for app/logic/mixed_effects/collapse_levels.R

box::use(
  testthat[...],
)

box::use(
  app / logic / mixed_effects / collapse_levels
)

# ------------------------------------------------------------------------------
# build_collapse_list() tests
# ------------------------------------------------------------------------------

describe("build_collapse_list", {
  it("builds list with both groups populated", {
    result <- collapse_levels$build_collapse_list(
      g1_name = "Low",
      g1_levels = c("A", "B"),
      g2_name = "High",
      g2_levels = c("C", "D")
    )
    expect_type(result, "list")
    expect_equal(names(result), c("Low", "High"))
    expect_equal(result$Low, c("A", "B"))
    expect_equal(result$High, c("C", "D"))
  })

  it("builds list with only group 1", {
    result <- collapse_levels$build_collapse_list(
      g1_name = "Combined",
      g1_levels = c("A", "B", "C"),
      g2_name = "",
      g2_levels = character(0)
    )
    expect_type(result, "list")
    expect_equal(names(result), "Combined")
    expect_equal(result$Combined, c("A", "B", "C"))
  })

  it("builds list with only group 2", {
    result <- collapse_levels$build_collapse_list(
      g1_name = "",
      g1_levels = character(0),
      g2_name = "Other",
      g2_levels = c("X", "Y")
    )
    expect_type(result, "list")
    expect_equal(names(result), "Other")
  })

  it("returns NULL when both groups empty", {
    result <- collapse_levels$build_collapse_list(
      g1_name = "",
      g1_levels = character(0),
      g2_name = "",
      g2_levels = character(0)
    )
    expect_null(result)
  })

  it("returns NULL when group names are NULL", {
    result <- collapse_levels$build_collapse_list(
      g1_name = NULL,
      g1_levels = c("A", "B"),
      g2_name = NULL,
      g2_levels = c("C", "D")
    )
    expect_null(result)
  })

  it("ignores group with empty name but has levels", {
    result <- collapse_levels$build_collapse_list(
      g1_name = "Valid",
      g1_levels = c("A"),
      g2_name = "",
      g2_levels = c("B", "C") # Should be ignored due to empty name
    )
    expect_equal(names(result), "Valid")
    expect_length(result, 1)
  })
})

# ------------------------------------------------------------------------------
# find_overlap() tests
# ------------------------------------------------------------------------------

describe("find_overlap", {
  it("returns overlapping levels", {
    result <- collapse_levels$find_overlap(
      g1_levels = c("A", "B", "C"),
      g2_levels = c("B", "C", "D")
    )
    expect_equal(sort(result), c("B", "C"))
  })

  it("returns NULL when no overlap", {
    result <- collapse_levels$find_overlap(
      g1_levels = c("A", "B"),
      g2_levels = c("C", "D")
    )
    expect_null(result)
  })

  it("returns NULL when g1 is empty", {
    result <- collapse_levels$find_overlap(
      g1_levels = character(0),
      g2_levels = c("A", "B")
    )
    expect_null(result)
  })

  it("returns NULL when g2 is empty", {
    result <- collapse_levels$find_overlap(
      g1_levels = c("A", "B"),
      g2_levels = character(0)
    )
    expect_null(result)
  })

  it("handles single overlapping level", {
    result <- collapse_levels$find_overlap(
      g1_levels = c("A", "B", "C"),
      g2_levels = c("C", "D", "E")
    )
    expect_equal(result, "C")
  })
})

# ------------------------------------------------------------------------------
# validate_no_overlap() tests
# ------------------------------------------------------------------------------

describe("validate_no_overlap", {
  it("returns valid=TRUE when no overlap", {
    result <- collapse_levels$validate_no_overlap(
      g1_levels = c("A", "B"),
      g2_levels = c("C", "D")
    )
    expect_true(result$valid)
    expect_null(result$overlap)
    expect_null(result$error_message)
  })

  it("returns valid=FALSE when overlap exists", {
    result <- collapse_levels$validate_no_overlap(
      g1_levels = c("A", "B"),
      g2_levels = c("B", "C")
    )
    expect_false(result$valid)
    expect_equal(result$overlap, "B")
    expect_type(result$error_message, "character")
  })

  it("includes factor_name and param_label in error message", {
    result <- collapse_levels$validate_no_overlap(
      g1_levels = c("A", "B"),
      g2_levels = c("B", "C"),
      factor_name = "Treatment",
      param_label = "Q0"
    )
    expect_match(result$error_message, "Treatment")
    expect_match(result$error_message, "Q0")
  })

  it("returns valid=TRUE for empty groups", {
    result <- collapse_levels$validate_no_overlap(
      g1_levels = character(0),
      g2_levels = c("A", "B")
    )
    expect_true(result$valid)
  })
})

# ------------------------------------------------------------------------------
# process_param_collapse() tests
# ------------------------------------------------------------------------------

describe("process_param_collapse", {
  it("returns NULL collapse when not enabled", {
    result <- collapse_levels$process_param_collapse(
      collapse_enabled = FALSE,
      g1_name = "Low",
      g1_levels = c("A", "B"),
      g2_name = "High",
      g2_levels = c("C", "D")
    )
    expect_null(result$collapse)
    expect_null(result$error)
  })

  it("builds collapse when enabled with valid groups", {
    result <- collapse_levels$process_param_collapse(
      collapse_enabled = TRUE,
      g1_name = "Low",
      g1_levels = c("A", "B"),
      g2_name = "High",
      g2_levels = c("C", "D")
    )
    expect_type(result$collapse, "list")
    expect_equal(names(result$collapse), c("Low", "High"))
    expect_null(result$error)
  })

  it("returns error when overlap detected", {
    result <- collapse_levels$process_param_collapse(
      collapse_enabled = TRUE,
      g1_name = "Low",
      g1_levels = c("A", "B"),
      g2_name = "High",
      g2_levels = c("B", "C"), # B overlaps
      factor_name = "Treatment",
      param_label = "Alpha"
    )
    expect_null(result$collapse)
    expect_false(result$error$valid)
    expect_equal(result$error$overlap, "B")
  })

  it("handles NULL collapse_enabled as FALSE", {
    result <- collapse_levels$process_param_collapse(
      collapse_enabled = NULL,
      g1_name = "Low",
      g1_levels = c("A", "B"),
      g2_name = "High",
      g2_levels = c("C", "D")
    )
    expect_null(result$collapse)
    expect_null(result$error)
  })
})

# ------------------------------------------------------------------------------
# build_collapse_structure() tests
# ------------------------------------------------------------------------------

describe("build_collapse_structure", {
  it("builds structure with single factor Q0 collapse", {
    f1_config <- list(
      name = "Treatment",
      q0 = list(collapse = list(Low = c("A", "B"), High = c("C", "D"))),
      alpha = list(collapse = NULL)
    )
    result <- collapse_levels$build_collapse_structure(
      factor1_config = f1_config
    )

    expect_type(result, "list")
    expect_true("Q0" %in% names(result))
    expect_equal(names(result$Q0), "Treatment")
  })

  it("builds structure with single factor both params", {
    f1_config <- list(
      name = "Treatment",
      q0 = list(collapse = list(Low = c("A", "B"))),
      alpha = list(collapse = list(High = c("C", "D")))
    )
    result <- collapse_levels$build_collapse_structure(
      factor1_config = f1_config
    )

    expect_true("Q0" %in% names(result))
    expect_true("alpha" %in% names(result))
  })

  it("builds structure with two factors", {
    f1_config <- list(
      name = "Treatment",
      q0 = list(collapse = list(Low = c("A", "B"))),
      alpha = list(collapse = NULL)
    )
    f2_config <- list(
      name = "Session",
      q0 = list(collapse = NULL),
      alpha = list(collapse = list(Early = c("1", "2")))
    )
    result <- collapse_levels$build_collapse_structure(
      factor1_config = f1_config,
      factor2_config = f2_config
    )

    expect_true("Q0" %in% names(result))
    expect_true("alpha" %in% names(result))
    expect_equal(names(result$Q0), "Treatment")
    expect_equal(names(result$alpha), "Session")
  })

  it("returns NULL when no collapse specified", {
    f1_config <- list(
      name = "Treatment",
      q0 = list(collapse = NULL),
      alpha = list(collapse = NULL)
    )
    result <- collapse_levels$build_collapse_structure(
      factor1_config = f1_config
    )
    expect_null(result)
  })

  it("returns NULL when config is NULL", {
    result <- collapse_levels$build_collapse_structure(
      factor1_config = NULL,
      factor2_config = NULL
    )
    expect_null(result)
  })
})
