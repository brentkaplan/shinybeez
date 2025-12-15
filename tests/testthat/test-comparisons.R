# Tests for comparisons module

box::use(
  app / logic / mixed_effects / comparisons
)

# Test build_specs_string
describe("build_specs_string", {
  it("returns NULL for NULL main_factor", {
    result <- comparisons$build_specs_string(NULL, NULL, c("A", "B"))
    expect_null(result)
  })

  it("returns NULL for empty main_factor", {
    result <- comparisons$build_specs_string("", NULL, c("A", "B"))
    expect_null(result)
  })

  it("returns main_factor alone when no by_factor", {
    result <- comparisons$build_specs_string("A", NULL, c("A", "B"))
    expect_equal(result, "A")
  })

  it("returns main_factor alone when by_factor is empty", {
    result <- comparisons$build_specs_string("A", "", c("A", "B"))
    expect_equal(result, "A")
  })

  it("returns interaction when by_factor is valid", {
    result <- comparisons$build_specs_string("A", "B", c("A", "B"))
    expect_equal(result, "A * B")
  })

  it("ignores by_factor if not in model_factors", {
    result <- comparisons$build_specs_string("A", "C", c("A", "B"))
    expect_equal(result, "A")
  })
})

# Test get_contrast_by_arg
describe("get_contrast_by_arg", {
  it("returns NULL for NULL by_factor", {
    result <- comparisons$get_contrast_by_arg(NULL, c("A", "B"))
    expect_null(result)
  })

  it("returns NULL for empty by_factor", {
    result <- comparisons$get_contrast_by_arg("", c("A", "B"))
    expect_null(result)
  })

  it("returns by_factor if valid", {
    result <- comparisons$get_contrast_by_arg("B", c("A", "B"))
    expect_equal(result, "B")
  })

  it("returns NULL if by_factor not in model_factors", {
    result <- comparisons$get_contrast_by_arg("C", c("A", "B"))
    expect_null(result)
  })
})

# Test is_valid_comparison_factor
describe("is_valid_comparison_factor", {
  it("returns FALSE for NULL", {
    result <- comparisons$is_valid_comparison_factor(NULL, c("A", "B"))
    expect_false(result)
  })

  it("returns FALSE for empty string", {
    result <- comparisons$is_valid_comparison_factor("", c("A", "B"))
    expect_false(result)
  })

  it("returns FALSE for invalid factor", {
    result <- comparisons$is_valid_comparison_factor("C", c("A", "B"))
    expect_false(result)
  })

  it("returns TRUE for valid factor", {
    result <- comparisons$is_valid_comparison_factor("A", c("A", "B"))
    expect_true(result)
  })
})

# Test get_comparison_data
describe("get_comparison_data", {
  it("returns NULL for NULL comparison_result", {
    result <- comparisons$get_comparison_data(NULL, "ratio")
    expect_null(result)
  })

  it("returns contrasts_ratio for ratio display_type", {
    comp_result <- list(
      contrasts_ratio = data.frame(x = 1),
      contrasts_log10 = data.frame(y = 2)
    )
    result <- comparisons$get_comparison_data(comp_result, "ratio")
    expect_equal(result, data.frame(x = 1))
  })

  it("returns contrasts_log10 for log10 display_type", {
    comp_result <- list(
      contrasts_ratio = data.frame(x = 1),
      contrasts_log10 = data.frame(y = 2)
    )
    result <- comparisons$get_comparison_data(comp_result, "log10")
    expect_equal(result, data.frame(y = 2))
  })
})

# Test build_comparison_caption
describe("build_comparison_caption", {
  it("builds ratio caption for Q0", {
    result <- comparisons$build_comparison_caption("Q0", "ratio")
    expect_equal(result, "Pairwise Comparisons for Q0 (Natural Scale Ratios)")
  })

  it("builds log10 caption for Q0", {
    result <- comparisons$build_comparison_caption("Q0", "log10")
    expect_equal(result, "Pairwise Comparisons for Q0 (log10 difference)")
  })

  it("builds ratio caption for alpha", {
    result <- comparisons$build_comparison_caption("alpha", "ratio")
    expect_equal(
      result,
      "Pairwise Comparisons for alpha (Natural Scale Ratios)"
    )
  })
})

# Test is_empty_comparison
describe("is_empty_comparison", {
  it("returns TRUE for NULL", {
    result <- comparisons$is_empty_comparison(NULL)
    expect_true(result)
  })

  it("returns TRUE for non-data.frame", {
    result <- comparisons$is_empty_comparison(list(a = 1))
    expect_true(result)
  })

  it("returns TRUE for empty data.frame", {
    result <- comparisons$is_empty_comparison(data.frame())
    expect_true(result)
  })

  it("returns FALSE for valid data.frame with rows", {
    result <- comparisons$is_empty_comparison(data.frame(x = 1))
    expect_false(result)
  })
})

# Test build_empty_comparison_message
describe("build_empty_comparison_message", {
  it("builds message for Q0", {
    result <- comparisons$build_empty_comparison_message("Q0")
    expect_match(result, "No Q0 comparisons available")
  })

  it("builds message for alpha", {
    result <- comparisons$build_empty_comparison_message("alpha")
    expect_match(result, "No alpha comparisons available")
  })
})

# Test get_other_factors
describe("get_other_factors", {
  it("returns empty for NULL model_factors", {
    result <- comparisons$get_other_factors(NULL, "A")
    expect_equal(result, character(0))
  })

  it("returns empty for single factor model", {
    result <- comparisons$get_other_factors("A", "A")
    expect_equal(result, character(0))
  })

  it("returns other factors excluding main", {
    result <- comparisons$get_other_factors(c("A", "B", "C"), "A")
    expect_equal(result, c("B", "C"))
  })
})

# Test build_specs_formula
describe("build_specs_formula", {
  it("returns NULL for NULL specs_string", {
    result <- comparisons$build_specs_formula(NULL)
    expect_null(result)
  })

  it("returns NULL for empty specs_string", {
    result <- comparisons$build_specs_formula("")
    expect_null(result)
  })

  it("returns formula for valid specs_string", {
    result <- comparisons$build_specs_formula("A")
    expect_s3_class(result, "formula")
    expect_equal(as.character(result)[2], "A")
  })

  it("returns interaction formula", {
    result <- comparisons$build_specs_formula("A * B")
    expect_s3_class(result, "formula")
    expect_equal(as.character(result)[2], "A * B")
  })
})

# Test can_compare
describe("can_compare", {
  it("returns FALSE for NULL", {
    result <- comparisons$can_compare(NULL)
    expect_false(result)
  })

  it("returns FALSE for empty vector", {
    result <- comparisons$can_compare(character(0))
    expect_false(result)
  })

  it("returns TRUE for non-empty vector", {
    result <- comparisons$can_compare(c("A"))
    expect_true(result)
  })
})

# Test round_comparison_data
describe("round_comparison_data", {
  it("returns NULL for NULL input", {
    result <- comparisons$round_comparison_data(NULL)
    expect_null(result)
  })

  it("returns non-data.frame unchanged", {
    result <- comparisons$round_comparison_data(list(a = 1))
    expect_equal(result, list(a = 1))
  })

  it("rounds numeric columns", {
    df <- data.frame(x = 1.23456789, y = "text")
    result <- comparisons$round_comparison_data(df, digits = 2)
    expect_equal(result$x, 1.23)
    expect_equal(result$y, "text")
  })

  it("uses default 4 digits", {
    df <- data.frame(x = 1.23456789)
    result <- comparisons$round_comparison_data(df)
    expect_equal(result$x, 1.2346)
  })
})
