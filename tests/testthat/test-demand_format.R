box::use(
  app/logic/validate,
)

# Regression tests for the demand wide/long format classifier.
#
# Two defects are covered:
#
#  1. The FORMAT FLIP (live on develop, signatures 56da2d58 / efb696cf).
#     Format was inferred from a row-content heuristic (ids unique => wide), but
#     check_data() judged the frame BEFORE remove_na_rows() while rename_cols() ran
#     on the stored POST-removal frame (file_input.R:95 vs :111 vs :135). Dropping NA
#     rows can make duplicated ids unique, flipping a long file into the wide branch,
#     where parse_number() turns the literal headers x/y into NA and the frame ends up
#     with columns named "NA". beezdemand then aborts with
#     `Cannot parse prices from column names: NA`.
#
#  2. The TAUTOLOGICAL HEADER GUARD (still live on main / production).
#     `all(sapply(parse_number(...), is.numeric))` is always TRUE, because
#     is.numeric(NA_real_) is TRUE. Fixed on develop by d3f2f93; these tests pin it so
#     it cannot regress.
#
# The upload sequence under test is the real one from app/view/file_input.R:
# validate, then drop NA rows, then store, then rename headers, then reshape.

# Reproduce app/view/file_input.R's ordering EXACTLY - including the sufficiency
# gate at :138 - so the tests exercise the real upload path, not a subset of it.
upload_sequence <- function(dat) {
  chk <- validate$check_data(dat, type = "demand")
  if (is.character(chk)) {
    return(list(rejected = TRUE, message = chk))
  }
  stored <- validate$remove_na_rows(dat)$data
  enough <- validate$check_demand_sufficiency(stored)
  if (is.character(enough)) {
    return(list(rejected = TRUE, message = enough))
  }
  reshaped <- validate$rename_cols(stored) |>
    validate$reshape_data(dat = _, type = "demand")
  list(rejected = FALSE, data = reshaped)
}

describe("demand format classification (the NA-removal flip)", {
  it("rejects honestly when NA-row removal leaves one point per id", {
    # The flip needed EVERY id to survive with exactly one row - that is the only
    # way `length(unique(id)) == length(id)` becomes true. Such data cannot fit a
    # curve at all, so the honest outcome is a named rejection, NOT a beezdemand
    # abort about column names ("Cannot parse prices from column names: NA").
    dat <- data.frame(
      id = c("s1", "s1", "s2", "s2"),
      x  = c(1, 2, 1, 2),
      y  = c(10, NA, 8, NA)
    )

    res <- upload_sequence(dat)

    expect_true(res$rejected)
    expect_match(res$message, "price point", ignore.case = TRUE)
    expect_match(res$message, "s1", fixed = TRUE)
    # the defect signature must NOT be what the user sees
    expect_false(grepl("Cannot parse prices", res$message, fixed = TRUE))
    expect_false(grepl("scale_id", res$message, fixed = TRUE))
  })

  it("keeps long data long when ids survive NA removal with enough points", {
    # 3 price points per id, one NA each -> 2 survive. Ids stay duplicated here,
    # but the frame must be classified long on its NAMES, not on that fact.
    dat <- data.frame(
      id = c("s1", "s1", "s1", "s2", "s2", "s2"),
      x  = c(1, 2, 3, 1, 2, 3),
      y  = c(10, 6, NA, 8, 5, NA)
    )

    res <- upload_sequence(dat)

    expect_false(res$rejected)
    expect_false(any(names(res$data) == "NA"))
    expect_true(all(c("id", "x", "y") %in% names(res$data)))
  })

  it("keeps grouped long data long through the full upload sequence", {
    dat <- data.frame(
      id    = c("s1", "s1", "s1", "s2", "s2", "s2"),
      group = c("a", "a", "a", "b", "b", "b"),
      x     = c(1, 2, 3, 1, 2, 3),
      y     = c(10, 6, NA, 8, 5, NA)
    )

    res <- upload_sequence(dat)

    expect_false(res$rejected)
    expect_false(any(names(res$data) == "NA"))
    expect_true(all(c("id", "group", "x", "y") %in% names(res$data)))
  })

  it("classifies long data as long even when ids are already unique", {
    # Format is a property of the column names, not of the rows.
    dat <- data.frame(id = c("s1", "s2"), x = c(1, 2), y = c(10, 8))

    expect_equal(validate$demand_format(dat), "long")
    expect_equal(
      validate$demand_format(
        data.frame(id = "s1", group = "a", x = 1, y = 10)
      ),
      "long"
    )
  })

  it("classifies price-headed data as wide regardless of id duplication", {
    dat <- data.frame(
      id = c("s1", "s2"),
      `0.01` = c(100, 90),
      `0.1` = c(80, 70),
      check.names = FALSE
    )
    expect_equal(validate$demand_format(dat), "wide")
  })

  it("rename_cols never produces an \"NA\" column name", {
    long <- data.frame(id = c("s1", "s2"), x = c(1, 2), y = c(10, 8))
    out <- validate$rename_cols(long)
    expect_false(any(names(out) == "NA"))
    expect_equal(names(out), c("id", "x", "y"))
  })
})

describe("wide price-header validation", {
  it("names the offending headers instead of a vague message", {
    dat <- data.frame(
      id = c("s1", "s2"),
      price_low = c(100, 90),
      price_high = c(80, 70),
      check.names = FALSE
    )

    res <- validate$check_data(dat, type = "demand")

    expect_type(res, "character")
    expect_match(res, "not numeric", ignore.case = TRUE)
    expect_match(res, "price_low", fixed = TRUE)
    expect_match(res, "price_high", fixed = TRUE)
  })

  it("names the offending headers for grouped wide data", {
    dat <- data.frame(
      id = c("s1", "s2"),
      group = c("a", "b"),
      abc = c(100, 90),
      `0.5` = c(80, 70),
      check.names = FALSE
    )

    res <- validate$check_data(dat, type = "demand")

    expect_type(res, "character")
    expect_match(res, "abc", fixed = TRUE)
    # only the bad one is named; the parseable header is not
    expect_false(grepl("0.5", res, fixed = TRUE))
  })

  it("rejects headers that parse to duplicate prices", {
    # "$1" and "1.00" both parse to 1 -> today they silently become two "1" columns
    dat <- data.frame(
      id = c("s1", "s2"),
      `$1` = c(100, 90),
      `1.00` = c(80, 70),
      check.names = FALSE
    )

    res <- validate$check_data(dat, type = "demand")

    expect_type(res, "character")
    expect_match(res, "duplicate", ignore.case = TRUE)
  })

  it("does not emit a parse warning when rejecting a bad header", {
    dat <- data.frame(
      id = c("s1", "s2"),
      price_low = c(100, 90),
      price_high = c(80, 70),
      check.names = FALSE
    )
    expect_no_warning(validate$check_data(dat, type = "demand"))
  })
})

describe("exported functions cannot be bypassed", {
  it("reshape_data rejects a bad wide frame called directly, without check_data", {
    # rename_cols() and reshape_data() are exported; guarding only check_data()
    # leaves this path reaching beezdemand::pivot_demand_data() with an NA column.
    dat <- data.frame(
      id = c("s1", "s2"),
      price_low = c(100, 90),
      price_high = c(80, 70),
      check.names = FALSE
    )

    expect_error(
      validate$reshape_data(dat, type = "demand"),
      "not numeric",
      ignore.case = TRUE
    )
  })

  it("rename_cols aborts on an unparseable wide header", {
    dat <- data.frame(
      id = c("s1", "s2"),
      price_low = c(100, 90),
      check.names = FALSE
    )

    expect_error(validate$rename_cols(dat), "price_low", fixed = TRUE)
  })
})

describe("post-NA-removal sufficiency", {
  it("rejects an id left with fewer than two distinct price points", {
    # One observation per participant cannot support an individual demand curve:
    # the systematicity code divides by nrow(adf) - 1 and by a zero price range.
    dat <- data.frame(
      id = c("s1", "s1", "s2", "s2"),
      x  = c(1, 2, 1, 2),
      y  = c(10, NA, 8, NA)
    )
    stored <- validate$remove_na_rows(dat)$data

    res <- validate$check_demand_sufficiency(stored)

    expect_type(res, "character")
    expect_match(res, "s1", fixed = TRUE)
    expect_match(res, "price point", ignore.case = TRUE)
  })

  it("accepts ids that retain two or more distinct price points", {
    dat <- data.frame(
      id = c("s1", "s1", "s2", "s2"),
      x  = c(1, 2, 1, 2),
      y  = c(10, 5, 8, 4)
    )
    expect_true(validate$check_demand_sufficiency(dat))
  })

  it("rejects a wide frame with only one price column", {
    # Reshapes to exactly one observation per id, which cannot fit a curve.
    dat <- data.frame(
      id = c("s1", "s2"),
      `0.01` = c(100, 90),
      check.names = FALSE
    )

    res <- validate$check_demand_sufficiency(dat)

    expect_type(res, "character")
    expect_match(res, "price", ignore.case = TRUE)
  })

  it("accepts a wide frame with two or more price columns", {
    dat <- data.frame(
      id = c("s1", "s2"),
      `0.01` = c(100, 90),
      `0.1` = c(80, 70),
      check.names = FALSE
    )
    expect_true(validate$check_demand_sufficiency(dat))
  })

  it("counts distinct PARSED prices, not distinct raw strings", {
    # "$1" and "1.00" are two raw strings but one price - retype_data() parses
    # both to 1, so this id really has a single price point.
    dat <- data.frame(
      id = c("s1", "s1"),
      x  = c("$1", "1.00"),
      y  = c(10, 8),
      stringsAsFactors = FALSE
    )

    res <- validate$check_demand_sufficiency(dat)

    expect_type(res, "character")
    expect_match(res, "s1", fixed = TRUE)
  })
})

describe("malformed long-like schemas", {
  it("rejects a reordered long schema rather than treating it as wide", {
    dat <- data.frame(id = c("s1", "s1"), y = c(10, 8), x = c(1, 2))

    res <- validate$check_data(dat, type = "demand")

    expect_type(res, "character")
  })
})
