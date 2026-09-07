# Test file tests/testthat/test-validate_header_grammar.R
#
# Two bugs found 2026-09-06 while planning the wide-to-long mapper:
#  1. readr::parse_number() accepts embedded digits, so a Qualtrics item export
#     `id, APT_1, APT_2, ...` PASSED demand validation with prices 1, 2, 3 invented
#     from the item index. A header is a price only if the WHOLE header is a number.
#  2. The bundled template_discounting_wide.csv (`id,1,7,30,...`) was REJECTED by
#     check_discounting_data(), which demanded exactly id,x,y whenever `id` was present.

box::use(
  testthat[...],
  vroom,
)

box::use(
  app / logic / validate,
)

normalise <- function(dat) {
  colnames(dat) <- trimws(tolower(colnames(dat)))
  dat
}

describe("parse_header_number", {
  it("parses whole-header numbers with optional currency and whitespace", {
    expect_equal(
      validate$parse_header_number(c("0", "$0.50 ", " 1", "£5", "100")),
      c(0, 0.5, 1, 5, 100)
    )
  })

  it("returns NA for headers that merely contain digits", {
    expect_true(all(is.na(
      validate$parse_header_number(c("apt_1", "q5_3", "price_0.5", "x", "1 day", ""))
    )))
  })
})

describe("demand wide headers (bug 1)", {
  it("rejects id + APT_n item headers instead of inventing prices", {
    apt <- data.frame(id = 1:3, apt_1 = c(5, 4, 3), apt_2 = c(4, 3, 2), apt_3 = c(1, 1, 0))
    chk <- validate$check_data(apt, type = "demand")
    expect_type(chk, "character")
    expect_match(chk, "Could not parse these price headers", fixed = TRUE)
    expect_match(chk, "apt_1", fixed = TRUE)
  })

  it("still accepts every bundled demand template", {
    for (f in c(
      "template_demand_wide.csv", "template_demand_wide_onegroup.csv",
      "template_demand_long.csv", "template_demand_long_onegroup.csv"
    )) {
      dat <- normalise(vroom$vroom(
        file.path(find_project_root(), "app/static/data/templates", f), show_col_types = FALSE
      ))
      expect_true(isTRUE(validate$check_data(dat, type = "demand")), info = f)
    }
  })

  it("rename_cols() uses the anchored grammar", {
    dat <- data.frame(id = 1:2, `$0.50` = c(1, 2), `1` = c(0, 1), check.names = FALSE)
    expect_equal(colnames(validate$rename_cols(dat)), c("id", "0.5", "1"))
  })
})

describe("discounting wide indifference points (bug 2)", {
  template <- normalise(vroom$vroom(
    file.path(find_project_root(), "app/static/data/templates/template_discounting_wide.csv"),
    show_col_types = FALSE
  ))

  it("accepts the bundled wide template", {
    expect_true(isTRUE(validate$check_data(template, type = "discounting")))
  })

  it("reshapes the template to id, x, y through prepare_discounting_data()", {
    long <- validate$prepare_discounting_data(template)
    expect_equal(colnames(long), c("id", "x", "y"))
    expect_true(is.numeric(long$x))
    expect_equal(sort(unique(long$x)), c(1, 7, 30, 90, 180, 365))
    expect_equal(nrow(long), nrow(template) * 6)
  })

  it("accepts exactly two delay columns", {
    dat <- data.frame(id = c("a", "b"), `7` = c(0.9, 0.8), `30` = c(0.5, 0.4), check.names = FALSE)
    expect_true(isTRUE(validate$check_data(dat, type = "discounting")))
    expect_equal(nrow(validate$prepare_discounting_data(dat)), 4)
  })

  it("rejects id-first files whose delay headers are not numbers", {
    dat <- data.frame(id = 1:2, d_7 = c(0.9, 0.8), d_30 = c(0.5, 0.4))
    chk <- validate$check_data(dat, type = "discounting")
    expect_match(chk, "one numeric delay per column header", fixed = TRUE)
    expect_match(chk, "d_7", fixed = TRUE)
  })

  it("rejects wide files with duplicated ids", {
    dat <- data.frame(id = c(1, 1), `7` = c(0.9, 0.8), `30` = c(0.5, 0.4), check.names = FALSE)
    expect_match(validate$check_data(dat, type = "discounting"), "one row per id", fixed = TRUE)
  })

  it("keeps the exact-three-column message for a long frame with extra columns", {
    dat <- data.frame(id = 1:2, x = c(1, 7), y = c(0.9, 0.5), z = 1:2)
    expect_match(
      validate$check_data(dat, type = "discounting"),
      "Indifference point data must have exactly three columns: id, x, y", fixed = TRUE
    )
  })
})
