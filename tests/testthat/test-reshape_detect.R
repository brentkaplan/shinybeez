box::use(
  testthat[...],
  vroom,
)

box::use(
  app / logic / reshape / detect,
)

fixture <- function(name) {
  dat <- vroom$vroom(testthat::test_path("fixtures", name), show_col_types = FALSE)
  colnames(dat) <- trimws(tolower(colnames(dat)))   # file_input.R normalises before validation
  dat
}

describe("guess_id_col", {
  it("prefers a recognised id name over position", {
    expect_equal(detect$guess_id_col(fixture("wide-qualtrics-apt.csv")), "responseid")
    expect_equal(detect$guess_id_col(fixture("wide-id-not-first.csv")), "subj")
    expect_equal(detect$guess_id_col(fixture("wide-price-suffix.csv")), "participant")
  })
  it("falls back to the first all-unique column, else NA", {
    dat <- data.frame(a = c(1, 1), b = c("u", "v"), c = c(3, 4))
    expect_equal(detect$guess_id_col(dat), "b")
    expect_true(is.na(detect$guess_id_col(data.frame(a = c(1, 1), b = c(2, 2)))))
  })
})

describe("split_suffix", {
  it("splits prefix and trailing number", {
    out <- detect$split_suffix(c("apt_1", "q5_3", "price_0.5", "x", "d 30"))
    expect_equal(out$prefix, c("apt", "q5", "price", NA, "d"))
    expect_equal(out$suffix, c(1, 3, 0.5, NA, 30))
  })
})

describe("numeric_share", {
  it("counts whole-cell numbers, with currency, and ignores NA", {
    expect_equal(detect$numeric_share(c("$5", "6", NA, " 0.5 ")), 1)
    expect_equal(detect$numeric_share(c(1, 2, NA)), 1)
  })
  it("does not count cells that merely contain digits", {
    expect_equal(detect$numeric_share(c("2026-01-01", "R_1", "6 drinks")), 0)
    expect_equal(detect$numeric_share(character(0)), 0)
  })
})

describe("x_from_names", {
  it("uses whole-header numbers when every header is one", {
    expect_equal(detect$x_from_names(c("$0", "0.5", "1")), c(0, 0.5, 1))
  })
  it("falls back to suffix numbers", {
    expect_equal(detect$x_from_names(c("price_0", "price_0.5")), c(0, 0.5))
    expect_equal(detect$x_from_names(c("apt_1", "total")), c(1, NA))
  })
})

describe("header_x_available", {
  it("is FALSE for an item-index run: the trailing number is a position, not a price", {
    expect_false(detect$header_x_available(paste0("apt_", 1:5)))
  })
  it("is TRUE when the trailing numbers are not a sequential index", {
    expect_true(detect$header_x_available(c("price_0", "price_0.5", "price_1", "price_5", "price_10")))
  })
  it("is FALSE for a partial index run: q5_3..q5_10 are item positions too", {
    expect_false(detect$header_x_available(paste0("q5_", 3:10)))
  })
  it("is TRUE for non-consecutive trailing numbers such as delays", {
    expect_true(detect$header_x_available(c("d_7", "d_30", "d_90")))
  })
  it("checks every series on its own: two index runs flattened are not one run", {
    expect_false(detect$series_header_x_available(list(paste0("alc_", 1:3), paste0("cig_", 1:3))))
    expect_true(detect$series_header_x_available(list(c("alc_0", "alc_5"), c("cig_0", "cig_5"))))
    expect_false(detect$series_header_x_available(list(c("alc_0", "alc_5"), paste0("cig_", 1:3))))
    expect_false(detect$series_header_x_available(list()))
  })
  it("is TRUE for whole-header numbers even when they happen to be 1..n", {
    expect_true(detect$header_x_available(c("1", "2", "3")))
  })
  it("is FALSE with no columns", {
    expect_false(detect$header_x_available(character(0)))
  })
})

describe("cluster_series_columns", {
  it("treats consecutive 1..n suffixes as item indices (manual prices)", {
    cl <- detect$cluster_series_columns(fixture("wide-qualtrics-apt.csv"), exclude = "responseid")
    expect_length(cl, 1)
    expect_equal(cl[[1]]$cols, paste0("apt_", 1:5))
    expect_null(cl[[1]]$x)
    expect_equal(cl[[1]]$x_source, "manual")
  })
  it("treats a partial consecutive run (q5_3..q5_10) as item indices too", {
    dat <- as.data.frame(setNames(replicate(8, c(3, 2, 1), simplify = FALSE), paste0("q5_", 3:10)))
    dat$id <- 1:3
    cl <- detect$cluster_series_columns(dat, exclude = "id")
    expect_length(cl, 1)
    expect_null(cl[[1]]$x)
    expect_equal(cl[[1]]$x_source, "manual")
  })
  it("offers non-consecutive suffixes as prices", {
    cl <- detect$cluster_series_columns(fixture("wide-price-suffix.csv"), exclude = "participant")
    expect_equal(cl[[1]]$x, c(0, 0.5, 1, 5, 10))
    expect_equal(cl[[1]]$x_source, "header")
  })
  it("returns one cluster per prefix, largest first", {
    dat <- fixture("wide-two-commodities.csv")
    cl <- detect$cluster_series_columns(dat, exclude = "id")
    expect_equal(vapply(cl, `[[`, character(1), "label"), c("alc", "cig"))
  })
  it("groups whole-number headers into one header cluster", {
    cl <- detect$cluster_series_columns(fixture("wide-id-not-first.csv"), exclude = "subj")
    expect_length(cl, 1)
    expect_equal(cl[[1]]$x, c(0, 0.5, 1, 5))
    expect_equal(cl[[1]]$x_source, "header")
  })
  it("ignores columns whose cells are not mostly numbers", {
    dat <- fixture("wide-qualtrics-apt.csv")
    cl <- detect$cluster_series_columns(dat)
    expect_false("startdate" %in% unlist(lapply(cl, `[[`, "cols")))
  })
  it("does not cluster columns whose cells merely contain digits", {
    dat <- data.frame(
      id = 1:2,
      note_1 = c("2026-01-01", "2026-02-01"),
      note_2 = c("2026-04-01", "2026-05-01"),
      stringsAsFactors = FALSE
    )
    expect_length(detect$cluster_series_columns(dat, exclude = "id"), 0)
  })
})

describe("guess_spec", {
  it("prefills the Qualtrics export with manual prices needed", {
    s <- detect$guess_spec(fixture("wide-qualtrics-apt.csv"), "demand")
    expect_equal(s$id_col, "responseid")
    expect_equal(s$series[[1]]$cols, paste0("apt_", 1:5))
    expect_equal(s$x_source, "manual")
    expect_equal(s$series[[1]]$label, "")
  })
  it("keeps only the first cluster on discounting", {
    s <- detect$guess_spec(fixture("wide-two-commodities.csv"), "discounting")
    expect_length(s$series, 1)
  })
  it("labels several clusters by prefix", {
    s <- detect$guess_spec(fixture("wide-two-commodities.csv"), "demand")
    expect_equal(vapply(s$series, `[[`, character(1), "label"), c("alc", "cig"))
  })
  it("returns an empty series when nothing is detectable", {
    s <- detect$guess_spec(data.frame(a = c("x", "y"), b = c("u", "v")), "demand")
    expect_length(s$series, 1)
    expect_equal(s$series[[1]]$cols, character(0))
  })
})

describe("detect_long", {
  it("finds id/x/y in a long file whose columns are misnamed", {
    out <- detect$detect_long(fixture("long-misnamed.csv"), "demand")
    expect_equal(out$id_col, "subject")
    expect_equal(out$x_col, "price")
    expect_equal(out$y_col, "consumption")
    expect_null(out$group_col)
  })
  it("ignores extra columns, whatever their order, and offers a group column", {
    out <- detect$detect_long(fixture("long-extra-cols.csv"), "demand")
    expect_equal(out$id_col, "id")
    expect_equal(out$x_col, "x")
    expect_equal(out$y_col, "y")
    expect_equal(out$group_col, "site")
  })
  it("never offers a group column on discounting", {
    expect_null(detect$detect_long(fixture("long-extra-cols.csv"), "discounting")$group_col)
  })
  it("reads currency-style x cells", {
    dat <- fixture("long-misnamed.csv")
    dat$price <- paste0("$", dat$price)
    expect_equal(detect$detect_long(dat, "demand")$x_col, "price")
  })
  it("finds the mixed-effects long shape with covariates", {
    out <- detect$detect_long(fixture("long-me-covariates.csv"), "mixed_effects_demand")
    expect_equal(out$id_col, "subject")
    expect_equal(out$x_col, "price")
    expect_equal(out$y_col, "consumption")
    expect_equal(out$group_col, "sex")
  })
  it("finds delay/indifference names on discounting", {
    out <- detect$detect_long(fixture("long-ip-named.csv"), "discounting")
    expect_equal(out$x_col, "delay")
    expect_equal(out$y_col, "indiff")
  })
  it("returns NULL for every wide fixture and template", {
    wide <- c(
      "wide-id-not-first.csv", "wide-ip-delays-named.csv", "wide-me-with-covariates.csv",
      "wide-price-suffix.csv", "wide-qualtrics-apt.csv", "wide-two-commodities.csv",
      "demand-minimal.csv", "demand-minimal-grouped.csv",
      "discounting-five-trial-dd-minimal.csv", "discounting-five-trial-pd-minimal.csv"
    )
    # The MCQ fixtures are deliberately absent: they are genuinely one row per observation,
    # and file_input's is_fixed_schema() blocks them before a mapper request exists.
    for (nm in wide) {
      expect_null(detect$detect_long(fixture(nm), "demand"), info = nm)
    }
    root <- find_project_root()
    for (nm in list.files(file.path(root, "app/static/data/templates"), pattern = "wide.*[.]csv$")) {
      dat <- vroom$vroom(file.path(root, "app/static/data/templates", nm), show_col_types = FALSE)
      colnames(dat) <- trimws(tolower(colnames(dat)))
      expect_null(detect$detect_long(dat, "demand"), info = nm)
    }
  })
  it("returns NULL when there is nothing to reshape", {
    expect_null(detect$detect_long(data.frame(id = c("a", "a"), x = c(1, 2)), "demand"))
    expect_null(detect$detect_long(data.frame(id = "a", x = 1, y = 2), "demand"))
    expect_null(detect$detect_long(data.frame(id = c("a", "b"), x = c(1, 2), y = c(3, 4)), "demand"))
  })
  it("returns NULL when the only repeating column has no second numeric column", {
    dat <- data.frame(
      site = c("north", "north", "south", "south"),
      x = c(1, 2, 1, 2),
      note = c("a", "b", "c", "d"),
      stringsAsFactors = FALSE
    )
    expect_null(detect$detect_long(dat, "demand"))
  })
  it("recognises the app's own long files and templates", {
    expect_equal(detect$detect_long(fixture("demand-minimal-long.csv"), "demand")$id_col, "id")
    expect_equal(detect$detect_long(fixture("discounting-ip-minimal.csv"), "discounting")$x_col, "x")
    root <- find_project_root()
    dat <- vroom$vroom(
      file.path(root, "app/static/data/templates/template_demand_long_onegroup.csv"), show_col_types = FALSE
    )
    colnames(dat) <- trimws(tolower(colnames(dat)))
    out <- detect$detect_long(dat, "demand")
    expect_equal(c(out$id_col, out$x_col, out$y_col, out$group_col), c("id", "x", "y", "group"))
  })
  it("declines a long frame whose x repeats within a participant rather than guessing", {
    # monkey x drug x dose x price: no single column is a grid shared across participants,
    # so there is no honest mapping to offer. Two grouping columns are out of scope.
    expect_null(detect$detect_long(fixture("mixed-effects-minimal.csv"), "mixed_effects_demand"))
  })
  it("needs more than one participant before repetition means anything", {
    one <- data.frame(id = rep("s1", 4), x = c(0, 1, 5, 10), y = c(10, 8, 5, 2), stringsAsFactors = FALSE)
    expect_null(detect$detect_long(one, "demand"))
  })
})

describe("guess_spec layout", {
  it("returns a long spec for a misnamed long file", {
    s <- detect$guess_spec(fixture("long-misnamed.csv"), "demand")
    expect_equal(s$layout, "long")
    expect_equal(s$id_col, "subject")
    expect_equal(s$x_col, "price")
    expect_equal(s$y_col, "consumption")
    expect_length(s$series, 0)
  })
  it("still returns the wide spec for a wide file", {
    s <- detect$guess_spec(fixture("wide-qualtrics-apt.csv"), "demand")
    expect_equal(s$layout, "wide")
    expect_equal(s$series[[1]]$cols, paste0("apt_", 1:5))
  })
  it("guess_spec_long falls back to the first three columns when detection misses", {
    s <- detect$guess_spec_long(fixture("wide-qualtrics-apt.csv"), "demand")
    expect_equal(s$layout, "long")
    expect_equal(c(s$id_col, s$x_col, s$y_col), c("startdate", "responseid", "age"))
  })
  it("guess_spec_wide always returns the wide guess", {
    expect_equal(detect$guess_spec_wide(fixture("long-misnamed.csv"), "demand")$layout, "wide")
  })
})

describe("detect_long, responses that are not responses", {
  it("does not offer a covariate as the response column", {
    # A file whose only real response column is empty loses it to obliterate_empty_cols()
    # before the mapper opens; `age` is numeric and repeats the participant's own value, so
    # it must not be prefilled as consumption.
    dat <- data.frame(
      id = rep(c("a", "b"), each = 3), x = rep(c(1, 2, 3), 2),
      age = rep(c(30, 40), each = 3), stringsAsFactors = FALSE
    )
    expect_null(detect$detect_long(dat, "demand"))
  })
  it("keeps a flat response when its name says it is one", {
    # A non-discounter answers the same value at every delay; every participant being flat
    # is unusual but real, and `indiff` is not a covariate however flat it is.
    dat <- data.frame(
      id = rep(c("a", "b"), each = 3), delay = rep(c(1, 7, 30), 2),
      indiff = c(0.5, 0.5, 0.5, 0.2, 0.2, 0.2), stringsAsFactors = FALSE
    )
    out <- detect$detect_long(dat, "discounting")
    expect_equal(out$x_col, "delay")
    expect_equal(out$y_col, "indiff")
  })
  it("prefers a response that varies over a flat one with a response-like name", {
    dat <- data.frame(
      id = rep(c("a", "b"), each = 3), x = rep(c(1, 2, 3), 2),
      value = rep(c(30, 40), each = 3), consumption = c(9, 6, 3, 8, 5, 2), stringsAsFactors = FALSE
    )
    expect_equal(detect$detect_long(dat, "demand")$y_col, "consumption")
  })
  it("still picks a response that varies within the participant", {
    dat <- data.frame(
      id = rep(c("a", "b"), each = 3), x = rep(c(1, 2, 3), 2),
      age = rep(c(30, 40), each = 3), resp = c(9, 6, 3, 8, 5, 2), stringsAsFactors = FALSE
    )
    expect_equal(detect$detect_long(dat, "demand")$y_col, "resp")
  })
})
