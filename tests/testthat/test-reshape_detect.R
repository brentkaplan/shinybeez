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

describe("x_from_names", {
  it("uses whole-header numbers when every header is one", {
    expect_equal(detect$x_from_names(c("$0", "0.5", "1")), c(0, 0.5, 1))
  })
  it("falls back to suffix numbers", {
    expect_equal(detect$x_from_names(c("price_0", "price_0.5")), c(0, 0.5))
    expect_equal(detect$x_from_names(c("apt_1", "total")), c(1, NA))
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
