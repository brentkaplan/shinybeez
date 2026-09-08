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
  it("reads a monkey x drug frame as a within-subject design", {
    # `dose` is perfectly confounded with `drug` here (Alfentanil <-> 0.003, Saline <-> 0),
    # so this is one two-level factor wearing two column names, not two crossed factors.
    # The file passes check_data() on the mixed-effects tab, so the mapper only ever sees
    # it from the demand tab, where the wide guess had nothing to offer.
    out <- detect$detect_long(fixture("mixed-effects-minimal.csv"), "mixed_effects_demand")
    expect_equal(
      c(out$id_col, out$x_col, out$y_col, out$group_col), c("monkey", "x", "y", "drug")
    )
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
  it("prefers a response that varies, whatever the other column is called", {
    dat <- data.frame(
      id = rep(c("a", "b"), each = 3), x = rep(c(1, 2, 3), 2),
      consumption = rep(c(30, 40), each = 3), mystery = c(9, 6, 3, 8, 5, 2), stringsAsFactors = FALSE
    )
    expect_equal(detect$detect_long(dat, "demand")$y_col, "mystery")
  })
  it("does not let an ambiguous name rescue a flat covariate", {
    # "value" reads as a response but is just as often a per-participant score; only an
    # unmistakable response name (y, consumption, indiff, ip) rescues a flat column.
    dat <- data.frame(
      id = rep(c("a", "b"), each = 3), x = rep(c(1, 2, 3), 2),
      value = rep(c(30, 40), each = 3), stringsAsFactors = FALSE
    )
    expect_null(detect$detect_long(dat, "demand"))
  })
  it("still picks a response that varies within the participant", {
    dat <- data.frame(
      id = rep(c("a", "b"), each = 3), x = rep(c(1, 2, 3), 2),
      age = rep(c(30, 40), each = 3), resp = c(9, 6, 3, 8, 5, 2), stringsAsFactors = FALSE
    )
    expect_equal(detect$detect_long(dat, "demand")$y_col, "resp")
  })
})

describe("detect_long, within-subject grouped data", {
  it("detects a file whose price grid repeats inside every condition", {
    out <- detect$detect_long(fixture("long-within-subject.csv"), "demand")
    expect_equal(
      c(out$id_col, out$x_col, out$y_col, out$group_col),
      c("subject", "price", "consumption", "condition")
    )
  })
  it("does not need the condition column to be recognisably named", {
    dat <- fixture("long-within-subject.csv")
    colnames(dat)[colnames(dat) == "condition"] <- "phase"
    out <- detect$detect_long(dat, "demand")
    expect_equal(out$group_col, "phase")
    expect_equal(out$x_col, "price")
  })
  it("carries the composite group through guess_spec", {
    s <- detect$guess_spec(fixture("long-within-subject.csv"), "demand")
    expect_equal(s$layout, "long")
    expect_equal(s$group_col, "condition")
  })
  it("declines a ragged design where a participant is missing a condition", {
    dat <- fixture("long-within-subject.csv")
    dat <- dat[!(dat$subject == "s3" & dat$condition == "stress"), , drop = FALSE]
    expect_null(detect$detect_long(dat, "demand"))
  })
  it("declines when the conditions do not ask the same prices", {
    # One price differs, so the grids overlap without matching: the plain-id path still
    # sees x repeat within the participant, and the composite attempt refuses a design
    # whose levels are not asking the same question.
    dat <- fixture("long-within-subject.csv")
    dat$price[dat$condition == "stress" & dat$price == 4] <- 8
    expect_null(detect$detect_long(dat, "demand"))
  })
  it("declines a genuinely crossed two-factor design", {
    # monkey x drug x dose x price: widening the key by one column still leaves the price
    # grid repeated inside every cell, so there is no honest single group to offer.
    expect_null(detect$detect_long(fixture("long-two-factor.csv"), "mixed_effects_demand"))
    expect_null(detect$detect_long(fixture("long-two-factor.csv"), "demand"))
  })
  it("prefers a recognisably named condition when two columns both partition", {
    dat <- fixture("long-within-subject.csv")
    dat$aaa_label <- dat$condition
    out <- detect$detect_long(dat, "demand")
    expect_equal(out$group_col, "condition")
  })
  it("leaves the plain-id path in charge when it already succeeds", {
    # group is constant within id, so the between-subject path answers first and the
    # composite attempt never runs.
    dat <- fixture("long-extra-cols.csv")
    out <- detect$detect_long(dat, "demand")
    expect_equal(out$id_col, detect$detect_long(dat, "demand")$id_col)
    expect_false(is.null(out))
  })
  it("never offers a group for a discounting target", {
    expect_null(detect$detect_long(fixture("long-within-subject.csv"), "discounting")$group_col)
  })
})

describe("detect_long, within-subject false positives", {
  it("declines a condition column with blank levels", {
    # validate_spec() rejects an empty group value outright (spec.R), so offering one
    # prefills a mapping that cannot be confirmed.
    dat <- expand.grid(
      id = paste0("s", 1:3), condition = c("", "stress"), x = c(1, 2),
      stringsAsFactors = FALSE
    )
    dat$y <- seq_len(nrow(dat))
    expect_null(detect$detect_long(dat, "demand"))
  })
  it("declines a whitespace-only condition level", {
    dat <- expand.grid(
      id = paste0("s", 1:3), condition = c("  ", "stress"), x = c(1, 2),
      stringsAsFactors = FALSE
    )
    dat$y <- seq_len(nrow(dat))
    expect_null(detect$detect_long(dat, "demand"))
  })
  it("will not accept a per-condition score as the response, however it is named", {
    # Flat within the cell means it does not answer the price. The name exception exists
    # for a non-discounter answering one indifference point at every delay, and discounting
    # never reaches the composite attempt.
    dat <- expand.grid(
      id = paste0("s", 1:3), condition = c("base", "stress"), x = c(1, 2, 4),
      stringsAsFactors = FALSE
    )
    dat$consumption <- ifelse(dat$condition == "base", 100, 50)
    expect_null(detect$detect_long(dat, "demand"))
  })
  it("keeps two participants apart when their names contain the key separator", {
    dat <- data.frame(
      id = rep(c("A", "A\r", "C"), each = 4),
      condition = rep(c("\rB", "\rB", "B", "B"), 3),
      x = rep(c(1, 2, 1, 2), 3),
      y = seq_len(12),
      stringsAsFactors = FALSE
    )
    out <- detect$detect_long(dat, "demand")
    expect_equal(c(out$id_col, out$x_col, out$y_col, out$group_col),
                 c("id", "x", "y", "condition"))
  })
})

describe("partitioning_candidates", {
  two_commodity <- function(cond_name = "commodity") {
    d <- rbind(
      expand.grid(id = paste0("s", 1:3), lvl = "beer", price = c(1, 2, 4, 8, 16),
                  stringsAsFactors = FALSE),
      expand.grid(id = paste0("s", 1:3), lvl = "cigarettes", price = c(0.25, 0.5, 1.5, 3, 6),
                  stringsAsFactors = FALSE)
    )
    d$consumption <- round(20 / d$price, 1)
    names(d)[names(d) == "lvl"] <- cond_name
    d[order(d$id, d[[cond_name]], d$price), c("id", cond_name, "price", "consumption")]
  }
  it("finds a column that splits each participant into complete sets", {
    expect_equal(
      detect$partitioning_candidates(two_commodity(), "id", c("price", "consumption")),
      "commodity"
    )
  })
  it("finds it whatever it is called", {
    expect_equal(
      detect$partitioning_candidates(two_commodity("phase"), "id", c("price", "consumption")),
      "phase"
    )
  })
  it("offers nothing when a column is constant within the participant", {
    # a between-subject group is guess_group_col()'s job; its (id, level) table has zero cells
    expect_equal(
      detect$partitioning_candidates(fixture("long-extra-cols.csv"), "id", c("x", "y")),
      character(0)
    )
  })
  it("offers a condition that was coded as a number, but does not choose it", {
    # 0/1 is as common a coding for a condition as a word is, so it belongs in the modal.
    # It is not chosen unasked: a column of bare codes is indistinguishable from a trial
    # index, and "session = 1,2,1,2" over four prices partitions just as cleanly.
    dat <- rbind(
      expand.grid(id = paste0("s", 1:3), condition = 0L, price = c(1, 2, 4, 8, 16),
                  stringsAsFactors = FALSE),
      expand.grid(id = paste0("s", 1:3), condition = 1L, price = c(0.25, 0.5, 1.5, 3, 6),
                  stringsAsFactors = FALSE)
    )
    dat$consumption <- seq_len(nrow(dat))
    expect_equal(
      detect$partitioning_candidates(dat, "id", c("price", "consumption")), "condition"
    )
    expect_null(detect$detect_long(dat, "demand")$group_col)
  })
  it("groups a condition whose levels are short labels, by decision", {
    # DELIBERATE, not an oversight. A header that says `condition`, levels that interleave on
    # the price axis, and values that are words: the header is the only declaration of intent
    # a file carries, and honouring it is the right reading. Merging T1 and T2 into one curve
    # would be the error. The note and the curve count in the preview make the choice visible
    # and one click to undo, which is what earns the right to make it.
    dat <- rbind(
      expand.grid(id = paste0("s", 1:3), condition = "T1", price = c(1, 2, 4, 8, 16),
                  stringsAsFactors = FALSE),
      expand.grid(id = paste0("s", 1:3), condition = "T2", price = c(0.25, 0.5, 1.5, 3, 6),
                  stringsAsFactors = FALSE)
    )
    dat$consumption <- seq_len(nrow(dat))
    expect_equal(detect$detect_long(dat, "demand")$group_col, "condition")
  })
  it("does not silently group on an alternating trial index called a session", {
    dat <- data.frame(
      id = rep(paste0("s", 1:4), each = 4), x = rep(1:4, 4), y = seq_len(16),
      session = rep(c(1, 2, 1, 2), 4)
    )
    expect_null(detect$detect_long(dat, "demand")$group_col)
    expect_equal(detect$partitioning_candidates(dat, "id", c("x", "y")), "session")
  })
  it("offers nothing for a plain long file", {
    expect_equal(
      detect$partitioning_candidates(fixture("demand-minimal-long.csv"), "id", c("x", "y")),
      character(0)
    )
  })
})

describe("detect_long, conditions that ask different questions", {
  two_commodity <- function(cond_name = "commodity") {
    d <- rbind(
      expand.grid(id = paste0("s", 1:3), lvl = "beer", price = c(1, 2, 4, 8, 16),
                  stringsAsFactors = FALSE),
      expand.grid(id = paste0("s", 1:3), lvl = "cigarettes", price = c(0.25, 0.5, 1.5, 3, 6),
                  stringsAsFactors = FALSE)
    )
    d$consumption <- round(20 / d$price, 1)
    names(d)[names(d) == "lvl"] <- cond_name
    d[order(d$id, d[[cond_name]], d$price), c("id", cond_name, "price", "consumption")]
  }
  it("no longer merges two commodities into one curve", {
    out <- detect$detect_long(two_commodity(), "demand")
    expect_equal(
      c(out$id_col, out$x_col, out$y_col, out$group_col),
      c("id", "price", "consumption", "commodity")
    )
  })
  it("leaves a condition it cannot name to the user", {
    # structural detection cannot tell a real condition from a derived bin of the price, so
    # an unrecognised name is offered in the modal, never chosen silently.
    out <- detect$detect_long(two_commodity("phase"), "demand")
    expect_null(out$group_col)
    expect_equal(
      detect$partitioning_candidates(two_commodity("phase"), "id", c("price", "consumption")),
      "phase"
    )
  })
  it("does not split one curve on a price band that calls itself a condition", {
    # low/high are contiguous slices of one price grid, not two grids that interleave.
    dat <- data.frame(
      id = rep(paste0("s", 1:3), each = 10),
      condition = rep(rep(c("low", "high"), each = 5), 3),
      x = rep(c(0.25, 0.5, 1.5, 3, 6, 8, 10, 12, 14, 16), 3),
      y = seq_len(30),
      stringsAsFactors = FALSE
    )
    expect_null(detect$detect_long(dat, "demand")$group_col)
  })
  it("never offers a group for a discounting target", {
    expect_null(detect$detect_long(two_commodity(), "discounting")$group_col)
  })
})
