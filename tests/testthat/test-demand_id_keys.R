# A within-subject file has the same id in more than one group. The upload accepts it (the
# duplicate-price check keys on id and group), but everything that works by id alone
# aborted: Two Stage, which ignores grouping, said "Duplicates found where id = 1", and so
# did the ungrouped Empirical Measures. Such an id is analysed as one curve per group.

box::use(
  testthat[...],
)

box::use(
  app / logic / demand / empirical,
  app / logic / demand / fitting,
  app / logic / demand / id_keys,
  app / logic / demand / systematic,
)

prices <- c(0, 0.5, 1, 2, 5, 10, 20)

one_curve <- function(id, group, y) {
  data.frame(id = id, group = group, x = prices, y = y, stringsAsFactors = FALSE)
}

# id 1 is in groups a and b; ids 2 to 5 are in one group each.
within_subject <- function() {
  rbind(
    one_curve("1", "a", c(10, 9, 7, 5, 2, 1, 0)),
    one_curve("2", "a", c(12, 10, 9, 7, 3, 1, 0)),
    one_curve("3", "a", c(9, 8, 7, 5, 2, 0, 0)),
    one_curve("1", "b", c(8, 7, 6, 4, 2, 0, 0)),
    one_curve("4", "b", c(11, 9, 8, 6, 2, 1, 0)),
    one_curve("5", "b", c(7, 6, 5, 4, 1, 0, 0))
  )
}

describe("ids_spanning_groups()", {
  it("names the ids that appear in more than one group", {
    expect_identical(id_keys$ids_spanning_groups(within_subject()), "1")
  })

  it("is empty when every id has one group, or there is no group column", {
    dat <- within_subject()
    expect_identical(id_keys$ids_spanning_groups(dat[dat$group == "a", ]), character(0))
    expect_identical(id_keys$ids_spanning_groups(dat[dat$group == "a", c("id", "x", "y")]), character(0))
  })
})

describe("key_ids_within_group()", {
  it("labels each curve of a spanning id by its group and leaves other ids alone", {
    out <- id_keys$key_ids_within_group(within_subject())
    expect_identical(
      as.character(out$id),
      rep(c("1 (a)", "2", "3", "1 (b)", "4", "5"), each = length(prices))
    )
    expect_identical(out$group, within_subject()$group)
    expect_identical(out$y, within_subject()$y)
    expect_identical(attr(out, "ids_keyed_by_group"), "1")
  })

  it("leaves no id with two rows at one price", {
    out <- id_keys$key_ids_within_group(within_subject())
    expect_identical(anyDuplicated(out[c("id", "x")]), 0L)
  })

  it("returns the data as it came, with an empty record, when no id spans groups", {
    dat <- within_subject()
    dat <- dat[!(dat$id == "1" & dat$group == "b"), ]
    out <- id_keys$key_ids_within_group(dat)
    expect_identical(out$id, dat$id)
    expect_identical(attr(out, "ids_keyed_by_group"), character(0))
  })

  it("keeps a factor id a factor, with each new label where its id stood", {
    dat <- within_subject()
    dat$id <- factor(dat$id, levels = c("5", "1", "2", "3", "4"))
    dat$group <- factor(dat$group)
    out <- id_keys$key_ids_within_group(dat)
    expect_identical(levels(out$id), c("5", "1 (a)", "1 (b)", "2", "3", "4"))
  })

  it("never gives a new label that another id already has", {
    dat <- rbind(within_subject(), one_curve("1 (a)", "a", c(6, 5, 4, 3, 1, 0, 0)))
    out <- id_keys$key_ids_within_group(dat)
    labels <- unique(as.character(out$id))
    # 7 curves in, 7 distinct ids out
    expect_length(labels, 7L)
    expect_identical(anyDuplicated(out[c("id", "x")]), 0L)
    # the id that was already called "1 (a)" keeps its name
    expect_identical(unique(as.character(out$id[43:49])), "1 (a)")
  })
})

describe("key_ids_within_group() and labels that contain the separator", {
  it("keeps four id and group pairs as four ids", {
    dat <- data.frame(
      id = c("a\rb", "a\rb", "a", "a"), group = c("c", "d", "b\rc", "e"),
      x = 1, y = 1, stringsAsFactors = FALSE
    )
    out <- id_keys$key_ids_within_group(dat)
    expect_identical(as.character(out$id), c("a\rb (c)", "a\rb (d)", "a (b\rc)", "a (e)"))
  })
})

describe("key_ids_notice()", {
  it("names the ids and says how they are analysed", {
    msg <- id_keys$key_ids_notice(id_keys$key_ids_within_group(within_subject()))
    expect_match(msg, "\"1\"", fixed = TRUE)
    expect_match(msg, "more than one group", fixed = TRUE)
    expect_match(msg, "1 (a)", fixed = TRUE)
  })

  it("is NULL when no id was keyed", {
    dat <- within_subject()
    expect_null(id_keys$key_ids_notice(id_keys$key_ids_within_group(dat[dat$group == "a", ])))
  })
})

describe("a within-subject file, once keyed", {
  keyed <- function() id_keys$key_ids_within_group(within_subject())

  it("fits one Two Stage curve per id and group: 6 curves from 5 ids", {
    fit <- fitting$fit_demand_ungrouped(keyed(), eq = "koff", agg = NULL, k = 2)
    expect_identical(nrow(fit$results), 6L)
    expect_setequal(as.character(fit$results$id), c("1 (a)", "1 (b)", "2", "3", "4", "5"))
  })

  it("gives the curve of id 1 in group a the fit that curve gets on its own", {
    all_fit <- fitting$fit_demand_ungrouped(keyed(), eq = "koff", agg = NULL, k = 2)$results
    alone <- one_curve("only", "a", c(10, 9, 7, 5, 2, 1, 0))
    alone_fit <- fitting$fit_demand_ungrouped(alone, eq = "koff", agg = NULL, k = 2)$results
    expect_identical(all_fit$Q0d[all_fit$id == "1 (a)"], alone_fit$Q0d)
    expect_identical(all_fit$Alpha[all_fit$id == "1 (a)"], alone_fit$Alpha)
  })

  it("computes ungrouped empirical measures and systematic criteria without aborting", {
    measures <- empirical$compute_empirical_measures(keyed(), is_grouped = FALSE)
    # one row per curve, after the aggregate row
    expect_identical(sum(measures$id != "aggregate"), 6L)
    checks <- systematic$compute_systematic(
      keyed(), deltaq = 0.025, bounce = 0.1, reversals = 0, ncons0 = 2, is_grouped = FALSE
    )
    expect_identical(nrow(checks), 6L)
  })

  it("reads the highest consumption of id 1 in group b off its own curve", {
    measures <- empirical$compute_empirical_measures(keyed(), is_grouped = FALSE)
    # that curve is 8, 7, 6, 4, 2, 0, 0: intensity (consumption at the lowest price) is 8
    expect_identical(measures$Intensity[measures$id == "1 (b)"], 8)
  })
})
