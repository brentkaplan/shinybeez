box::use(
  testthat[...],
)

box::use(
  app / logic / reshape / spec,
)

# Qualtrics-style purchase task: id not first, item headers, one demographic column.
apt <- function() {
  data.frame(
    age = c(31, 45, 28),
    responseid = c("R_1", "R_2", "R_3"),
    apt_1 = c(10, 12, 8),
    apt_2 = c(8, 9, 7),
    apt_3 = c("5", "6 drinks", "4"),
    apt_4 = c(2, 3, 1),
    stringsAsFactors = FALSE
  )
}

apt_spec <- function(...) {
  spec$new_spec(
    target = "demand",
    id_col = "responseid",
    series = list(spec$new_series(c("apt_1", "apt_2", "apt_3", "apt_4"), x = c(0, 0.5, 1, 5))),
    x_source = "manual",
    ...
  )
}

describe("parse_x_text", {
  it("splits on commas, spaces, semicolons and newlines and tolerates $", {
    expect_equal(spec$parse_x_text("0, $0.50;1\n5  10"), c(0, 0.5, 1, 5, 10))
  })
  it("returns numeric(0) for empty input", {
    expect_equal(spec$parse_x_text(""), numeric(0))
    expect_equal(spec$parse_x_text(NULL), numeric(0))
  })
  it("returns NA for a token that is not a number", {
    expect_true(is.na(spec$parse_x_text("0 abc 1")[2]))
  })
})

describe("validate_spec", {
  it("accepts a well-formed single-series demand spec", {
    expect_true(isTRUE(spec$validate_spec(apt_spec(), apt())))
  })

  it("requires an id column that exists and has no empty values", {
    s <- apt_spec()
    s$id_col <- NULL
    expect_match(spec$validate_spec(s, apt()), "identifies each participant")
    s$id_col <- "nope"
    expect_match(spec$validate_spec(s, apt()), "\"nope\" is not in the data")
    dat <- apt()
    dat$responseid[2] <- NA
    expect_match(spec$validate_spec(apt_spec(), dat), "has empty values")
  })

  it("requires at least two columns per series and no role overlap", {
    s <- apt_spec()
    s$series[[1]]$cols <- "apt_1"
    s$series[[1]]$x <- 0
    expect_match(spec$validate_spec(s, apt()), "at least two response columns")
    s <- apt_spec()
    s$series[[1]]$cols[1] <- "responseid"
    expect_match(spec$validate_spec(s, apt()), "cannot be a response column and also")
  })

  it("requires x to match the columns, be numeric, unique, and non-negative", {
    s <- apt_spec()
    s$series[[1]]$x <- c(0, 0.5, 1)
    expect_match(spec$validate_spec(s, apt()), "4 columns selected but 3 prices entered")
    s$series[[1]]$x <- c(0, 0.5, NA, 5)
    expect_match(spec$validate_spec(s, apt()), "must be a number")
    s$series[[1]]$x <- c(0, 0.5, 0.5, 5)
    expect_match(spec$validate_spec(s, apt()), "must be unique")
    s$series[[1]]$x <- c(-1, 0.5, 1, 5)
    expect_match(spec$validate_spec(s, apt()), "cannot be negative")
  })

  it("requires delays to be positive on the discounting target", {
    dat <- data.frame(id = 1:2, d_7 = c(0.9, 0.8), d_30 = c(0.5, 0.4))
    s <- spec$new_spec("discounting", "id", list(spec$new_series(c("d_7", "d_30"), x = c(0, 30))))
    expect_match(spec$validate_spec(s, dat), "greater than zero")
    s$series[[1]]$x <- c(7, 30)
    expect_true(isTRUE(spec$validate_spec(s, dat)))
  })

  it("requires unique ids unless a group column separates repeats", {
    dat <- rbind(apt(), apt())
    expect_match(spec$validate_spec(apt_spec(), dat), "Rows do not have unique ids")
    dat$cond <- rep(c("a", "b"), each = 3)
    expect_true(isTRUE(spec$validate_spec(apt_spec(group_col = "cond"), dat)))
  })

  it("requires named, unique series and forbids a group column with several series", {
    dat <- apt()
    dat$cig_1 <- c(20, 15, 10)
    dat$cig_2 <- c(10, 8, 5)
    two <- function(label2 = "cig", group_col = NULL) {
      spec$new_spec(
        "demand", "responseid",
        list(
          spec$new_series(c("apt_1", "apt_2", "apt_3", "apt_4"), x = c(0, 0.5, 1, 5), label = "alc"),
          spec$new_series(c("cig_1", "cig_2"), x = c(0, 1), label = label2)
        ),
        group_col = group_col
      )
    }
    expect_true(isTRUE(spec$validate_spec(two(), dat)))
    expect_match(spec$validate_spec(two(label2 = ""), dat), "Give every series a name")
    expect_match(spec$validate_spec(two(label2 = "alc"), dat), "Series names must be unique")
    dat$cond <- "a"
    expect_match(spec$validate_spec(two(group_col = "cond"), dat), "separate group column cannot be used")
  })

  it("allows only one series and no group on discounting", {
    dat <- data.frame(id = 1:2, d_7 = c(0.9, 0.8), d_30 = c(0.5, 0.4), e_7 = c(1, 1), e_30 = c(0, 0), g = "a")
    s <- spec$new_spec("discounting", "id", list(
      spec$new_series(c("d_7", "d_30"), x = c(7, 30), label = "d"),
      spec$new_series(c("e_7", "e_30"), x = c(7, 30), label = "e")
    ))
    expect_match(spec$validate_spec(s, dat), "one set of delay columns")
    s <- spec$new_spec("discounting", "id", list(spec$new_series(c("d_7", "d_30"), x = c(7, 30))), group_col = "g")
    expect_match(spec$validate_spec(s, dat), "cannot carry a group column")
  })

  it("rejects reserved names among carried columns and keep_cols outside ME", {
    dat <- apt()
    dat$x <- 1
    s <- spec$new_spec(
      "mixed_effects_demand", "responseid",
      list(spec$new_series(c("apt_1", "apt_2"), x = c(0, 0.5))),
      keep_cols = c("age", "x")
    )
    expect_match(spec$validate_spec(s, dat), "Columns named \"x\" cannot be carried along")
    s$keep_cols <- "age"
    expect_true(isTRUE(spec$validate_spec(s, dat)))
    s$target <- "demand"
    expect_match(spec$validate_spec(s, dat), "Only the mixed-effects tab can carry")
    s$keep_cols <- "nope"
    expect_match(spec$validate_spec(s, dat), "\"nope\"")
  })

  it("rejects keep_cols named after apply_spec's temporary columns", {
    dat <- apt()
    dat$.row <- 1:3
    s <- spec$new_spec(
      "mixed_effects_demand", "responseid",
      list(spec$new_series(c("apt_1", "apt_2"), x = c(0, 0.5))),
      keep_cols = ".row"
    )
    expect_match(spec$validate_spec(s, dat), "cannot be carried along")
  })

  it("rejects a frame with no data rows before any cell check", {
    expect_equal(spec$validate_spec(apt_spec(), apt()[0, ]), "The file has no data rows.")
  })

  it("rejects a series with no numeric cells and ids with fewer than two responses", {
    dat <- apt()
    dat$apt_1 <- "x"
    dat$apt_2 <- "y"
    s <- spec$new_spec("demand", "responseid", list(spec$new_series(c("apt_1", "apt_2"), x = c(0, 1))))
    expect_match(spec$validate_spec(s, dat), "none of the selected columns contain numbers")
    dat <- apt()
    dat$apt_2[1] <- NA
    dat$apt_3[1] <- NA
    dat$apt_4[1] <- NA
    expect_match(spec$validate_spec(apt_spec(), dat), "fewer than two usable responses: \"R_1\"")
  })
})

describe("apply_spec", {
  it("pivots a single series into id, x, y ordered by row then x, parsing text cells", {
    out <- spec$apply_spec(apt_spec(), apt())
    expect_equal(colnames(out$data), c("id", "x", "y"))
    expect_equal(nrow(out$data), 12)
    expect_equal(out$data$id[1:4], rep("R_1", 4))
    expect_equal(out$data$x[1:4], c(0, 0.5, 1, 5))
    expect_equal(out$data$y[1:4], c(10, 8, 5, 2))
    expect_equal(out$data$y[out$data$id == "R_2" & out$data$x == 1], 6)   # "6 drinks"
    expect_type(out$data$id, "character")
    expect_equal(out$losses, list(n_na_y = 0L, n_na_keep = 0L))
    expect_equal(out$n_ids, 3)
  })

  it("carries a group column on demand", {
    dat <- apt()
    dat$cond <- c("a", "a", "b")
    out <- spec$apply_spec(apt_spec(group_col = "cond"), dat)
    expect_equal(colnames(out$data), c("id", "group", "x", "y"))
    expect_equal(out$data$group[out$data$id == "R_3"], rep("b", 4))
  })

  it("turns several series into group levels on demand", {
    dat <- apt()
    dat$cig_1 <- c(20, 15, 10)
    dat$cig_2 <- c(10, 8, 5)
    s <- spec$new_spec("demand", "responseid", list(
      spec$new_series(c("apt_1", "apt_2", "apt_3", "apt_4"), x = c(0, 0.5, 1, 5), label = "alc"),
      spec$new_series(c("cig_1", "cig_2"), x = c(0, 1), label = "cig")
    ))
    out <- spec$apply_spec(s, dat)
    expect_equal(colnames(out$data), c("id", "group", "x", "y"))
    expect_equal(nrow(out$data), 18)
    expect_equal(sort(unique(out$data$group)), c("alc", "cig"))
    expect_equal(out$data$y[out$data$id == "R_1" & out$data$group == "cig"], c(20, 10))
  })

  it("emits id, x, y, series, keep_cols on mixed effects and counts covariate losses", {
    dat <- apt()
    dat$age[3] <- NA
    dat$cig_1 <- c(20, 15, 10)
    dat$cig_2 <- c(10, 8, 5)
    s <- spec$new_spec("mixed_effects_demand", "responseid", list(
      spec$new_series(c("apt_1", "apt_2"), x = c(0, 0.5), label = "alc"),
      spec$new_series(c("cig_1", "cig_2"), x = c(0, 1), label = "cig")
    ), keep_cols = "age")
    out <- spec$apply_spec(s, dat)
    expect_equal(colnames(out$data), c("id", "x", "y", "series", "age"))
    expect_equal(nrow(out$data), 12)
    expect_equal(out$losses$n_na_keep, 4L)
  })

  it("drops NA responses by default and reports the count", {
    dat <- apt()
    dat$apt_2[1] <- NA
    out <- spec$apply_spec(apt_spec(), dat)
    expect_equal(nrow(out$data), 11)
    expect_equal(out$losses$n_na_y, 1L)
    keep <- spec$apply_spec(apt_spec(drop_na = FALSE), dat)
    expect_equal(nrow(keep$data), 12)
  })

  it("emits exactly id, x, y on discounting", {
    dat <- data.frame(id = c("P1", "P2"), d_7 = c(0.9, 0.8), d_30 = c(0.5, 0.4))
    s <- spec$new_spec("discounting", "id", list(spec$new_series(c("d_7", "d_30"), x = c(7, 30))))
    out <- spec$apply_spec(s, dat)
    expect_equal(colnames(out$data), c("id", "x", "y"))
    expect_equal(out$data$x, c(7, 30, 7, 30))
  })

  it("stops with the validation message on an invalid spec", {
    s <- apt_spec()
    s$series[[1]]$x <- c(0, 1)
    expect_error(spec$apply_spec(s, apt()), "4 columns selected but 2 prices entered")
  })
})

describe("apply_spec output re-enters the existing validators unchanged", {
  box::use(app / logic / validate)
  box::use(app / logic / mixed_effects / data_prep)

  it("demand: check_data passes and rename/reshape/retype leave it long", {
    long <- spec$apply_spec(apt_spec(), apt())$data
    expect_true(isTRUE(validate$check_data(long, type = "demand")))
    expect_equal(validate$demand_format(long), "long")
    final <- validate$retype_data(validate$reshape_data(validate$rename_cols(long)))
    expect_equal(colnames(final), c("id", "x", "y"))
    expect_true(isTRUE(validate$check_demand_sufficiency(long)))
  })

  it("discounting: check_data passes and prepare_discounting_data keeps id, x, y", {
    dat <- data.frame(id = c("P1", "P2"), d_7 = c(0.9, 0.8), d_30 = c(0.5, 0.4))
    s <- spec$new_spec("discounting", "id", list(spec$new_series(c("d_7", "d_30"), x = c(7, 30))))
    long <- spec$apply_spec(s, dat)$data
    expect_true(isTRUE(validate$check_data(long, type = "discounting")))
    expect_equal(colnames(validate$prepare_discounting_data(long)), c("id", "x", "y"))
  })

  it("mixed effects: check_data passes and the pickers guess id, x, y", {
    s <- spec$new_spec("mixed_effects_demand", "responseid",
                       list(spec$new_series(c("apt_1", "apt_2"), x = c(0, 0.5))), keep_cols = "age")
    long <- spec$apply_spec(s, apt())$data
    expect_true(isTRUE(validate$check_data(long, type = "mixed_effects_demand")))
    guessed <- data_prep$guess_variable_columns(long)
    expect_equal(guessed[c("id", "x", "y")], list(id = "id", x = "x", y = "y"))
  })
})
