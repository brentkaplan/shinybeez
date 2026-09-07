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
