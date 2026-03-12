# Tests for app/logic/discounting/five_trial.R

box::use(
  testthat[...],
  readr,
)

box::use(
  app / logic / discounting / five_trial
)

# --- Fixture helpers ----------------------------------------------------------

load_dd_fixture <- function() {
  readr$read_csv(
    fixture_path("discounting-five-trial-dd-minimal.csv"),
    show_col_types = FALSE
  )
}

load_pd_fixture <- function() {
  readr$read_csv(
    fixture_path("discounting-five-trial-pd-minimal.csv"),
    show_col_types = FALSE
  )
}

# ------------------------------------------------------------------------------
# compute_five_trial_dd() tests
# ------------------------------------------------------------------------------

describe("compute_five_trial_dd", {

  it("returns a data frame", {
    dat <- load_dd_fixture()
    result <- suppressWarnings(five_trial$compute_five_trial_dd(dat))
    expect_s3_class(result, "data.frame")
  })

  it("result has at least one row per input participant", {
    dat <- load_dd_fixture()
    n_participants <- length(unique(dat$ResponseId))
    result <- suppressWarnings(five_trial$compute_five_trial_dd(dat))
    expect_gte(nrow(result), n_participants)
  })

  it("result contains ResponseId column", {
    dat <- load_dd_fixture()
    result <- suppressWarnings(five_trial$compute_five_trial_dd(dat))
    expect_true("ResponseId" %in% names(result))
  })

  it("result contains kval column (delay discounting k parameter)", {
    dat <- load_dd_fixture()
    result <- suppressWarnings(five_trial$compute_five_trial_dd(dat))
    expect_true("kval" %in% names(result))
  })

  it("kval column is numeric", {
    dat <- load_dd_fixture()
    result <- suppressWarnings(five_trial$compute_five_trial_dd(dat))
    expect_type(result$kval, "double")
  })
})

# ------------------------------------------------------------------------------
# compute_five_trial_pd() tests
# ------------------------------------------------------------------------------

describe("compute_five_trial_pd", {

  it("returns a data frame", {
    dat <- load_pd_fixture()
    result <- suppressWarnings(five_trial$compute_five_trial_pd(dat))
    expect_s3_class(result, "data.frame")
  })

  it("result has at least one row per input participant", {
    dat <- load_pd_fixture()
    n_participants <- length(unique(dat$ResponseId))
    result <- suppressWarnings(five_trial$compute_five_trial_pd(dat))
    expect_gte(nrow(result), n_participants)
  })

  it("result contains ResponseId column", {
    dat <- load_pd_fixture()
    result <- suppressWarnings(five_trial$compute_five_trial_pd(dat))
    expect_true("ResponseId" %in% names(result))
  })

  it("result contains hval column (probability discounting h parameter)", {
    dat <- load_pd_fixture()
    result <- suppressWarnings(five_trial$compute_five_trial_pd(dat))
    expect_true("hval" %in% names(result))
  })

  it("hval column is numeric", {
    dat <- load_pd_fixture()
    result <- suppressWarnings(five_trial$compute_five_trial_pd(dat))
    expect_type(result$hval, "double")
  })
})
