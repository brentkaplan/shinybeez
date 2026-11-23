# Tests for app/logic/discounting/scoring.R

box::use(
  testthat[...],
)

box::use(
  app / logic / discounting / scoring
)

# --- Test data helpers --------------------------------------------------------

make_mock_results <- function() {
  data.frame(
    subjectid = c("s1", "s2"),
    small_k = c(0.123456789, 0.987654321),
    medium_k = c(0.111111111, 0.222222222),
    large_k = c(0.333333333, 0.444444444),
    overall_k = c(0.555555555, 0.666666666),
    geomean_k = c(0.777777777, 0.888888888),
    small_proportion = c(0.12345, 0.67891),
    medium_proportion = c(0.56789, 0.23456),
    large_proportion = c(0.98765, 0.43219),
    overall_proportion = c(0.11111, 0.99999),
    overall_consistency = c(0.98765, 0.43219),
    small_consistency = c(0.12345, 0.67891),
    medium_consistency = c(0.56789, 0.23456),
    large_consistency = c(0.33333, 0.77777),
    composite_consistency = c(0.44444, 0.55555),
    other_col = c(1.123456, 2.654321),
    impute_method = c("none", "none"),
    stringsAsFactors = FALSE
  )
}

make_mcq_data <- function(n_ids = 5, seed = 42) {
  beezdiscounting::generate_data_mcq(n_ids = n_ids, seed = seed)
}

# ------------------------------------------------------------------------------
# resolve_imputation() tests
# ------------------------------------------------------------------------------

describe("resolve_imputation", {

  it("returns 'none' impute_method and FALSE random for 'none'", {
    result <- scoring$resolve_imputation("none")
    expect_equal(result$impute_method, "none")
    expect_false(result$random)
  })

  it("returns 'none' impute_method and FALSE random for NULL input", {
    result <- scoring$resolve_imputation(NULL)
    expect_equal(result$impute_method, "none")
    expect_false(result$random)
  })

  # These previously asserted that arbitrary strings ("mean", "median", "random", "Random",
  # "RANDOM") were passed through unchanged. beezdiscounting accepts none/ggm/GGM/inn/INN and
  # nothing else — every one of those values is rejected with
  #   "Impute method must be one of none, ggm, GGM, inn, INN"
  # which is production signature 1155c2d9, 21 events. The old expectations described a
  # pass-through contract that could only ever end in that abort, so they documented the bug
  # instead of catching it. resolve_imputation() now falls back to "none" for anything the
  # downstream package will not accept.

  it("falls back to 'none' for methods beezdiscounting does not accept", {
    for (unsupported in c("mean", "median", "random", "Random", "RANDOM", "")) {
      result <- scoring$resolve_imputation(unsupported)
      expect_equal(result$impute_method, "none")
      expect_false(result$random)
    }
  })

  it("passes through the supported methods unchanged", {
    for (supported in c("none", "ggm", "GGM", "inn", "INN")) {
      result <- scoring$resolve_imputation(supported)
      expect_equal(result$impute_method, supported)
      expect_false(result$random)
    }
  })

  it("splits the _random suffix into the method and the random flag", {
    result <- scoring$resolve_imputation("INN_random")
    expect_equal(result$impute_method, "INN")
    expect_true(result$random)
  })

  it("always returns a list with exactly two named elements", {
    result <- scoring$resolve_imputation("none")
    expect_type(result, "list")
    expect_equal(length(result), 2)
    expect_true(all(c("impute_method", "random") %in% names(result)))
  })

  it("strips _random suffix and returns TRUE random for 'INN_random'", {
    result <- scoring$resolve_imputation("INN_random")
    expect_equal(result$impute_method, "INN")
    expect_true(result$random)
  })

  it("strips _random suffix and returns TRUE random for 'GGM_random'", {
    result <- scoring$resolve_imputation("GGM_random")
    expect_equal(result$impute_method, "GGM")
    expect_true(result$random)
  })

  it("strips _random suffix case-insensitively for 'inn_Random'", {
    result <- scoring$resolve_imputation("inn_Random")
    expect_equal(result$impute_method, "inn")
    expect_true(result$random)
  })
})

# ------------------------------------------------------------------------------
# friendly_discounting_error() tests
# ------------------------------------------------------------------------------

describe("friendly_discounting_error", {

  it("translates 'In index: 1' pattern to data format error", {
    msg <- scoring$friendly_discounting_error("Error in mutate: In index: 1. something")
    expect_match(msg, "Data format error")
  })

  it("translates impute method error", {
    msg <- scoring$friendly_discounting_error("Impute method must be one of none, ggm")
    expect_match(msg, "Invalid imputation method")
  })

  it("translates argument length zero error", {
    msg <- scoring$friendly_discounting_error("argument is of length zero")
    expect_match(msg, "required column appears to be missing")
  })

  it("translates undefined columns error", {
    msg <- scoring$friendly_discounting_error("undefined columns selected")
    expect_match(msg, "missing expected columns")
  })

  it("translates response length error", {
    msg <- scoring$friendly_discounting_error("Response length not equal to 27")
    expect_match(msg, "exactly 27 items")
  })

  it("returns generic message for unrecognized errors", {
    msg <- scoring$friendly_discounting_error("something completely unknown")
    expect_match(msg, "An error occurred during scoring")
    expect_match(msg, "something completely unknown")
  })
})

# ------------------------------------------------------------------------------
# format_mcq_results() tests
# ------------------------------------------------------------------------------

describe("format_mcq_results", {

  it("rounds _k columns to 6 decimal places", {
    mock <- make_mock_results()
    result <- scoring$format_mcq_results(mock)
    k_cols <- c("small_k", "medium_k", "large_k", "overall_k", "geomean_k")
    for (col in k_cols) {
      expect_equal(
        result[[col]],
        round(mock[[col]], 6),
        info = paste(col, "should be rounded to 6 decimal places")
      )
    }
  })

  it("rounds _prop columns to 3 decimal places", {
    mock <- make_mock_results()
    result <- scoring$format_mcq_results(mock)
    prop_cols <- c(
      "small_proportion", "medium_proportion",
      "large_proportion", "overall_proportion"
    )
    for (col in prop_cols) {
      expect_equal(
        result[[col]],
        round(mock[[col]], 3),
        info = paste(col, "should be rounded to 3 decimal places")
      )
    }
  })

  it("rounds _cons columns to 3 decimal places", {
    mock <- make_mock_results()
    result <- scoring$format_mcq_results(mock)
    cons_cols <- c(
      "overall_consistency", "small_consistency",
      "medium_consistency", "large_consistency",
      "composite_consistency"
    )
    for (col in cons_cols) {
      expect_equal(
        result[[col]],
        round(mock[[col]], 3),
        info = paste(col, "should be rounded to 3 decimal places")
      )
    }
  })

  it("does not modify columns that lack _k, _prop, or _cons in the name", {
    mock <- make_mock_results()
    result <- scoring$format_mcq_results(mock)
    expect_equal(result$other_col, mock$other_col)
    expect_equal(result$subjectid, mock$subjectid)
    expect_equal(result$impute_method, mock$impute_method)
  })

  it("preserves all column names", {
    mock <- make_mock_results()
    result <- scoring$format_mcq_results(mock)
    expect_equal(names(result), names(mock))
  })

  it("preserves the number of rows", {
    mock <- make_mock_results()
    result <- scoring$format_mcq_results(mock)
    expect_equal(nrow(result), nrow(mock))
  })

  it("returns a data.frame", {
    mock <- make_mock_results()
    result <- scoring$format_mcq_results(mock)
    expect_s3_class(result, "data.frame")
  })
})

# ------------------------------------------------------------------------------
# score_and_format_mcq() tests
# ------------------------------------------------------------------------------

describe("score_and_format_mcq", {

  it("returns a list with results, data, and summary components", {
    mcq_data <- make_mcq_data(n_ids = 3)
    result <- scoring$score_and_format_mcq(mcq_data)
    expect_type(result, "list")
    expect_true("results" %in% names(result))
    expect_true("data" %in% names(result))
    expect_true("summary" %in% names(result))
  })

  it("results component is a data.frame with one row per participant", {
    mcq_data <- make_mcq_data(n_ids = 5)
    result <- scoring$score_and_format_mcq(mcq_data)
    expect_s3_class(result$results, "data.frame")
    expect_equal(nrow(result$results), 5)
  })

  it("data component is a data.frame", {
    mcq_data <- make_mcq_data(n_ids = 3)
    result <- scoring$score_and_format_mcq(mcq_data)
    expect_s3_class(result$data, "data.frame")
  })

  it("summary component is a data.frame with expected columns", {
    mcq_data <- make_mcq_data(n_ids = 3)
    result <- scoring$score_and_format_mcq(mcq_data)
    expect_s3_class(result$summary, "data.frame")
    expect_true(all(c("Metric", "Mean", "SD", "SEM") %in% names(result$summary)))
  })

  it("results contain expected MCQ columns", {
    mcq_data <- make_mcq_data(n_ids = 3)
    result <- scoring$score_and_format_mcq(mcq_data)
    expected_cols <- c(
      "subjectid", "overall_k", "small_k", "medium_k", "large_k",
      "geomean_k", "overall_consistency", "small_consistency",
      "medium_consistency", "large_consistency"
    )
    for (col in expected_cols) {
      expect_true(
        col %in% names(result$results),
        info = paste(col, "should be in results")
      )
    }
  })

  it("results have _k columns rounded to 6 decimal places", {
    mcq_data <- make_mcq_data(n_ids = 5)
    result <- scoring$score_and_format_mcq(mcq_data)
    k_cols <- grep("_k$", names(result$results), value = TRUE)
    for (col in k_cols) {
      values <- result$results[[col]]
      expect_equal(
        values,
        round(values, 6),
        info = paste(col, "should be rounded to 6 decimal places")
      )
    }
  })

  it("results have _consistency columns rounded to 3 decimal places", {
    mcq_data <- make_mcq_data(n_ids = 5)
    result <- scoring$score_and_format_mcq(mcq_data)
    cons_cols <- grep("_cons", names(result$results), value = TRUE)
    for (col in cons_cols) {
      values <- result$results[[col]]
      expect_equal(
        values,
        round(values, 3),
        info = paste(col, "should be rounded to 3 decimal places")
      )
    }
  })

  it("results have _proportion columns rounded to 3 decimal places", {
    mcq_data <- make_mcq_data(n_ids = 5)
    result <- scoring$score_and_format_mcq(mcq_data)
    prop_cols <- grep("_prop", names(result$results), value = TRUE)
    for (col in prop_cols) {
      values <- result$results[[col]]
      expect_equal(
        values,
        round(values, 3),
        info = paste(col, "should be rounded to 3 decimal places")
      )
    }
  })

  it("defaults imputation to 'none' when not specified", {
    mcq_data <- make_mcq_data(n_ids = 3)
    result <- scoring$score_and_format_mcq(mcq_data)
    expect_true(all(result$results$impute_method == "none"))
  })

  it("defaults trans to 'none' when not specified", {
    mcq_data <- make_mcq_data(n_ids = 3)
    result_default <- scoring$score_and_format_mcq(mcq_data)
    result_explicit <- scoring$score_and_format_mcq(mcq_data, trans = "none")
    expect_equal(result_default$results, result_explicit$results)
  })

  it("accepts an explicit 'none' imputation method", {
    mcq_data <- make_mcq_data(n_ids = 3, seed = 99)
    result <- scoring$score_and_format_mcq(mcq_data, imputation = "none")
    expect_type(result, "list")
    expect_s3_class(result$results, "data.frame")
  })

  it("returns consistent results for the same input data", {
    mcq_data <- make_mcq_data(n_ids = 3, seed = 123)
    result1 <- scoring$score_and_format_mcq(mcq_data)
    result2 <- scoring$score_and_format_mcq(mcq_data)
    expect_equal(result1$results, result2$results)
  })

  # Imputation fills in missing answers; it must never alter an answer the participant
  # gave. Uploads are not guaranteed to list questions 1..27 in order.
  it("returns each observed answer unchanged after INN imputation, whatever the question order", {
    dat <- make_mcq_data(n_ids = 2)
    dat$response[c(3, 11, 20, 30, 41)] <- NA
    reversed <- dat[order(dat$subjectid, -dat$questionid), ]

    scored <- suppressWarnings(scoring$score_and_format_mcq(reversed, imputation = "INN"))$data

    check <- merge(
      dat, scored[, c("subjectid", "questionid", "newresponse")],
      by = c("subjectid", "questionid")
    )
    observed <- !is.na(check$response)
    expect_equal(nrow(check), nrow(dat))
    expect_equal(check$newresponse[observed], check$response[observed])
  })

  it("returns each observed answer unchanged when question ids are a factor", {
    # factor("1".."27") sorts as text ("1", "10", "11", ...), not as numbers
    dat <- data.frame(subjectid = 1, questionid = 1:27, response = rep(c(0, 1), length.out = 27))
    dat$response[3] <- NA
    as_factor <- dat
    as_factor$questionid <- factor(as.character(as_factor$questionid))

    scored <- suppressWarnings(scoring$score_and_format_mcq(as_factor, imputation = "INN"))$data
    scored$questionid <- as.numeric(as.character(scored$questionid))

    check <- merge(dat, scored[, c("questionid", "newresponse")], by = "questionid")
    observed <- !is.na(check$response)
    expect_equal(check$newresponse[observed], check$response[observed])
  })

  it("imputes the same answer for a question whatever the question order", {
    dat <- make_mcq_data(n_ids = 2)
    dat$response[c(3, 11, 20, 30, 41)] <- NA
    reversed <- dat[order(dat$subjectid, -dat$questionid), ]

    by_key <- function(d) {
      d <- d[order(d$subjectid, d$questionid), c("subjectid", "questionid", "newresponse")]
      rownames(d) <- NULL
      d
    }
    in_order <- suppressWarnings(scoring$score_and_format_mcq(dat, imputation = "INN"))$data
    out_of_order <- suppressWarnings(scoring$score_and_format_mcq(reversed, imputation = "INN"))$data

    expect_equal(by_key(out_of_order), by_key(in_order))
  })
})

# ------------------------------------------------------------------------------
# mean_prop_ss() tests
# ------------------------------------------------------------------------------

describe("mean_prop_ss", {
  # Each of the 9 k ranks has 3 items. A participant who answers 0 to all 27 chose the
  # smaller-sooner option on 3 of 3 items at every rank (proportion 1); one who answers
  # 1 to all 27 chose it on 0 of 3 (proportion 0).
  all_answers <- function(subjectid, response) {
    data.frame(subjectid = subjectid, questionid = 1:27, response = response)
  }

  it("averages two opposite participants to one half at every k rank", {
    dat <- rbind(all_answers("A", 0), all_answers("B", 1))

    for (d in list(dat, dat[rev(seq_len(nrow(dat))), ])) {
      out <- scoring$mean_prop_ss(d)
      expect_equal(nrow(out), 9)
      expect_equal(out$prop_ss, rep(0.5, 9))
    }
  })

  it("weights participants equally: two of three choosing smaller-sooner gives 0.67", {
    dat <- rbind(all_answers("A", 0), all_answers("B", 0), all_answers("C", 1))

    expect_equal(scoring$mean_prop_ss(dat)$prop_ss, rep(0.67, 9))
  })

  it("averages exact thirds and rounds once: one of six items gives 0.17, not 0.16", {
    # A chose smaller-sooner on question 1 only; B never did. At question 1's k rank
    # that is 1 of 3 items for A and 0 of 3 for B: mean 1/6 = 0.1667. Every other
    # rank is 0 for both.
    a <- all_answers("A", 1)
    a$response[a$questionid == 1] <- 0
    out <- scoring$mean_prop_ss(rbind(a, all_answers("B", 1)))

    expect_equal(sort(out$prop_ss), c(rep(0, 8), 0.17))
  })

  # Questions 1, 9 and 13 are the 3 items at the lowest k rank (0.00016).
  it("averages over the participants who answered when one skipped a whole k rank", {
    a <- all_answers("A", 1)
    a$response[a$questionid %in% c(1, 9, 13)] <- NA
    out <- scoring$mean_prop_ss(rbind(a, all_answers("B", 0)))

    # Lowest rank: only B answered, 3 of 3 smaller-sooner. Other ranks: A 0, B 1.
    expect_equal(out$prop_ss[which.min(out$k_rank)], 1)
    expect_equal(out$prop_ss[-which.min(out$k_rank)], rep(0.5, 8))
  })

  it("keeps one half as one half when a participant answered 2 of 3 items", {
    # A answered questions 1 (0) and 9 (1) and skipped 13: 1 of 2 = 0.5. B: 0 of 3.
    a <- all_answers("A", 1)
    a$response[a$questionid == 1] <- 0
    a$response[a$questionid == 13] <- NA
    out <- scoring$mean_prop_ss(rbind(a, all_answers("B", 1)))

    expect_equal(out$prop_ss[which.min(out$k_rank)], 0.25)
  })

  it("returns NA at a k rank that no participant answered", {
    a <- all_answers("A", 0)
    a$response[a$questionid %in% c(1, 9, 13)] <- NA
    out <- scoring$mean_prop_ss(a)

    expect_true(is.na(out$prop_ss[which.min(out$k_rank)]))
    expect_equal(out$prop_ss[-which.min(out$k_rank)], rep(1, 8))
  })

  it("does not warn about missing responses, which the app reports itself", {
    a <- all_answers("A", 1)
    a$response[a$questionid == 13] <- NA

    expect_no_warning(scoring$mean_prop_ss(rbind(a, all_answers("B", 1))))
  })

  it("leaves a single participant's proportions as they are", {
    expect_equal(scoring$mean_prop_ss(all_answers("A", 0))$prop_ss, rep(1, 9))
  })

  it("returns a prop_ss_output with k_rank, so the existing plot method applies", {
    out <- scoring$mean_prop_ss(rbind(all_answers("A", 0), all_answers("B", 1)))

    expect_s3_class(out, "prop_ss_output")
    expect_named(out, c("k_rank", "prop_ss"))
    expect_s3_class(plot(out), "ggplot")
  })
})

describe("plot_mean_prop_ss", {
  x_labels <- function(p) {
    ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()
  }
  dat <- rbind(
    data.frame(subjectid = "A", questionid = 1:27, response = 0),
    data.frame(subjectid = "B", questionid = 1:27, response = 1)
  )

  it("labels the x axis with the 9 k values of the 27-Item MCQ", {
    p <- scoring$plot_mean_prop_ss(dat)

    expect_s3_class(p, "ggplot")
    expect_equal(
      x_labels(p),
      c("0.00016", "0.0004", "0.001", "0.0025", "0.006", "0.016", "0.041", "0.1", "0.25")
    )
  })

  it("plots the mean proportions", {
    p <- scoring$plot_mean_prop_ss(dat)

    expect_equal(ggplot2::ggplot_build(p)$data[[1]]$y, rep(0.5, 9))
  })

  it("builds without a message or a warning", {
    expect_no_message(scoring$plot_mean_prop_ss(dat))
    expect_no_warning(scoring$plot_mean_prop_ss(dat))
  })
})
