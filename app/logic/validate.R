box::use(
  assertthat,
  beezdemand[pivot_demand_data],
  beezdiscounting,
  dplyr,
  readr,
  tidyr,
  stats
)

#' Validate a single assertion, returning error message or TRUE
#' @param condition Logical condition
#' @param msg Error message if condition fails
#' @return TRUE if valid, or character error message
validate_condition <- function(condition, msg) {
  result <- assertthat$validate_that(condition, msg = msg)
  if (is.character(result)) return(result)
  TRUE
}

#' Classify a demand frame as wide or long
#'
#' The format is a property of the column NAMES, never of the rows. Deciding it
#' from id-uniqueness (the old heuristic) is unsafe: `check_data()` runs before
#' `remove_na_rows()` while `rename_cols()`/`reshape_data()` run on the stored,
#' post-removal frame (file_input.R:95/:111/:135). Dropping rows can make
#' duplicated ids unique, flipping an already-validated long file into the wide
#' branch, where `parse_number()` turns the literal headers `x`/`y` into `NA` and
#' the frame ends up with columns named "NA".
#'
#' @param dat Data frame
#' @return "long" or "wide"
#' @export
demand_format <- function(dat) {
  nms <- colnames(dat)
  if (identical(nms, c("id", "x", "y")) ||
        identical(nms, c("id", "group", "x", "y"))) {
    return("long")
  }
  "wide"
}

#' Positions of the price columns in a wide demand frame
#' @param dat Data frame
#' @return Integer vector, empty if there are no price columns
price_col_index <- function(dat) {
  start <- if ("group" %in% colnames(dat)) 3L else 2L
  n <- length(colnames(dat))
  if (n < start) return(integer(0))
  seq.int(start, n)
}

#' Validate the price headers of a wide demand frame
#'
#' Single source of truth for header validation, so the exported `rename_cols()`
#' and `reshape_data()` cannot be called directly to bypass the guard.
#'
#' @param dat Data frame in wide format
#' @return TRUE or character error message naming the offending headers
#' @export
check_price_headers <- function(dat) {
  idx <- price_col_index(dat)
  if (length(idx) == 0) {
    return("There are no price columns in the data.")
  }

  headers <- colnames(dat)[idx]
  # An unparseable header is an expected, handled condition here - the warning
  # readr emits for it is noise, so it is suppressed at the parse call only.
  parsed <- suppressWarnings(readr$parse_number(headers))

  bad <- headers[is.na(parsed)]
  if (length(bad) > 0) {
    return(paste0(
      "The column names are not numeric. Could not parse these price headers: ",
      paste0("\"", bad, "\"", collapse = ", "), "."
    ))
  }

  dupes <- unique(parsed[duplicated(parsed)])
  if (length(dupes) > 0) {
    return(paste0(
      "Duplicate price columns. These headers all resolve to the same price: ",
      paste0("\"", headers[parsed %in% dupes], "\"", collapse = ", "),
      ". Please give each price a single column."
    ))
  }

  TRUE
}

#' Validate demand data (wide or long format)
#' @param dat Data frame
#' @return TRUE or character error message
check_demand_data <- function(dat) {
  if (demand_format(dat) == "wide") {
    if ("group" %in% colnames(dat)) {
      chk <- validate_condition(
        all(colnames(dat)[1:2] == c("id", "group")),
        "The first two columns do not match `id`, `group`"
      )
      if (is.character(chk)) return(chk)
      dat <- dplyr$relocate(dat, group, .after = id)
    } else {
      chk <- validate_condition(
        colnames(dat)[1] == "id",
        "The first column is not `id`"
      )
      if (is.character(chk)) return(chk)
    }
    chk <- check_price_headers(dat)
    if (is.character(chk)) return(chk)
  }
  TRUE
}

#' Check every id retains enough price points to fit a demand curve
#'
#' Must run on the STORED, post-`remove_na_rows()` frame. A single observation per
#' participant cannot support an individual curve - the systematicity code divides
#' by `nrow(adf) - 1` and by a zero price range - so accepting it only defers the
#' failure into beezdemand.
#'
#' @param dat Long-format demand data frame
#' @return TRUE or character error message naming the offending ids
#' @export
check_demand_sufficiency <- function(dat) {
  if (demand_format(dat) == "wide") {
    # One price column gives every participant a single point once reshaped.
    if (length(price_col_index(dat)) < 2) {
      return(paste0(
        "The data have fewer than two price columns. Each participant needs at ",
        "least two price points to fit a demand curve."
      ))
    }
    return(TRUE)
  }

  # Count distinct PARSED prices, mirroring retype_data(): "$1" and "1.00" are
  # two raw strings but one price, and would otherwise be counted as two.
  prices <- dat$x
  if (!is.numeric(prices)) {
    prices <- suppressWarnings(readr$parse_number(as.character(prices)))
  }
  distinct_prices <- tapply(
    prices, dat$id, function(v) length(unique(v[!is.na(v)]))
  )
  short <- names(distinct_prices)[distinct_prices < 2]
  if (length(short) > 0) {
    return(paste0(
      "After removing missing values these ids have fewer than two distinct ",
      "price points: ", paste0("\"", short, "\"", collapse = ", "),
      ". Each participant needs at least two price points to fit a demand curve."
    ))
  }
  TRUE
}

#' Validate mixed effects demand data
#' @param dat Data frame
#' @return TRUE or character error message
check_mixed_effects_data <- function(dat) {
  chk <- validate_condition(
    any(c("monkey", "id") %in% colnames(dat)),
    "ID column ('monkey' or 'id') not found."
  )
  if (is.character(chk)) return(chk)

  chk <- validate_condition(
    "x" %in% colnames(dat),
    "Price/X column ('x') not found."
  )
  if (is.character(chk)) return(chk)

  chk <- validate_condition(
    any(c("y", "y_ll4") %in% colnames(dat)),
    "Consumption column ('y' or 'y_ll4') not found."
  )
  if (is.character(chk)) return(chk)

  TRUE
}

#' Validate discounting data
#' @param dat Data frame
#' @return TRUE or character error message
check_discounting_data <- function(dat) {
  chk <- validate_condition(
    any(colnames(dat) %in% c("subjectid", "responseid", "id", "x", "y")),
    "Check colnames for 'subjectid', 'responseid', 'id', 'x', or 'y' are in data"
  )
  if (is.character(chk)) return(chk)

  if ("subjectid" %in% colnames(dat)) {
    chk <- validate_condition(
      ncol(dat) == 28 || ncol(dat) == 3,
      "Number of columns does not appear to match the template"
    )
    if (is.character(chk)) return(chk)
  } else if ("responseid" %in% colnames(dat)) {
    i_cols <- grep("^i[0-9]+$", colnames(dat), value = TRUE)
    chk <- validate_condition(
      length(i_cols) >= 1,
      "Check to make sure you are using the correct Qualtrics template."
    )
    if (is.character(chk)) return(chk)
  } else if ("id" %in% colnames(dat)) {
    chk <- validate_condition(
      identical(colnames(dat), c("id", "x", "y")),
      "Indifference point data must have exactly three columns: id, x, y"
    )
    if (is.character(chk)) return(chk)
  }

  TRUE
}

#' @export
check_data <- function(dat, type = "demand") {
  # Normalize column names: trim whitespace and lowercase
  # tryCatch protects against non-UTF8 column names from corrupted files
  normalized <- tryCatch(
    trimws(tolower(colnames(dat))),
    error = function(e) NULL
  )
  if (is.null(normalized)) {
    return("Column names contain invalid characters. Is this a valid data file?")
  }
  colnames(dat) <- normalized

  if (type == "demand") {
    check_demand_data(dat)
  } else if (type == "mixed_effects_demand") {
    check_mixed_effects_data(dat)
  } else {
    check_discounting_data(dat)
  }
}

#' Does this discounting format use all-NA columns structurally?
#'
#' Qualtrics 5.5-Trial: an unanswered branch of the adaptive tree is an entirely
#' empty item column, but beezdiscounting still selects all of I1-I31 by name.
#' MCQ: an all-NA question would break the `ncol(dat) == 28` format detection.
#' For both, dropping empty columns destroys structure rather than noise.
#'
#' @param dat Data frame with lowercased column names
#' @return TRUE if empty columns must be preserved
#' @export
preserves_empty_cols <- function(dat) {
  any(c("responseid", "subjectid") %in% colnames(dat))
}

#' @export
obliterate_empty_cols <- function(dat) {
  # figure out which columns are empty
  empty_cols <- colnames(dat)[colSums(is.na(dat)) == nrow(dat)]
  # remove empty columns
  dat <- dat[, !colnames(dat) %in% empty_cols]
  return(dat)
}

#' @export
rename_cols <- function(dat) {
  if (demand_format(dat) == "long") return(dat)

  chk <- check_price_headers(dat)
  if (is.character(chk)) stop(chk, call. = FALSE)

  if ("group" %in% colnames(dat)) {
    dat <- dplyr$relocate(dat, group, .after = id)
  }
  idx <- price_col_index(dat)
  colnames(dat)[idx] <- suppressWarnings(
    readr$parse_number(colnames(dat)[idx])
  )
  dat
}

#' @export
reshape_data <- function(dat, type = "demand") {
  if (type == "demand") {
    if (demand_format(dat) == "long") {
      return(dat)
    }
    # Guard here too: reshape_data() is exported and can be called without ever
    # going through check_data(), which would hand an "NA" column to beezdemand.
    chk <- check_price_headers(dat)
    if (is.character(chk)) stop(chk, call. = FALSE)
    pivot_demand_data(dat, format = "long", drop_na = FALSE)
  } else if (type == "discounting") {
    if (ncol(dat) == 28) {
      dat |>
        beezdiscounting$wide_to_long_mcq(dat = _)
    } else if (ncol(dat) < 28 && length(unique(dat$id)) == length(dat$id)) {
      dat |>
        tidyr$pivot_longer(
          cols = 2:ncol(dat),
          names_to = "x",
          values_to = "y"
        )
    } else {
      dat
    }
  }
}

#' Shape a discounting upload into the frame the analysis functions expect
#'
#' Mirrors the branches the discounting navpanel used to inline, with one correction: a
#' long `id`/`x`/`y` upload is now numeric-coerced like every other shape. Previously it fell
#' into a bare `else` that stored the frame verbatim, so a character `y` (quoted numbers, a
#' blank, a stray text cell) reached `beezdiscounting::check_unsystematic`, which does
#' arithmetic on it — production errors 2a0fc474 / 46abab75 / 11dc772e.
#'
#' The coercion is shape-gated on purpose. The final branch is a catch-all that also receives
#' Qualtrics 5.5-Trial uploads (ResponseId / I1-I31, no id/x/y at all); those must pass
#' through untouched.
#'
#' @param dat Data frame as uploaded.
#' @return Data frame ready for the discounting analyses.
#' @export
prepare_discounting_data <- function(dat) {
  cols <- colnames(dat)

  # 28 columns is the MCQ shape.
  if (ncol(dat) == 28) {
    return(reshape_data(dat, type = "discounting"))
  }

  # id-first and wide: reshape to long, then coerce.
  if (identical(cols[1], "id") && ncol(dat) > 3) {
    return(retype_data(reshape_data(dat, type = "discounting")))
  }

  # Already long. This is the branch that used to skip coercion entirely.
  if (all(c("id", "x", "y") %in% cols)) {
    return(retype_data(dat))
  }

  dat
}

#' @export
retype_data <- function(dat) {
  if (!is.numeric(dat$x)) {
    dat$x <- readr$parse_number(dat$x)
  }
  if ("group" %in% colnames(dat)) {
    dat |>
      dplyr$mutate(
        id = as.factor(id),
        group = as.factor(group),
        x = as.numeric(x),
        y = as.numeric(y)
      )
  } else {
    dat |>
      dplyr$mutate(
        id = as.factor(id),
        x = as.numeric(x),
        y = as.numeric(y)
      )
  }
}

#' @export
remove_na_rows <- function(dat) {
  n_before <- nrow(dat)
  dat_clean <- dat[stats::complete.cases(dat), ]
  n_after <- nrow(dat_clean)
  n_dropped <- n_before - n_after
  list(data = dat_clean, n_dropped = n_dropped)
}

#' @export
k_values <- c(1.5, 2, 2.5, 3, 3.5, 4)
