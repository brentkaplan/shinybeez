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

#' Validate demand data (wide or long format)
#' @param dat Data frame
#' @return TRUE or character error message
check_demand_data <- function(dat) {
  if (length(unique(dat$id)) == length(dat$id)) {
    # Wide format
    if ("group" %in% colnames(dat)) {
      chk <- validate_condition(
        all(colnames(dat)[1:2] == c("id", "group")),
        "The first two columns do not match `id`, `group`"
      )
      if (is.character(chk)) return(chk)
      dat <- dplyr$relocate(dat, group, .after = id)
      chk <- validate_condition(
        all(!is.na(
          readr$parse_number(colnames(dat)[3:length(colnames(dat))])
        )),
        "The column names are not numeric"
      )
      if (is.character(chk)) return(chk)
    } else {
      chk <- validate_condition(
        colnames(dat)[1] == "id",
        "The first column is not `id`"
      )
      if (is.character(chk)) return(chk)
      chk <- validate_condition(
        all(!is.na(
          readr$parse_number(colnames(dat)[2:length(colnames(dat))])
        )),
        "The column names are not numeric"
      )
      if (is.character(chk)) return(chk)
    }
  } else {
    # Long format
    if ("group" %in% colnames(dat)) {
      chk <- validate_condition(
        all(colnames(dat) == c("id", "group", "x", "y")),
        "Check colnames `id`, `group`, `x`, and `y` in data"
      )
      if (is.character(chk)) return(chk)
    } else {
      chk <- validate_condition(
        all(colnames(dat) == c("id", "x", "y")),
        "Check colnames `id`, `x`, and `y` are ordered in  data"
      )
      if (is.character(chk)) return(chk)
    }
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
    chk <- validate_condition(
      all(paste0("i", c(1:31)) %in% colnames(dat)),
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
  # check if dat is wider than it is long
  if (length(unique(dat$id)) == length(dat$id)) {
    lcols <- length(colnames(dat))
    if ("group" %in% colnames(dat)) {
      dat <- dplyr$relocate(dat, group, .after = id)
      colnames(dat)[3:lcols] <- readr$parse_number(colnames(dat)[3:lcols])
    } else {
      colnames(dat)[2:lcols] <- readr$parse_number(colnames(dat)[2:lcols])
    }
  }
  dat
}

#' @export
reshape_data <- function(dat, type = "demand") {
  if (type == "demand") {
    # check if dat is wider than it is long
    if (length(unique(dat$id)) == length(dat$id)) {
      pivot_demand_data(dat, format = "long", drop_na = FALSE)
    } else {
      dat
    }
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
