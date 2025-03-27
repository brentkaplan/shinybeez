box::use(
  assertthat,
  beezdiscounting,
  dplyr,
  readr,
  tidyr,
)

#' @export
check_data <- function(dat, type = "demand") {
  if (type == "demand") {
    if (length(unique(dat$id)) == length(dat$id)) {
      if ("group" %in% colnames(dat)) {
        return_msg <- assertthat$validate_that(
          all(colnames(dat)[1:2] == c("id", "group")),
          msg = "The first two columns do not match `id`, `group`"
        )
        if (is.character(return_msg)) {
          return(return_msg)
        }
        dat <- dplyr$relocate(dat, group, .after = id)
        return_msg <- assertthat$validate_that(
          all(sapply(readr$parse_number(colnames(dat)[3:length(colnames(dat))]), is.numeric)),
          msg = "The column names are not numeric"
        )
        if (is.character(return_msg)) {
          return(return_msg)
        }
      } else {
        return_msg <- assertthat$validate_that(
          colnames(dat)[1] == c("id"),
          msg = "The first column is not `id`"
        )
        if (is.character(return_msg)) {
          return(return_msg)
        }
        return_msg <- assertthat$validate_that(
          all(sapply(readr$parse_number(colnames(dat)[2:length(colnames(dat))]), is.numeric)),
          msg = "The column names are not numeric"
        )
        if (is.character(return_msg)) {
          return(return_msg)
        }
      }
    } else {
      if ("group" %in% colnames(dat)) {
        return_msg <- assertthat$validate_that(
          all(colnames(dat) == c("id", "group", "x", "y")),
          msg = "Check colnames `id`, `group`, `x`, and `y` in data"
        )
      } else {
        return_msg <- assertthat$validate_that(
          all(colnames(dat) == c("id", "x", "y")),
          msg = "Check colnames `id`, `x`, and `y` are ordered in  data"
        )
      }
    }
  } else {
    # check if file has correct id columns
    return_msg <- assertthat$validate_that(
      any(colnames(dat) %in% c("subjectid", "ResponseId", "id", "x", "y")),
      msg = "Check colnames for 'subjectid', 'ResponseId', 'id', 'x', or 'y' are in data"
    )
    if ("subjectid" %in% colnames(dat)) {
      # check if 28 or 3 columns wide
      return_msg <- assertthat$validate_that(
        ncol(dat) == 28 | ncol(dat) == 3,
        msg = "Number of columns does not appear to match the template"
      )
    } else if ("ResponseId" %in% colnames(dat)) {
      # check if columns match the qualtrics template output
      return_msg <- assertthat$validate_that(
        all(paste0("I", c(1:31)) %in% colnames(dat)),
        msg = "Check to make sure you are using the correct Qualtrics template."
      )
    } else if ("id" %in% colnames(dat)) {
      return_msg <- assertthat$validate_that(
        colnames(dat)[1] == c("id"),
        msg = "The first column is not `id`"
      )
      if (is.character(return_msg)) {
        return(return_msg)
      }
      return_msg <- assertthat$validate_that(
        all(sapply(readr$parse_number(colnames(dat)[2:length(colnames(dat))]), is.numeric)),
        msg = "The column names are not numeric"
      )
      if (is.character(return_msg)) {
        return(return_msg)
      }
    }
  }
  if (is.character(return_msg)) {
    return(return_msg)
  } else {
    return(TRUE)
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
      if ("group" %in% colnames(dat)) {
        dat |>
          tidyr$pivot_longer(
            cols = 3:ncol(dat),
            names_to = "x",
            values_to = "y"
          )
      } else {
        dat |>
          tidyr$pivot_longer(
            cols = 2:ncol(dat),
            names_to = "x",
            values_to = "y"
          )
      }
    } else {
      dat
    }
  } else if (type == "discounting") {
    if (ncol(dat) == 28) {
      dat |>
        beezdiscounting$wide_to_long_mcq(dat = _)
    } else if (ncol(dat) < 28 & length(unique(dat$id)) == length(dat$id)) {
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
  if (class(dat$x) != "numeric") dat$x <- readr$parse_number(dat$x)
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
k_values <- c(1.5, 2, 2.5, 3, 3.5, 4)
