#' Reshape spec: what the user told us about a wide file
#'
#' A spec is a plain list so it can be built from Shiny inputs, logged in
#' telemetry summaries, and tested without a session.

box::use(
  readr[parse_number],
  stats[complete.cases],
  utils[head],
)

targets <- c("demand", "mixed_effects_demand", "discounting")

#' Names a carried (keep_cols) column may not have: the mixed-effects output columns,
#' the pipeline's model column names, and apply_spec()'s temporaries. Only carried columns
#' can clash - the group column is renamed to `group` on output, so a file column named
#' "group" is a fine group column on demand and a fine covariate on mixed effects.
#' @export
RESERVED <- c("id", "x", "y", "series", "monkey", "y_ll4", ".row", ".series") # nolint: object_name_linter

#' @export
new_series <- function(cols, x = NULL, label = "") {
  list(label = if (is.null(label)) "" else label, cols = as.character(cols), x = x)
}

#' @export
new_spec <- function(target, id_col, series = list(), group_col = NULL,
                     keep_cols = character(0), x_source = "header", drop_na = TRUE,
                     layout = "wide", x_col = NULL, y_col = NULL) {
  stopifnot(target %in% targets, layout %in% c("wide", "long"))
  if (layout == "long") {
    # x_source describes how the wide pivot learns its prices; a long frame already has
    # them in a column, and "none" keeps the telemetry summary honest.
    stopifnot(length(series) == 0)
    x_source <- "none"
  } else {
    stopifnot(x_source %in% c("header", "manual"))
  }
  list(
    target = target,
    layout = layout,
    id_col = id_col,
    x_col = x_col,
    y_col = y_col,
    group_col = group_col,
    keep_cols = as.character(keep_cols),
    series = series,
    x_source = x_source,
    drop_na = isTRUE(drop_na)
  )
}

#' Parse a pasted vector of prices/delays
#' @param text "0, $0.50; 1\n5 10" and friends
#' @return numeric, NA for tokens that are not numbers, numeric(0) for empty input
#' @export
parse_x_text <- function(text) {
  if (is.null(text) || length(text) == 0 || is.na(text[1]) || !nzchar(trimws(text[1]))) {
    return(numeric(0))
  }
  tokens <- strsplit(trimws(text[1]), "[,;[:space:]]+")[[1]]
  tokens <- tokens[nzchar(tokens)]
  suppressWarnings(parse_number(tokens))
}

#' Parse response cells to numbers: numeric columns as-is, text via parse_number()
#'
#' Inf and NaN are missing responses, not responses: nothing downstream can fit them,
#' and counting them as usable would let a participant pass the two-response rule with
#' nothing to fit.
#' @export
parse_cells <- function(v) {
  out <- if (is.numeric(v)) as.numeric(v) else suppressWarnings(parse_number(as.character(v)))
  out[!is.finite(out)] <- NA_real_
  out
}

quote_names <- function(x) paste0("\"", x, "\"", collapse = ", ")

# A row with no group belongs to no curve: the shared complete-case cleanup drops it after
# the preview has already counted it, so refuse it here instead.
group_values_ok <- function(spec, dat) {
  if (is.null(spec$group_col)) return(TRUE)
  values <- as.character(dat[[spec$group_col]])
  if (anyNA(values) || any(!nzchar(trimws(values)))) {
    return(paste0(
      "The group column ", quote_names(spec$group_col), " has empty values; every row needs one."
    ))
  }
  TRUE
}

x_noun <- function(target) if (target == "discounting") "delays" else "prices"

series_name <- function(s, i) if (nzchar(trimws(s$label))) s$label else paste("Series", i)

# Response cells of one series as a numeric vector, column-major (all of cols[1], then cols[2], ...)
series_cells <- function(s, dat) {
  unlist(lapply(s$cols, function(cn) parse_cells(dat[[cn]])), use.names = FALSE)
}

#' Validate a spec against the frame it will be applied to
#' @return TRUE or a user-facing character message (the first failure)
#' @export
validate_spec <- function(spec, dat) {
  if (identical(spec$layout, "long")) validate_long_spec(spec, dat) else validate_wide_spec(spec, dat)
}

x_col_label <- function(target) if (target == "discounting") "delay column" else "price column"

y_col_label <- function(target) {
  if (target == "discounting") "indifference point column" else "consumption column"
}

chosen <- function(v) !(is.null(v) || length(v) != 1 || is.na(v) || !nzchar(v))

# The group/carried column rules, shared by both layouts.
validate_extra_cols <- function(spec, dat, role_cols) {
  cols <- colnames(dat)
  extra <- c(spec$group_col, spec$keep_cols)
  missing <- setdiff(extra, cols)
  if (length(missing) > 0) {
    return(paste0("These columns are not in the data: ", quote_names(missing), "."))
  }
  reserved <- intersect(spec$keep_cols, RESERVED)
  if (length(reserved) > 0) {
    return(paste0(
      "Columns named ", quote_names(reserved), " cannot be carried along; they clash ",
      "with the output columns. Rename them in the file first."
    ))
  }
  if (spec$target != "mixed_effects_demand" && length(spec$keep_cols) > 0) {
    return("Only the mixed-effects tab can carry extra columns.")
  }
  if (spec$target == "discounting" && !is.null(spec$group_col)) {
    return("Indifference point data cannot carry a group column.")
  }
  chk <- group_values_ok(spec, dat)
  if (is.character(chk)) {
    return(chk)
  }
  clash <- intersect(extra, role_cols)
  if (length(clash) > 0) {
    return(paste0(
      quote_names(clash), " cannot be a group or carried column and also an id, ",
      x_col_label(spec$target), " or ", y_col_label(spec$target), "."
    ))
  }
  TRUE
}

# A frame that is already one row per observation: three named columns, no pivot.
validate_long_spec <- function(spec, dat) {
  cols <- colnames(dat)
  if (nrow(dat) == 0) {
    return("The file has no data rows.")
  }
  if (!chosen(spec$id_col)) {
    return("Choose the column that identifies each participant.")
  }
  if (!chosen(spec$x_col)) {
    return(paste0("Choose the ", x_col_label(spec$target), "."))
  }
  if (!chosen(spec$y_col)) {
    return(paste0("Choose the ", y_col_label(spec$target), "."))
  }

  roles <- c(spec$id_col, spec$x_col, spec$y_col)
  absent <- setdiff(roles, cols)
  if (length(absent) > 0) {
    return(paste0("These columns are not in the data: ", quote_names(absent), "."))
  }
  if (anyDuplicated(roles) > 0) {
    return(paste0(
      "The id, ", x_col_label(spec$target), " and ", y_col_label(spec$target),
      " must be three different columns; one cannot be used twice."
    ))
  }
  chk <- validate_extra_cols(spec, dat, roles)
  if (is.character(chk)) {
    return(chk)
  }

  ids <- as.character(dat[[spec$id_col]])
  if (anyNA(ids) || any(!nzchar(trimws(ids)))) {
    return(paste0("The id column ", quote_names(spec$id_col), " has empty values."))
  }

  x <- parse_cells(dat[[spec$x_col]])
  n_bad <- sum(is.na(x) | !is.finite(x))
  if (n_bad > 0) {
    return(sprintf(
      "Every row needs a number in the %s %s; %d row%s do not.",
      x_col_label(spec$target), quote_names(spec$x_col), n_bad, if (n_bad == 1) "" else "s"
    ))
  }
  if (spec$target == "discounting" && any(x <= 0)) {
    return(paste0("Delays must be greater than zero; ", quote_names(spec$x_col), " has values at or below zero."))
  }
  if (spec$target != "discounting" && any(x < 0)) {
    return(paste0("Prices cannot be negative; ", quote_names(spec$x_col), " has negative values."))
  }

  y <- parse_cells(dat[[spec$y_col]])
  if (all(is.na(y))) {
    return(paste0(
      "None of the values in the ", y_col_label(spec$target), " ", quote_names(spec$y_col),
      " contain numbers."
    ))
  }

  # A participant may repeat a price in another group or session; without a group column
  # the repeat is a duplicate row.
  key <- paste(ids, x, sep = "\r")
  if (!is.null(spec$group_col)) key <- paste(key, as.character(dat[[spec$group_col]]), sep = "\r")
  dupes <- unique(ids[duplicated(key)])
  if (length(dupes) > 0) {
    return(paste0(
      "Some participants have the same ", sub(" column$", "", x_col_label(spec$target)),
      " twice: ", quote_names(head(dupes, 10)), if (length(dupes) > 10) ", \u2026" else "",
      ". If each participant has several sessions or conditions, choose the column that ",
      "distinguishes them."
    ))
  }

  # One curve per participant, or per participant x group when a group column is chosen:
  # two points in two different groups are two one-point curves, not a fittable pair.
  curve <- ids
  if (!is.null(spec$group_col)) curve <- paste(ids, as.character(dat[[spec$group_col]]), sep = "\r")
  keep <- if (spec$drop_na) !is.na(y) else rep(TRUE, length(y))
  per_curve <- table(curve[keep])
  short <- setdiff(unique(curve), names(per_curve)[per_curve >= 2])
  if (length(short) > 0) {
    short_ids <- unique(ids[curve %in% short])
    return(paste0(
      "These ids have fewer than two usable responses: ",
      quote_names(head(short_ids, 10)), if (length(short_ids) > 10) ", \u2026" else "",
      ". Each participant needs at least two",
      if (!is.null(spec$group_col)) " in every group" else "", "."
    ))
  }
  TRUE
}

validate_wide_spec <- function(spec, dat) {
  cols <- colnames(dat)
  if (nrow(dat) == 0) {
    return("The file has no data rows.")
  }

  # id
  if (is.null(spec$id_col) || length(spec$id_col) != 1 || is.na(spec$id_col) || !nzchar(spec$id_col)) {
    return("Choose the column that identifies each participant.")
  }
  if (!spec$id_col %in% cols) {
    return(paste0("The id column ", quote_names(spec$id_col), " is not in the data."))
  }
  ids <- as.character(dat[[spec$id_col]])
  if (anyNA(ids) || any(!nzchar(trimws(ids)))) {
    return(paste0("The id column ", quote_names(spec$id_col), " has empty values."))
  }

  # group / keep columns
  extra <- c(spec$group_col, spec$keep_cols)
  missing <- setdiff(extra, cols)
  if (length(missing) > 0) {
    return(paste0("These columns are not in the data: ", quote_names(missing), "."))
  }
  reserved <- intersect(spec$keep_cols, RESERVED)
  if (length(reserved) > 0) {
    return(paste0(
      "Columns named ", quote_names(reserved), " cannot be carried along; they clash ",
      "with the output columns. Rename them in the file first."
    ))
  }
  if (spec$target != "mixed_effects_demand" && length(spec$keep_cols) > 0) {
    return("Only the mixed-effects tab can carry extra columns.")
  }
  if (spec$target == "discounting" && !is.null(spec$group_col)) {
    return("Indifference point data cannot carry a group column.")
  }
  chk <- group_values_ok(spec, dat)
  if (is.character(chk)) {
    return(chk)
  }

  # series
  if (length(spec$series) == 0) {
    return("Add at least one set of response columns.")
  }
  if (spec$target == "discounting" && length(spec$series) != 1) {
    return("Indifference point data can hold one set of delay columns.")
  }
  role_cols <- c(spec$id_col, extra)
  seen <- character(0)
  for (i in seq_along(spec$series)) {
    s <- spec$series[[i]]
    nm <- series_name(s, i)
    if (length(s$cols) < 2) {
      return(paste0(nm, ": choose at least two response columns."))
    }
    absent <- setdiff(s$cols, cols)
    if (length(absent) > 0) {
      return(paste0(nm, ": these columns are not in the data: ", quote_names(absent), "."))
    }
    clash <- intersect(s$cols, c(role_cols, seen))
    if (length(clash) > 0) {
      return(paste0(
        nm, ": ", quote_names(clash),
        " cannot be a response column and also an id, group, carried, or other series column."
      ))
    }
    seen <- c(seen, s$cols)
    x <- s$x
    if (length(x) != length(s$cols)) {
      return(sprintf(
        "%s: %d columns selected but %d %s entered.", nm, length(s$cols), length(x), x_noun(spec$target)
      ))
    }
    if (!is.numeric(x) || anyNA(x) || any(!is.finite(x))) {
      return(paste0(nm, ": every one of the ", x_noun(spec$target), " must be a number."))
    }
    if (anyDuplicated(x) > 0) {
      return(paste0(
        nm, ": ", x_noun(spec$target), " must be unique; duplicated: ",
        quote_names(unique(x[duplicated(x)])), "."
      ))
    }
    if (spec$target == "discounting" && any(x <= 0)) {
      return(paste0(nm, ": delays must be greater than zero."))
    }
    if (spec$target != "discounting" && any(x < 0)) {
      return(paste0(nm, ": prices cannot be negative."))
    }
  }

  # one row per id (x group)
  key <- ids
  if (!is.null(spec$group_col)) key <- paste(ids, as.character(dat[[spec$group_col]]), sep = "\r")
  if (anyDuplicated(key) > 0) {
    return(paste0(
      "Rows do not have unique ids. If you have several sessions per participant, ",
      "choose the column that distinguishes them as the group column."
    ))
  }

  # series labels
  if (length(spec$series) > 1) {
    labels <- vapply(spec$series, function(s) trimws(s$label), character(1))
    if (any(!nzchar(labels))) {
      return("Give every series a name; it becomes the group label.")
    }
    if (anyDuplicated(labels) > 0) {
      return(paste0("Series names must be unique; duplicated: ", quote_names(unique(labels[duplicated(labels)])), "."))
    }
    if (spec$target == "demand" && !is.null(spec$group_col)) {
      return("With more than one series the series names become the groups, so a separate group column cannot be used.")
    }
  }

  # cells: numbers present, and every id keeps >= 2 responses per series
  for (i in seq_along(spec$series)) {
    s <- spec$series[[i]]
    nm <- series_name(s, i)
    ymat <- matrix(series_cells(s, dat), nrow = nrow(dat))
    if (all(is.na(ymat))) {
      return(paste0(nm, ": none of the selected columns contain numbers."))
    }
    short <- unique(ids[rowSums(!is.na(ymat)) < 2])
    if (length(short) > 0) {
      return(paste0(
        nm, ": these ids have fewer than two usable responses: ",
        quote_names(head(short, 10)), if (length(short) > 10) ", …" else "",
        ". Each participant needs at least two."
      ))
    }
  }

  TRUE
}

output_columns <- function(spec, long) {
  switch(
    spec$target,
    demand = c("id", if ("group" %in% names(long)) "group", "x", "y"),
    discounting = c("id", "x", "y"),
    mixed_effects_demand = c("id", "x", "y", if ("series" %in% names(long)) "series", spec$keep_cols)
  )
}

#' Apply a validated spec: one row per participant x series x price
#' @return list(data, losses = list(n_na_y, n_na_keep), n_ids)
#' @export
apply_spec <- function(spec, dat) {
  chk <- validate_spec(spec, dat)
  if (is.character(chk)) stop(chk, call. = FALSE)
  if (identical(spec$layout, "long")) apply_long_spec(spec, dat) else apply_wide_spec(spec, dat)
}

# Already one row per observation: select, rename and parse. The file's row order stands.
apply_long_spec <- function(spec, dat) {
  ids <- as.character(dat[[spec$id_col]])
  long <- data.frame(
    id = ids,
    x = parse_cells(dat[[spec$x_col]]),
    y = parse_cells(dat[[spec$y_col]]),
    stringsAsFactors = FALSE
  )
  if (!is.null(spec$group_col)) {
    if (spec$target == "demand") long$group <- as.character(dat[[spec$group_col]])
    if (spec$target == "mixed_effects_demand") long$series <- as.character(dat[[spec$group_col]])
  }
  if (spec$target == "mixed_effects_demand") {
    for (kc in spec$keep_cols) long[[kc]] <- dat[[kc]]
  }

  n_na_y <- sum(is.na(long$y))
  if (spec$drop_na) long <- long[!is.na(long$y), , drop = FALSE]
  n_na_keep <- if (length(spec$keep_cols) > 0) {
    sum(!complete.cases(long[, spec$keep_cols, drop = FALSE]))
  } else {
    0L
  }
  long <- long[, output_columns(spec, long), drop = FALSE]
  rownames(long) <- NULL

  list(
    data = long,
    losses = list(n_na_y = as.integer(n_na_y), n_na_keep = as.integer(n_na_keep)),
    n_ids = length(unique(ids))
  )
}

apply_wide_spec <- function(spec, dat) {
  n <- nrow(dat)
  ids <- as.character(dat[[spec$id_col]])
  multi <- length(spec$series) > 1

  pieces <- lapply(seq_along(spec$series), function(i) {
    s <- spec$series[[i]]
    k <- length(s$cols)
    out <- data.frame(
      id = rep(ids, times = k),
      x = rep(s$x, each = n),
      y = series_cells(s, dat),
      .row = rep(seq_len(n), times = k),
      .series = i,
      stringsAsFactors = FALSE
    )
    if (spec$target == "demand") {
      if (multi) {
        out$group <- s$label
      } else if (!is.null(spec$group_col)) {
        out$group <- rep(as.character(dat[[spec$group_col]]), times = k)
      }
    }
    if (spec$target == "mixed_effects_demand") {
      if (multi) out$series <- s$label
      for (kc in spec$keep_cols) out[[kc]] <- rep(dat[[kc]], times = k)
    }
    out
  })

  long <- do.call(rbind, pieces)
  long <- long[order(long$.row, long$.series, long$x), , drop = FALSE]
  long$.row <- NULL
  long$.series <- NULL

  n_na_y <- sum(is.na(long$y))
  if (spec$drop_na) long <- long[!is.na(long$y), , drop = FALSE]
  n_na_keep <- if (length(spec$keep_cols) > 0) {
    sum(!complete.cases(long[, spec$keep_cols, drop = FALSE]))
  } else {
    0L
  }
  long <- long[, output_columns(spec, long), drop = FALSE]
  rownames(long) <- NULL

  list(
    data = long,
    losses = list(n_na_y = as.integer(n_na_y), n_na_keep = as.integer(n_na_keep)),
    n_ids = length(unique(ids))
  )
}
