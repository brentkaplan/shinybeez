#' Prefill heuristics for the wide-to-long mapper
#'
#' Every guess is a starting point the user can change; validate_spec() is the
#' only gate. Designed to fail towards "ask the user" rather than invent prices.

box::use(
  app / logic / validate[parse_header_number],
  . / spec[new_series, new_spec, parse_cells],
)

`%||%` <- function(a, b) if (is.null(a)) b else a

id_pattern <- "^(response_?id|subject_?id|subject|subj|participant|pid|ppt|id)$"
x_name_pattern <- "^(x|price|prices|cost|delay|delays|amount)$"
y_name_pattern <- "^(y|y_ll4|consumption|consumed|response|value|indiff|indifference|ip)$"
group_name_pattern <- "^(group|series|condition|cond|commodity|drug|session|site|wave|arm)$"
suffix_pattern <- "^(.*?)[_. -]?([0-9]+(\\.[0-9]+)?)$"

#' @export
guess_id_col <- function(dat) {
  nms <- colnames(dat)
  hit <- nms[grepl(id_pattern, tolower(nms), perl = TRUE)]
  if (length(hit) > 0) return(hit[1])
  for (nm in nms) {
    v <- dat[[nm]]
    if (!anyNA(v) && anyDuplicated(v) == 0) return(nm)
  }
  NA_character_
}

#' Share of non-missing cells that are whole-cell numbers (optional currency symbol)
#'
#' Uses the header grammar, not readr::parse_number(): "2026-01-01" and "R_1"
#' contain digits but are not responses, and must not pull a column into a series.
#' @export
numeric_share <- function(v) {
  v <- v[!is.na(v)]
  if (length(v) == 0) return(0)
  if (is.numeric(v)) return(1)
  mean(!is.na(parse_header_number(as.character(v))))
}

#' Split "prefix_<number>" headers
#' @return data.frame(name, prefix, suffix); NA prefix/suffix where there is no trailing number
#' @export
split_suffix <- function(nms) {
  m <- regmatches(nms, regexec(suffix_pattern, nms, perl = TRUE))
  prefix <- vapply(m, function(z) if (length(z) >= 3) z[2] else NA_character_, character(1))
  suffix <- vapply(m, function(z) if (length(z) >= 3) as.numeric(z[3]) else NA_real_, numeric(1))
  data.frame(name = nms, prefix = prefix, suffix = suffix, stringsAsFactors = FALSE)
}

#' Prices/delays implied by column names: whole-header numbers, else trailing numbers
#' @export
x_from_names <- function(cols) {
  whole <- parse_header_number(cols)
  if (length(cols) > 0 && all(!is.na(whole))) return(whole)
  split_suffix(cols)$suffix
}

# Consecutive whole numbers in column order (1..n, but also 3..10 from a Qualtrics
# export whose first items were dropped) are item positions, not prices or delays.
is_index_run <- function(suffix) {
  length(suffix) >= 2 && !anyNA(suffix) && all(suffix == round(suffix)) && all(diff(suffix) == 1)
}

#' Whether "Read from column names" should be offered for these columns
#'
#' A header carries a real price/delay when the whole header is a number, or when its
#' trailing number is not merely a sequential item index: `apt_1, apt_2, …, apt_5` suffix-
#' parses to 1, 2, 3, 4, 5, but that is the item's position, not a price, and mirrors the
#' guess `cluster_series_columns()` makes for the same pattern (`x_source = "manual"`).
#' @export
header_x_available <- function(cols) {
  if (length(cols) == 0) return(FALSE)
  whole <- parse_header_number(cols)
  if (all(!is.na(whole))) return(TRUE)
  x <- x_from_names(cols)
  !anyNA(x) && !is_index_run(x)
}

#' Whether header prices/delays can be read for EVERY series
#'
#' Each series is judged on its own: `alc_1..alc_3` and `cig_1..cig_3` are two item-index
#' runs, but flattened together their suffixes (1, 2, 3, 1, 2, 3) are not one run.
#' @param cols_list list of character vectors, one per series
#' @export
series_header_x_available <- function(cols_list) {
  length(cols_list) > 0 && all(vapply(cols_list, header_x_available, logical(1)))
}

#' Group candidate response columns into series
#' @param exclude Columns already used (id, etc.)
#' @param min_numeric Minimum share of numeric cells for a column to count
#' @return list of list(label, cols, x, x_source), largest cluster first
#' @export
cluster_series_columns <- function(dat, exclude = character(0), min_numeric = 0.8) {
  nms <- setdiff(colnames(dat), exclude)
  nms <- nms[vapply(nms, function(nm) numeric_share(dat[[nm]]) >= min_numeric, logical(1))]
  if (length(nms) == 0) return(list())

  clusters <- list()
  whole <- parse_header_number(nms)
  header_cols <- nms[!is.na(whole)]
  if (length(header_cols) >= 2) {
    clusters[[length(clusters) + 1]] <- list(
      label = "", cols = header_cols, x = whole[!is.na(whole)], x_source = "header"
    )
  }

  parts <- split_suffix(nms[is.na(whole)])
  parts <- parts[!is.na(parts$suffix) & nzchar(parts$prefix), , drop = FALSE]
  for (p in unique(parts$prefix)) {
    grp <- parts[parts$prefix == p, , drop = FALSE]
    if (nrow(grp) < 2) next
    if (is_index_run(grp$suffix)) {
      clusters[[length(clusters) + 1]] <- list(label = p, cols = grp$name, x = NULL, x_source = "manual")
    } else {
      clusters[[length(clusters) + 1]] <- list(label = p, cols = grp$name, x = grp$suffix, x_source = "header")
    }
  }

  sizes <- vapply(clusters, function(cl) length(cl$cols), numeric(1))
  clusters[order(-sizes)]
}

# A column can identify participants when it has no gaps and most of its values name
# more than one row: 3 participants x 5 prices repeats every id five times.
repeats_enough <- function(v) {
  if (anyNA(v)) return(FALSE)
  counts <- table(as.character(v))
  length(counts) > 0 && mean(counts >= 2) >= 0.8
}

# Name matches first, then the supplied key ascending, then column order.
rank_candidates <- function(cands, pattern, key = NULL) {
  if (length(cands) == 0) return(cands)
  hit <- grepl(pattern, tolower(cands), perl = TRUE)
  if (is.null(key)) key <- seq_along(cands)
  cands[order(!hit, key, seq_along(cands))]
}

n_distinct_chr <- function(v) length(unique(as.character(v)))

constant_within_id <- function(v, ids) {
  all(vapply(split(as.character(v), ids), function(z) length(unique(z)) == 1, logical(1)))
}

# The price grid: at least two participants, one value per participant per row, and a grid
# they mostly share. With a single id nothing is learned from repetition - any constant
# column is an "id" and any unique column an "x", which is how a 5.5-Trial export's timing
# columns read as a long frame - so a one-participant file stays on the wide path.
x_shape_ok <- function(v, ids) {
  if (anyNA(v) || n_distinct_chr(v) < 2 || length(unique(ids)) < 2) return(FALSE)
  if (!all(vapply(split(v, ids), function(z) anyDuplicated(z) == 0, logical(1)))) return(FALSE)
  per_value <- vapply(split(ids, as.character(v)), function(z) length(unique(z)), numeric(1))
  mean(per_value >= 2) >= 0.8
}

#' Is this frame already one row per observation, and which columns carry it?
#'
#' Scored on the rows: an id column whose values repeat, an x column unique within each id
#' and shared across ids, and a second numeric column for y. Names only rank the candidates -
#' except for one veto: a frame whose numeric columns form a wide series (numeric headers, or
#' a shared `prefix_<number>` run) is a wide frame, however its rows happen to line up.
#' Without that veto `demand-minimal-grouped.csv` - four rows, two groups, five price headers -
#' reads as two participants with two prices.
#'
#' @return list(id_col, x_col, y_col, group_col) or NULL when the frame is not long
#' @export
detect_long <- function(dat, target) {
  if (!is.data.frame(dat) || nrow(dat) < 2 || ncol(dat) < 3) return(NULL)
  if (length(cluster_series_columns(dat)) > 0) return(NULL)

  nms <- colnames(dat)
  numericish <- nms[vapply(nms, function(nm) numeric_share(dat[[nm]]) >= 0.8, logical(1))]
  if (length(numericish) < 2) return(NULL)

  id_cands <- nms[vapply(nms, function(nm) repeats_enough(dat[[nm]]), logical(1))]
  id_cands <- rank_candidates(
    id_cands, id_pattern, key = vapply(id_cands, function(nm) n_distinct_chr(dat[[nm]]), numeric(1))
  )

  for (id_col in id_cands) {
    ids <- as.character(dat[[id_col]])
    rest <- setdiff(numericish, id_col)
    parsed <- lapply(rest, function(nm) parse_cells(dat[[nm]]))
    names(parsed) <- rest
    x_cands <- rest[vapply(rest, function(nm) x_shape_ok(parsed[[nm]], ids), logical(1))]
    x_cands <- rank_candidates(
      x_cands, x_name_pattern, key = vapply(x_cands, function(nm) n_distinct_chr(parsed[[nm]]), numeric(1))
    )
    if (length(x_cands) == 0) next
    x_col <- x_cands[1]
    # A response varies within the participant, unless its name says otherwise. A column
    # that repeats the participant's own value is usually a covariate - `id, x, age`, whose
    # empty y column was dropped before the mapper opened, must not offer ages as
    # consumption - but a non-discounter really does answer the same indifference point at
    # every delay, so a flat column named like a response is still a response.
    y_cands <- setdiff(rest, x_col)
    varies <- vapply(y_cands, function(nm) !constant_within_id(parsed[[nm]], ids), logical(1))
    keep <- varies | grepl(y_name_pattern, tolower(y_cands), perl = TRUE)
    y_cands <- y_cands[keep]
    if (length(y_cands) == 0) next
    y_cands <- rank_candidates(y_cands, y_name_pattern, key = as.numeric(!varies[keep]))
    y_col <- y_cands[1]

    group_col <- NULL
    if (target != "discounting") {
      n_ids <- length(unique(ids))
      cands <- setdiff(nms, c(id_col, x_col, y_col))
      cands <- cands[vapply(cands, function(nm) {
        v <- dat[[nm]]
        k <- n_distinct_chr(v)
        !nm %in% numericish && !anyNA(v) && k >= 2 && k < n_ids && k <= 10 &&
          constant_within_id(v, ids)
      }, logical(1))]
      cands <- rank_candidates(cands, group_name_pattern)
      if (length(cands) > 0) group_col <- cands[1]
    }

    return(list(id_col = id_col, x_col = x_col, y_col = y_col, group_col = group_col))
  }
  NULL
}

#' Best-guess wide spec for a frame that failed template validation
#' @export
guess_spec_wide <- function(dat, target) {
  id_col <- guess_id_col(dat)
  clusters <- cluster_series_columns(dat, exclude = if (is.na(id_col)) character(0) else id_col)
  if (target == "discounting" && length(clusters) > 1) clusters <- clusters[1]
  if (length(clusters) == 0) {
    clusters <- list(list(label = "", cols = character(0), x = NULL, x_source = "manual"))
  }
  if (length(clusters) == 1) clusters[[1]]$label <- ""
  all_header <- all(vapply(clusters, function(cl) identical(cl$x_source, "header"), logical(1)))
  new_spec(
    target = target,
    layout = "wide",
    id_col = if (is.na(id_col)) NULL else id_col,
    series = lapply(clusters, function(cl) new_series(cl$cols, x = cl$x, label = cl$label)),
    x_source = if (all_header) "header" else "manual"
  )
}

#' Prefill for a frame that is already long; falls back to the first three columns
#' @export
guess_spec_long <- function(dat, target) {
  hit <- detect_long(dat, target)
  cols <- colnames(dat)
  pick <- function(i) if (length(cols) >= i) cols[i] else NULL
  new_spec(
    target = target,
    layout = "long",
    id_col = hit$id_col %||% pick(1),
    x_col = hit$x_col %||% pick(2),
    y_col = hit$y_col %||% pick(3),
    group_col = hit$group_col
  )
}

#' Best-guess spec: long when the rows already are one per observation, else wide
#' @export
guess_spec <- function(dat, target) {
  if (is.null(detect_long(dat, target))) guess_spec_wide(dat, target) else guess_spec_long(dat, target)
}
