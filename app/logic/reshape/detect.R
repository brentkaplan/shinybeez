#' Prefill heuristics for the wide-to-long mapper
#'
#' Every guess is a starting point the user can change; validate_spec() is the
#' only gate. Designed to fail towards "ask the user" rather than invent prices.

box::use(
  readr[parse_number],
)

box::use(
  app / logic / validate[parse_header_number],
  . / spec[new_series, new_spec],
)

id_pattern <- "^(response_?id|subject_?id|subject|subj|participant|pid|ppt|id)$"
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

#' Share of non-missing cells that parse as numbers
#' @export
numeric_share <- function(v) {
  v <- v[!is.na(v)]
  if (length(v) == 0) return(0)
  if (is.numeric(v)) return(1)
  mean(!is.na(suppressWarnings(parse_number(as.character(v)))))
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

is_index_run <- function(suffix) {
  length(suffix) >= 2 && !anyNA(suffix) && isTRUE(all.equal(suffix, as.numeric(seq_along(suffix))))
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

#' Best-guess spec for a frame that failed template validation
#' @export
guess_spec <- function(dat, target) {
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
    id_col = if (is.na(id_col)) NULL else id_col,
    series = lapply(clusters, function(cl) new_series(cl$cols, x = cl$x, label = cl$label)),
    x_source = if (all_header) "header" else "manual"
  )
}
