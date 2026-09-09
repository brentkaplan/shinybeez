#' Prefill heuristics for the wide-to-long mapper
#'
#' Every guess is a starting point the user can change; validate_spec() is the
#' only gate. Designed to fail towards "ask the user" rather than invent prices.

box::use(
  app / logic / validate[parse_header_number],
  . / spec[composite_key, new_series, new_spec, parse_cells],
)

`%||%` <- function(a, b) if (is.null(a)) b else a

id_pattern <- "^(response_?id|subject_?id|subject|subj|participant|pid|ppt|id)$"
x_name_pattern <- "^(x|price|prices|cost|delay|delays|amount)$"
y_name_pattern <- "^(y|y_ll4|consumption|consumed|response|value|indiff|indifference|ip)$"
# Only an unmistakable response name rescues a column that never varies within a
# participant: "value" or "response" is as often a per-participant score.
flat_y_pattern <- "^(y|y_ll4|consumption|consumed|indiff|indifference|ip)$"
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

# A candidate condition column may widen the key only if it crosses the participants
# perfectly: no gaps, and every (participant, level) cell the same size. A ragged or
# unbalanced column is some other kind of descriptor, not one design repeated.
#
# It must also have fewer levels than there are participants, the same rule the
# between-subject path uses. Without it the two roles are symmetric and can swap: three
# subjects crossed with two conditions also reads as two "participants" (the conditions)
# each carrying a three-level "group" (the subjects), and the frame would be transposed.
partitions_cleanly <- function(v, ids) {
  if (anyNA(v) || any(!nzchar(trimws(as.character(v))))) return(FALSE)
  k <- n_distinct_chr(v)
  if (k < 2 || k > 10 || k >= length(unique(ids))) return(FALSE)
  tb <- table(ids, as.character(v))
  # Two rows per cell at the least: a one-row cell is a one-point curve, which
  # validate_long_spec() refuses anyway.
  all(tb >= 2) && length(unique(as.vector(tb))) == 1
}

#' Columns that split each participant's rows into complete, equal sets
#'
#' A within-subject condition the plain participant key never had to consult: when its levels
#' ask DIFFERENT x values, x is already unique within the participant, the long reading
#' succeeds without it, and dropping it merges two curves into one.
#'
#' Structural and name-agnostic on purpose. A derived band of x - `low`/`high` over one price
#' grid - has exactly this shape, and nothing in the rows tells the two apart. So this is the
#' list the modal offers the user, not the one it picks for them.
#' @param exclude Columns already spoken for (the x and y columns)
#' @export
partitioning_candidates <- function(dat, id_col, exclude = character(0)) {
  if (!is.data.frame(dat) || length(id_col) != 1 || !id_col %in% colnames(dat)) {
    return(character(0))
  }
  ids <- as.character(dat[[id_col]])
  # Numbers are not excluded the way guess_group_col() excludes them: a condition coded 0/1 is
  # as ordinary as one spelled out, and partitions_cleanly() is strict enough to carry the
  # weight on its own - a covariate is constant within the participant, so its (id, level)
  # table has empty cells, and a second response has far more than ten levels.
  cands <- setdiff(colnames(dat), c(id_col, exclude))
  cands <- cands[vapply(cands, function(nm) partitions_cleanly(dat[[nm]], ids), logical(1))]
  rank_candidates(cands, group_name_pattern)
}

# Two commodities interleave on the price axis - beer at 1..16 and cigarettes at 0.25..6 are
# asked over the same range. A band of one grid never does: every `low` price sits below every
# `high` one. Only an interleave is evidence enough to choose a column unasked.
levels_interleave <- function(x, lev) {
  lo <- vapply(split(x, lev), min, numeric(1))
  hi <- vapply(split(x, lev), max, numeric(1))
  if (length(lo) < 2) return(FALSE)
  o <- order(lo)
  any(hi[o][-length(o)] >= lo[o][-1])
}

# The one within-subject condition safe to choose without asking. Three things have to agree,
# because none of them is sufficient alone: the header names it a condition, its levels
# interleave on the x axis, and its VALUES are words rather than codes. The last one matters -
# `session = 1, 2, 1, 2` over four prices partitions exactly like a real two-session design,
# and nothing in the rows says which it is. A column of bare numbers is offered in the modal
# with the note instead; everything else that partitions is offered there too.
named_partition_col <- function(dat, id_col, used, x, target) {
  if (target == "discounting") return(NULL)
  cands <- partitioning_candidates(dat, id_col, exclude = setdiff(used, id_col))
  cands <- cands[grepl(group_name_pattern, tolower(cands), perl = TRUE)]
  cands <- cands[vapply(cands, function(nm) numeric_share(dat[[nm]]) < 0.8, logical(1))]
  for (nm in cands) {
    if (levels_interleave(x, as.character(dat[[nm]]))) return(nm)
  }
  NULL
}

# The within-subject signature: inside each participant, every level asks the identical set
# of x values. Exact, unlike the 80% overlap allowed across participants - a repeated design
# repeats exactly, and the looser test is what would let an irregular frame through.
grid_repeats_across_levels <- function(x, ids, lev) {
  all(vapply(split(seq_along(x), ids), function(i) {
    sets <- split(as.character(x[i]), lev[i])
    length(sets) >= 2 && length(unique(lapply(sets, function(z) sort(unique(z))))) == 1
  }, logical(1)))
}

# The x/y search, keyed on whatever identifies one series of observations: the participant,
# or - for a within-subject design - the participant crossed with a condition.
long_cols_for_key <- function(parsed, rest, keys, allow_flat = TRUE) {
  x_cands <- rest[vapply(rest, function(nm) x_shape_ok(parsed[[nm]], keys), logical(1))]
  x_cands <- rank_candidates(
    x_cands, x_name_pattern,
    key = vapply(x_cands, function(nm) n_distinct_chr(parsed[[nm]]), numeric(1))
  )
  if (length(x_cands) == 0) return(NULL)
  x_col <- x_cands[1]
  # A response varies within the series, unless its name says otherwise. A column that
  # repeats the participant's own value is usually a covariate - `id, x, age`, whose empty y
  # column was dropped before the mapper opened, must not offer ages as consumption - but a
  # non-discounter really does answer the same indifference point at every delay, so a flat
  # column named like a response is still a response.
  #
  # That exception is withdrawn on the composite attempt (`allow_flat = FALSE`): flat inside
  # one condition means the column scores the condition rather than answering the price, and
  # the non-discounter it was written for belongs to a discounting file, which never reaches
  # the composite attempt at all.
  y_cands <- setdiff(rest, x_col)
  varies <- vapply(y_cands, function(nm) !constant_within_id(parsed[[nm]], keys), logical(1))
  keep <- varies | (allow_flat & grepl(flat_y_pattern, tolower(y_cands), perl = TRUE))
  y_cands <- y_cands[keep]
  varies <- varies[keep]
  if (length(y_cands) == 0) return(NULL)
  # Variation first, then the name: a column that moves with price beats a flat one however
  # it is spelled.
  named <- grepl(y_name_pattern, tolower(y_cands), perl = TRUE)
  list(x_col = x_col, y_col = y_cands[order(!varies, !named, seq_along(y_cands))][1])
}

# Between-subject grouping: a column the participant carries, one value throughout.
guess_group_col <- function(dat, target, ids, numericish, used) {
  if (target == "discounting") return(NULL)
  n_ids <- length(unique(ids))
  cands <- setdiff(colnames(dat), used)
  cands <- cands[vapply(cands, function(nm) {
    v <- dat[[nm]]
    k <- n_distinct_chr(v)
    !nm %in% numericish && !anyNA(v) && k >= 2 && k < n_ids && k <= 10 &&
      constant_within_id(v, ids)
  }, logical(1))]
  cands <- rank_candidates(cands, group_name_pattern)
  if (length(cands) > 0) cands[1] else NULL
}

# Within-subject grouping: a column that varies inside the participant and crosses them
# cleanly. Ranked by name, then column order, so two columns that both partition give a
# stable answer.
composite_group_candidates <- function(dat, ids, numericish, id_col) {
  cands <- setdiff(colnames(dat), c(id_col, numericish))
  cands <- cands[vapply(cands, function(nm) partitions_cleanly(dat[[nm]], ids), logical(1))]
  rank_candidates(cands, group_name_pattern)
}

# A response column standing where the price should be, and a price column standing where the
# response should be. Not a mapping - a swap: the real x was rejected only because it repeats
# once per condition, which is exactly what the composite key exists to see.
reads_reversed <- function(hit) {
  grepl(y_name_pattern, tolower(hit$x_col), perl = TRUE) &&
    grepl(x_name_pattern, tolower(hit$y_col), perl = TRUE)
}

# The within-subject reading: the same grid asked once per condition, so x is unique only
# inside (participant, condition). The caller decides what to do with it - a discounting file
# cannot USE the condition, but knowing one exists is still what proves a reading reversed.
composite_hit <- function(dat, id_col, ids, parsed, rest, numericish) {
  for (g in composite_group_candidates(dat, ids, numericish, id_col)) {
    lev <- as.character(dat[[g]])
    hit <- long_cols_for_key(parsed, rest, composite_key(ids, lev), allow_flat = FALSE)
    if (is.null(hit)) next
    if (!grid_repeats_across_levels(parsed[[hit$x_col]], ids, lev)) next
    return(list(id_col = id_col, x_col = hit$x_col, y_col = hit$y_col, group_col = g))
  }
  NULL
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
#' Two attempts, in order. The participant alone answers a between-subject file, and its
#' grouping column is then guessed. Only when that finds nothing is the key widened to
#' (participant, condition), which is what a within-subject design needs - the same grid
#' asked once per condition leaves x repeating within the participant. The condition is not
#' guessed there: it is whichever column let the key work, and it has to earn that by
#' crossing the participants cleanly and repeating the grid exactly.
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

  # Columns a proven-reversed reading put on the x axis: they are responses, not participants.
  barred <- character(0)
  for (id_col in id_cands) {
    if (id_col %in% barred) next
    ids <- as.character(dat[[id_col]])
    rest <- setdiff(numericish, id_col)
    parsed <- lapply(rest, function(nm) parse_cells(dat[[nm]]))
    names(parsed) <- rest

    hit <- long_cols_for_key(parsed, rest, ids)
    if (!is.null(hit)) {
      # A reading is normally taken as it stands. The exception is one that reads reversed:
      # only then is the composite key consulted, and only to see whether it hands back the
      # SAME two columns the other way round. The name never picks a mapping here - it flags
      # a suspect one, and structure decides.
      # Reversed vocabulary is a suspicion, never a verdict. What settles it is the composite
      # key handing back the SAME two columns the other way round - that is the frame itself
      # saying the price was only rejected because it repeats once per condition.
      if (reads_reversed(hit)) {
        fixed <- composite_hit(dat, id_col, ids, parsed, rest, numericish)
        if (!is.null(fixed) &&
              identical(fixed$x_col, hit$y_col) && identical(fixed$y_col, hit$x_col)) {
          if (target != "discounting") return(fixed)
          # Proven reversed, and the correction needs a condition this target has nowhere to
          # put - the un-swapped columns would merge the conditions into one curve. Skip this
          # participant candidate, and bar the response column it chose from becoming the next
          # one. If nothing else fits, the file goes to the user rather than to a curve fitted
          # on transposed data.
          barred <- c(barred, hit$x_col)
          next
        }
        # Unproven: the names alone are not enough to throw away a structurally valid reading.
      }
      used <- c(id_col, hit$x_col, hit$y_col)
      group_col <- guess_group_col(dat, target, ids, numericish, used)
      # A between-subject group is carried by the participant; when there is none, the
      # condition may still be within-subject and simply invisible to the plain key.
      if (is.null(group_col)) {
        group_col <- named_partition_col(dat, id_col, used, parsed[[hit$x_col]], target)
      }
      return(list(id_col = id_col, x_col = hit$x_col, y_col = hit$y_col, group_col = group_col))
    }

    # The ordinary within-subject path, and never for discounting: that target has nowhere to
    # put the condition, and dropping it would merge two curves into one.
    if (target == "discounting") next
    fixed <- composite_hit(dat, id_col, ids, parsed, rest, numericish)
    if (!is.null(fixed)) return(fixed)
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
