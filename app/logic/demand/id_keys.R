#' Ids that appear in more than one group
#'
#' A within-subject file has the same id under several groups. Everything that works by id
#' alone (Two Stage fits, which ignore grouping; ungrouped empirical measures and
#' systematic criteria) saw two consumption values per price for such an id and aborted
#' with "Duplicates found where id = ...". Here each of that id's groups becomes a curve of
#' its own, labelled by id and group, so no analysis mixes two conditions into one curve.

#' Ids with rows in more than one group
#'
#' @param data Long demand frame
#' @return Character vector of ids, in order of appearance; empty without a group column
#' @export
ids_spanning_groups <- function(data) {
  if (!all(c("id", "group") %in% colnames(data))) {
    return(character(0))
  }
  pairs <- unique(data.frame(
    id = as.character(data$id), group = as.character(data$group), stringsAsFactors = FALSE
  ))
  unique(pairs$id[pairs$id %in% pairs$id[duplicated(pairs$id)]])
}

#' Give each curve of an id that spans groups an id of its own
#'
#' Id "1" in groups "a" and "b" becomes "1 (a)" and "1 (b)"; ids with one group keep their
#' name. A new label is never one that another id already has. A factor id stays a factor,
#' with the new labels in the place the old level had.
#'
#' @param data Long demand frame
#' @return `data` with the relabelled `id` and attr "ids_keyed_by_group": the original ids
#'   that were relabelled (empty when none)
#' @export
key_ids_within_group <- function(data) {
  spanning <- ids_spanning_groups(data)
  if (length(spanning) == 0) {
    attr(data, "ids_keyed_by_group") <- character(0)
    return(data)
  }
  id <- as.character(data$id)
  group <- as.character(data$group)
  old_levels <- if (is.factor(data$id)) levels(data$id) else unique(id)
  group_levels <- if (is.factor(data$group)) levels(data$group) else unique(group)

  pairs <- unique(data.frame(id = id, group = group, stringsAsFactors = FALSE))
  pairs <- pairs[pairs$id %in% spanning, ]
  pairs <- pairs[order(match(pairs$id, old_levels), match(pairs$group, group_levels)), ]
  kept <- setdiff(old_levels, spanning)
  # make.unique() leaves the names that come first alone, so an id already called "1 (a)"
  # keeps its name and the new label takes a suffix.
  pairs$label <- utils::tail(
    make.unique(c(kept, paste0(pairs$id, " (", pairs$group, ")")), sep = " #"),
    nrow(pairs)
  )

  # The id's length goes into the key: pasting id and group alone is ambiguous when a
  # label contains the separator.
  pair_key <- function(id, group) paste(nchar(id), id, group, sep = "\r")
  row_pair <- match(pair_key(id, group), pair_key(pairs$id, pairs$group))
  new_id <- ifelse(is.na(row_pair), id, pairs$label[row_pair])
  if (is.factor(data$id)) {
    new_levels <- unlist(lapply(old_levels, function(level) {
      if (level %in% spanning) pairs$label[pairs$id == level] else level
    }))
    new_id <- factor(new_id, levels = new_levels)
  }
  data$id <- new_id
  attr(data, "ids_keyed_by_group") <- spanning
  data
}

#' The notice for data whose ids were keyed by group
#'
#' @param data Frame from `key_ids_within_group()`
#' @return Character message, or NULL when no id was relabelled
#' @export
key_ids_notice <- function(data) {
  spanning <- attr(data, "ids_keyed_by_group")
  if (length(spanning) == 0) {
    return(NULL)
  }
  shown <- utils::head(spanning, 10L)
  more <- if (length(spanning) > length(shown)) {
    paste0(" (and ", length(spanning) - length(shown), " more)")
  } else {
    ""
  }
  first <- spanning[1]
  example <- as.character(data$id[match(TRUE, startsWith(as.character(data$id), paste0(first, " (")))])
  paste0(
    length(spanning), if (length(spanning) == 1) " id appears" else " ids appear",
    " in more than one group: ", paste0("\"", shown, "\"", collapse = ", "), more,
    ". Each group of such an id is analysed as a separate curve, labelled by id and group",
    if (!is.na(example)) paste0(" (for example ", example, ")") else "",
    ". Two Stage fits, empirical measures and systematic criteria count these as separate",
    " participants."
  )
}
