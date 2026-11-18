#' Read an uploaded CSV/TSV, whatever locale wrote it
#'
#' Excel in pt-BR and most European locales saves "CSV" with `;` between fields
#' and a decimal comma. A hard-coded "," read turned such a file into one column
#' named `id;x;y`, which every tab rejected and the mapper could not split
#' (2026-09-21..10-20: 0 of 23 failing discounting sessions got through).

box::use(
  utils[count.fields, head, type.convert],
  vroom,
)

box::use(
  app / logic / reshape / detect[is_group_like, is_id_like],
)

sniff_lines <- 50L

# Quote-aware field count per line. Choosing a delimiter by which one splits every
# line into the same number of fields (not by which character is commoner) keeps a
# comma file comma-delimited when its text fields contain semicolons.
consistent_fields <- function(lines, sep) {
  n <- tryCatch(
    count.fields(textConnection(lines), sep = sep, quote = "\"", comment.char = "", blank.lines.skip = TRUE),
    error = function(e) NA_integer_
  )
  length(n) > 0 && !anyNA(n) && n[1] > 1 && all(n == n[1])
}

#' Choose "," or ";" for a .csv from its first lines
#' @param lines Character vector, the file's first lines
#' @return "," or ";"
#' @export
sniff_delim <- function(lines) {
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0 || consistent_fields(lines, ",")) return(",")
  if (consistent_fields(lines, ";")) return(";")
  ","
}

# Numeric cells in a `;` file: a decimal comma, a decimal point, or a whole number.
number_cell <- "^\\s*[-+]?[0-9]+([.,][0-9]+)?\\s*$"

# A `;` file is read as text and each numeric-looking cell has its decimal comma
# turned into a point before type guessing, so `0,5` and `0.5` both parse, "10.5" is
# never taken for 105, and a stray text cell leaves the column exactly as a comma
# file with the same values would ("0.5", "bad", "1.5"), not as "0,5" for
# readr::parse_number() to read as 5.
convert_semicolon_columns <- function(dat) {
  converted <- character(0)
  for (nm in names(dat)) {
    v <- dat[[nm]]
    num <- !is.na(v) & grepl(number_cell, v, perl = TRUE)
    converted <- c(converted, trimws(v[num & grepl(",", v, fixed = TRUE)]))
    v[num] <- sub(",", ".", trimws(v[num]), fixed = TRUE)
    dat[[nm]] <- type.convert(v, as.is = TRUE)
  }
  # A decimal comma in a header (`R$0,50`) cannot be a field separator here.
  comma_header <- grepl("[0-9],[0-9]", names(dat))
  converted <- c(converted, names(dat)[comma_header])
  names(dat) <- gsub("([0-9]),([0-9])", "\\1.\\2", names(dat))
  attr(dat, "decimal_commas") <- comma_record(converted)
  dat
}

# What a read converted from decimal commas: how many values, and up to three as typed.
comma_record <- function(converted = character(0)) {
  list(n = length(converted), examples = head(unique(converted), 3L))
}

# A comma inside a number is a decimal mark in Brazil and most of Europe (7,5) and a
# thousands separator in the US and Mexico (1,234). One cell cannot say which, so a comma
# or tab file is judged as a whole.
comma_number <- "^\\s*[-+]?[0-9]+,[0-9]+\\s*$"
# 1,234 or 12,500: could be a thousands value. 7,5 and 12,50 could not, and neither could
# 0,250, since no thousands value starts with zero.
thousands_shaped <- "^\\s*[-+]?[1-9][0-9]{0,2},[0-9]{3}\\s*$"
# A number written with a decimal point: 0.5, 1.0, "1." and 1.5e-2 alike.
point_number <- "^\\s*[-+]?(?:[0-9]+\\.[0-9]*|\\.[0-9]+)(?:[eE][-+]?[0-9]+)?\\s*$"

# A price or delay header may carry a currency sign around its number: R$0,50 or 2,50€.
currency_header <- function(mark) {
  paste0(
    "(?i)^\\s*(?:(?:r|us|a|c|nz|hk|mx)?\\$|[\u20ac\u00a3\u00a5\u20b9\u20a9\u20bd])?\\s*",
    "[-+]?[0-9]+", mark, "[0-9]+\\s*[\u20ac\u00a3\u00a5\u20b9\u20a9\u20bd]?\\s*$"
  )
}
comma_header <- currency_header(",")
point_header <- currency_header("\\.")

# Whether the measurement columns write any number with a decimal point. `text` is the
# file read as text: type guessing turns "1.0" into 1 and would hide the point.
uses_decimal_point <- function(text) {
  in_column <- vapply(text, function(v) any(grepl(point_number, v, perl = TRUE)), logical(1))
  any(in_column) || any(grepl(point_header, names(text), perl = TRUE))
}

#' Read commas as decimal marks when the file shows that is what they are
#'
#' Converts only when some comma number could not be a thousands value and nothing in the
#' file uses a decimal point; then every comma number, in cells and headers, is read with
#' the comma as its decimal mark. A file whose comma numbers all look like 1,234, or one
#' that mixes commas and points, is returned as read, so validation rejects the cell by
#' name rather than the reader guessing.
#'
#' Id and group columns are labels, not measurements: they are never rewritten and are not
#' evidence either way, so an id "01,5" stays distinct from "1,50" and a group label "1,5"
#' does not turn a consumption of 1,234 into 1.234.
#' @param dat Frame read from a comma or tab file with no grouping mark
#' @param read_text Function returning the same file with every column as text; called
#'   only when the file has comma numbers, to look for decimal points as typed
#' @return `dat`, with attr "decimal_commas" = list(n, examples)
convert_decimal_commas <- function(dat, read_text) {
  measured <- !(is_id_like(trimws(names(dat))) | is_group_like(names(dat)))
  is_text <- measured & vapply(dat, is.character, logical(1))
  hits <- lapply(dat[is_text], function(v) !is.na(v) & grepl(comma_number, v, perl = TRUE))
  header_hit <- measured & grepl(comma_header, names(dat), perl = TRUE)
  header_numbers <- gsub("[^0-9,+-]", "", names(dat)[header_hit])
  found <- c(
    unlist(Map(function(v, hit) trimws(v[hit]), dat[is_text], hits), use.names = FALSE),
    header_numbers
  )
  decimal_file <- length(found) > 0 &&
    !all(grepl(thousands_shaped, found, perl = TRUE)) &&
    !uses_decimal_point(read_text()[measured])
  if (!decimal_file) {
    attr(dat, "decimal_commas") <- comma_record()
    return(dat)
  }
  for (nm in names(hits)[vapply(hits, any, logical(1))]) {
    v <- dat[[nm]]
    v[hits[[nm]]] <- sub(",", ".", trimws(v[hits[[nm]]]), fixed = TRUE)
    dat[[nm]] <- type.convert(v, as.is = TRUE)
  }
  names(dat)[header_hit] <- sub(",", ".", trimws(names(dat)[header_hit]), fixed = TRUE)
  attr(dat, "decimal_commas") <- comma_record(found)
  dat
}

#' The notice for a read that converted decimal commas
#' @param dat Frame from read_upload()
#' @return Character message, or NULL when nothing was converted
#' @export
decimal_comma_notice <- function(dat) {
  info <- attr(dat, "decimal_commas")
  if (is.null(info) || info$n == 0) return(NULL)
  example <- info$examples[1]
  sprintf(
    "Commas were read as decimal marks in %d value%s (for example \"%s\" as %s).",
    info$n, if (info$n == 1) "" else "s", example, sub(",", ".", example, fixed = TRUE)
  )
}

read_semicolon <- function(path) {
  dat <- vroom$vroom(
    path, delim = ";", quote = "\"", show_col_types = FALSE,
    col_types = vroom$cols(.default = vroom$col_character())
  )
  convert_semicolon_columns(dat)
}

# Some exports end every line of a comma file with a run of semicolons
# (`id,r$0,r$1;;;;`), which turned the last header into "r$1;;;;". Only a run of two
# or more on the header, with every line ending in `;`, counts as junk: a single
# trailing `;` can be data ("note;").
# vroom's default locale takes "," for a grouping mark, so a quoted "7,5" became 75 and
# passed validation as a number. With no grouping mark such a cell stays text, for
# convert_decimal_commas() to judge with the rest of the file.
no_grouping <- vroom$locale(grouping_mark = "")

# Column types are guessed from every row, not vroom's default sample: a text cell
# beyond the sample was forced into the sampled type and became NA with only a warning.
read_delimited <- function(file, delim) {
  read <- function(...) {
    vroom$vroom(file, delim = delim, show_col_types = FALSE, locale = no_grouping, ...)
  }
  convert_decimal_commas(
    read(guess_max = Inf),
    read_text = function() read(col_types = vroom$cols(.default = vroom$col_character()))
  )
}

read_comma <- function(path, first_line) {
  plain <- function() read_delimited(path, ",")
  if (!grepl(";{2,}\\s*$", first_line, useBytes = TRUE)) return(plain())
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  filled <- lines[nzchar(trimws(lines))]
  if (!all(grepl(";\\s*$", filled, useBytes = TRUE))) return(plain())
  lines <- sub(";+\\s*$", "", lines, useBytes = TRUE)
  read_delimited(I(paste(lines, collapse = "\n")), ",")
}

#' Read an uploaded CSV or TSV file
#'
#' @param path File path to read
#' @param ext File extension ("csv" or "tsv")
#' @return Data frame with attrs "delim" and "decimal_commas" (see decimal_comma_notice()),
#'   or NULL if reading fails
#' @export
read_upload <- function(path, ext) {
  ext <- tolower(ext)
  tryCatch({
    if (identical(ext, "tsv")) {
      dat <- read_delimited(path, "\t")
      attr(dat, "delim") <- "\t"
      return(dat)
    }
    if (!identical(ext, "csv")) return(NULL)
    # An unopenable file warns before it errors; the error is the signal.
    lines <- suppressWarnings(readLines(path, n = sniff_lines, warn = FALSE, encoding = "UTF-8"))
    lines[1] <- sub("^\ufeff", "", lines[1])
    delim <- sniff_delim(lines)
    dat <- if (delim == ";") read_semicolon(path) else read_comma(path, lines[1])
    attr(dat, "delim") <- delim
    dat
  }, error = function(e) NULL)
}

qualtrics_meta <- c(
  "startdate", "enddate", "status", "ipaddress", "progress",
  "duration (in seconds)", "finished", "recordeddate"
)

#' Shape of a rejected upload, for telemetry: counts and flags only
#'
#' Never column names or cell values.
#' @param dat Data frame as read by read_upload()
#' @return Named list
#' @export
upload_shape <- function(dat) {
  nms <- tolower(trimws(colnames(dat)))
  delim <- attr(dat, "delim")
  delim_name <- c("," = "comma", ";" = "semicolon", "\t" = "tab")
  list(
    n_cols = ncol(dat),
    n_rows = nrow(dat),
    delim = if (is.null(delim) || !delim %in% names(delim_name)) "unknown" else unname(delim_name[[delim]]),
    n_numeric_cols = sum(vapply(dat, is.numeric, logical(1))),
    first_col_id_like = length(nms) > 0 && is_id_like(nms[1]),
    any_id_like = any(is_id_like(nms)),
    has_qualtrics_meta = any(nms %in% qualtrics_meta)
  )
}
