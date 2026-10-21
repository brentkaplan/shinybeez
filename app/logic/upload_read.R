#' Read an uploaded CSV/TSV, whatever locale wrote it
#'
#' Excel in pt-BR and most European locales saves "CSV" with `;` between fields
#' and a decimal comma. A hard-coded "," read turned such a file into one column
#' named `id;x;y`, which every tab rejected and the mapper could not split
#' (2026-09-21..10-20: 0 of 23 failing discounting sessions got through).

box::use(
  utils[count.fields, type.convert],
  vroom,
)

box::use(
  app / logic / reshape / detect[is_id_like],
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
number_cell <- "^\\s*-?[0-9]+([.,][0-9]+)?\\s*$"

# A `;` file is read as text and each numeric-looking cell has its decimal comma
# turned into a point before type guessing, so `0,5` and `0.5` both parse, "10.5" is
# never taken for 105, and a stray text cell leaves the column exactly as a comma
# file with the same values would ("0.5", "bad", "1.5"), not as "0,5" for
# readr::parse_number() to read as 5.
convert_semicolon_columns <- function(dat) {
  for (nm in names(dat)) {
    v <- dat[[nm]]
    num <- !is.na(v) & grepl(number_cell, v, perl = TRUE)
    v[num] <- sub(",", ".", trimws(v[num]), fixed = TRUE)
    dat[[nm]] <- type.convert(v, as.is = TRUE)
  }
  # A decimal comma in a header (`R$0,50`) cannot be a field separator here.
  names(dat) <- gsub("([0-9]),([0-9])", "\\1.\\2", names(dat))
  dat
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
read_comma <- function(path, first_line) {
  plain <- function() vroom$vroom(path, delim = ",", show_col_types = FALSE)
  if (!grepl(";{2,}\\s*$", first_line, useBytes = TRUE)) return(plain())
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  filled <- lines[nzchar(trimws(lines))]
  if (!all(grepl(";\\s*$", filled, useBytes = TRUE))) return(plain())
  lines <- sub(";+\\s*$", "", lines, useBytes = TRUE)
  vroom$vroom(I(paste(lines, collapse = "\n")), delim = ",", show_col_types = FALSE)
}

#' Read an uploaded CSV or TSV file
#'
#' @param path File path to read
#' @param ext File extension ("csv" or "tsv")
#' @return Data frame with attr "delim", or NULL if reading fails
#' @export
read_upload <- function(path, ext) {
  ext <- tolower(ext)
  tryCatch({
    if (identical(ext, "tsv")) {
      dat <- vroom$vroom(path, delim = "\t", show_col_types = FALSE)
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
