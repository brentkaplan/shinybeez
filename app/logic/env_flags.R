#' On/off environment variables
#'
#' `config.yml` reads each of these with an inline expression (it cannot import a module),
#' using the same two word lists as `parse_flag()`. This module is the tested statement of
#' the rule and the source of the startup warnings.

on_words <- c("1", "true", "yes", "on", "t")
off_words <- c("0", "false", "no", "off", "f")

# Every on/off variable config.yml reads, with its value when unset.
known_flags <- c(
  GA_ENABLED = FALSE,
  LOG_PERFORMANCE = TRUE,
  LOG_TO_CONSOLE = TRUE,
  TELEMETRY_ANONYMIZE = TRUE,
  TELEMETRY_ENABLED = TRUE,
  TELEMETRY_TRACK_INPUTS = FALSE
)

normalise <- function(value) tolower(trimws(as.character(value)))

#' Read an on/off value
#'
#' @param value The variable's value as text; NA when unset
#' @param default What an unset, empty or unrecognised value means
#' @return TRUE for 1/true/yes/on/t, FALSE for 0/false/no/off/f (any case), else `default`
#' @export
parse_flag <- function(value, default) {
  word <- normalise(value)
  if (word %in% on_words) {
    return(TRUE)
  }
  if (word %in% off_words) {
    return(FALSE)
  }
  default
}

#' Warnings for on/off variables set to something that is not on or off
#'
#' The value is never printed: a variable set by mistake may hold a pasted secret.
#'
#' @return Character vector, one message per affected variable, in name order
#' @export
flag_warnings <- function() {
  names <- sort(names(known_flags))
  values <- Sys.getenv(names, unset = NA_character_)
  bad <- !is.na(values) & !normalise(values) %in% c(on_words, off_words)
  sprintf(
    "%s is set to a value that is not on or off (use 1/true/yes/on or 0/false/no/off); using its default, %s.",
    names[bad], ifelse(known_flags[names[bad]], "on", "off")
  )
}
