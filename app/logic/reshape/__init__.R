#' Wide-to-long reshape logic
#'
#' Pure functions behind the column-mapper modal: a serialisable spec, its
#' validation and application, and the prefill heuristics. No Shiny here.

box::use(
  . / detect,
  . / spec
)

#' @export
box::export(
  detect,
  spec
)
