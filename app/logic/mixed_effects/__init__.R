#' Mixed Effects Demand Logic Module
#'
#' Exports utility functions for mixed effects demand analysis.

box::use(
  . / collapse_levels,
  . / data_prep,
  . / model_fitting,
  . / emms_utils,
  . / export_utils,
  . / validation_utils
)

#' @export
box::export(
  collapse_levels,
  data_prep,
  model_fitting,
  emms_utils,
  export_utils,
  validation_utils
)
