#' Mixed Effects Demand Logic Module
#'
#' Exports utility functions for mixed effects demand analysis.

box::use(
  . / collapse_levels,
  . / comparisons,
  . / data_prep,
  . / model_fitting,
  . / emms_utils,
  . / export_utils,
  . / plotting,
  . / validation_utils
)

#' @export
box::export(
  collapse_levels,
  comparisons,
  data_prep,
  model_fitting,
  emms_utils,
  export_utils,
  plotting,
  validation_utils
)
