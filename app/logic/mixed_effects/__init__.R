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
  . / model_output_utils,
  . / model_summary,
  . / plotting,
  . / systematic_utils,
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
  model_output_utils,
  model_summary,
  plotting,
  systematic_utils,
  validation_utils
)
