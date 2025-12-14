#' Shared View Components
#'
#' Reusable UI components shared across demand, discounting, and mixed effects modules.

box::use(
  . / data_table,
  . / plot_settings
)

#' @export
box::export(data_table, plot_settings)
