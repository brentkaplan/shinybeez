#' Shared View Components
#'
#' Reusable UI components shared across demand, discounting, and mixed effects modules.

box::use(
  . / data_table,
  . / palette_picker,
  . / plot_layers,
  . / systematic_criteria
)

#' @export
box::export(data_table, palette_picker, plot_layers, systematic_criteria)
