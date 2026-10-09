#' Shared View Components
#'
#' Reusable UI components shared across demand, discounting, and mixed effects modules.

box::use(
  . / data_table,
  . / palette_picker,
  . / plot_layers,
  . / plot_settings,
  . / systematic_criteria
)

#' @export
box::export(data_table, palette_picker, plot_layers, plot_settings, systematic_criteria)
