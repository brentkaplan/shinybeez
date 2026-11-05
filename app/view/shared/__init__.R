#' Shared View Components
#'
#' Reusable UI components shared across demand, discounting, and mixed effects modules.

box::use(
  . / aesthetic_gate,
  . / data_table,
  . / palette_picker,
  . / plot_layers,
  . / systematic_criteria
)

#' @export
box::export(aesthetic_gate, data_table, palette_picker, plot_layers, systematic_criteria)
