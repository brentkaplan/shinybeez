#' Shared Data Table Component
#'
#' Reusable DT-based data table with export buttons and consistent styling.

box::use(
  DT,
  shiny
)

#' Create a standard data table UI
#'
#' @param id Module namespace ID
#' @param height Scroll height (default "300px")
#' @return DT output element
#' @export
ui <- function(id, height = "300px") {
  ns <- shiny$NS(id)
  DT$DTOutput(ns("table"))
}

#' Data table server with export buttons
#'
#' @param id Module namespace ID
#' @param data_reactive Reactive expression returning the data to display
#' @param filename_prefix Prefix for exported filenames
#' @param scroll_y Vertical scroll height
#' @param scroll_x Enable horizontal scrolling
#' @param fixed_columns Number of left columns to fix (default 1)
#' @param page_length Rows per page (default NULL for no paging)
#' @param round_digits Number of digits to round numeric columns (NULL to skip)
#' @export
server <- function(
  id,
  data_reactive,
  filename_prefix = "shinybeez_Data",
  scroll_y = 300,
  scroll_x = TRUE,
  fixed_columns = 1L,
  page_length = NULL,
  round_digits = NULL
) {
  shiny$moduleServer(id, function(input, output, session) {
    output$table <- DT$renderDT(server = FALSE, {
      data <- data_reactive()
      shiny$req(data)

      # Round numeric columns if requested
      if (!is.null(round_digits) && is.numeric(round_digits)) {
        numeric_cols <- sapply(data, is.numeric)
        data[, numeric_cols] <- lapply(
          data[, numeric_cols, drop = FALSE],
          function(x) round(x, round_digits)
        )
      }

      # Build options
      opts <- list(
        autoWidth = TRUE,
        ordering = TRUE,
        dom = if (is.null(page_length)) "Bti" else "Btipl",
        buttons = list(
          list(extend = "copy"),
          list(extend = "print"),
          list(
            extend = "csv",
            filename = filename_prefix,
            title = NULL
          ),
          list(
            extend = "excel",
            filename = filename_prefix,
            title = NULL
          ),
          list(
            extend = "pdf",
            filename = filename_prefix,
            title = NULL
          )
        ),
        deferRender = TRUE,
        scrollY = scroll_y,
        scroller = TRUE
      )

      if (scroll_x) {
        opts$scrollX <- TRUE
      }

      if (!is.null(page_length)) {
        opts$pageLength <- page_length
      }

      # Extensions to use
      extensions <- c("Buttons", "Scroller")
      if (fixed_columns > 0) {
        extensions <- c(extensions, "FixedColumns")
        opts$fixedColumns <- list(leftColumns = fixed_columns)
      }

      DT$datatable(
        data,
        rownames = FALSE,
        extensions = extensions,
        fillContainer = FALSE,
        options = opts
      )
    })
  })
}
