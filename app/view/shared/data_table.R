#' Shared Data Table Component
#'
#' Reusable DT-based data table with export buttons and consistent styling.
#' Provides both a standalone builder function and a full Shiny module.

box::use(
  DT,
  shiny
)

#' Build a standard datatable with export buttons
#'
#' Pure function that creates a configured DT::datatable object. Use this
#' inside any renderDT block to get consistent table styling across the app.
#'
#' @param data Data frame to display
#' @param filename_prefix Prefix for exported filenames
#' @param scroll_y Vertical scroll height in pixels
#' @param scroll_x Enable horizontal scrolling
#' @param fixed_columns Number of left columns to fix (0 for none)
#' @param page_length Rows per page (NULL for no paging)
#' @param round_digits Number of digits to round numeric columns (NULL to skip)
#' @param auto_width Enable auto column width
#' @param fill_container Fill parent container
#' @return A DT::datatable object
#' @export
build_datatable <- function(
  data,
  filename_prefix = "shinybeez_Data",
  scroll_y = 300,
  scroll_x = TRUE,
  fixed_columns = 0L,
  page_length = NULL,
  round_digits = NULL,
  auto_width = TRUE,
  fill_container = FALSE
) {
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
    autoWidth = auto_width,
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
    fillContainer = fill_container,
    options = opts
  )
}

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

      build_datatable(
        data,
        filename_prefix = filename_prefix,
        scroll_y = scroll_y,
        scroll_x = scroll_x,
        fixed_columns = fixed_columns,
        page_length = page_length,
        round_digits = round_digits
      )
    })
  })
}
