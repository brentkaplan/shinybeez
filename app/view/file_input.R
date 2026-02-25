box::use(
  bsicons,
  bslib,
  shiny,
  tools,
  vroom,
)

box::use(
  app / logic / logging_utils,
  app / logic / validate,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    shiny$fileInput(
      inputId = ns("upload"),
      label = bslib$tooltip(
        trigger = list(
          "Upload your file (csv or tsv) ",
          bsicons$bs_icon("info-circle")
        ),
        "If you are unsure of the format, please use the downloadable
              templates on the welcome page."
      ),
      accept = c(".csv", ".tsv")
    )
  )
}

#' @export
server <- function(id, type = "demand") {
  shiny$moduleServer(id, function(input, output, session) {
    # Create session-specific logger
    session_logger <- logging_utils$create_session_logger(session)

    shiny$observe({
      ext <- tools$file_ext(input$upload$name)
      if (type == "demand") {
        session_logger$info(
          paste0("Reading demand file: ", input$upload$name),
          category = "data_processing"
        )
        tmp <- switch(
          ext,
          csv = vroom$vroom(
            input$upload$datapath,
            delim = ",",
            show_col_types = FALSE
          ),
          tsv = vroom$vroom(
            input$upload$datapath,
            delim = "\t",
            show_col_types = FALSE
          )
        )
        chk_data <- validate$check_data(tmp, type = "demand")
        if (is.character(chk_data)) {
          shiny$showNotification(
            paste(
              "Data are not in the correct format. Please refer to the documentation.",
              chk_data
            ),
            type = "error",
            duration = 10
          )
          return()
        } else {
          # Remove rows with NAs and notify if any dropped
          na_result <- validate$remove_na_rows(tmp)
          tmp <- na_result$data
          if (na_result$n_dropped > 0) {
            shiny$showNotification(
              paste(
                na_result$n_dropped,
                "row(s) with missing values were dropped from your data."
              ),
              type = "warning",
              duration = 8
            )
          }
          session$userData$data$demand <- validate$obliterate_empty_cols(tmp)
        }
      } else if (type == "discounting") {
        session_logger$info(
          paste0("Reading discounting file: ", input$upload$name),
          category = "data_processing"
        )
        tmp <- switch(
          ext,
          csv = vroom$vroom(
            input$upload$datapath,
            delim = ",",
            show_col_types = FALSE
          ),
          tsv = vroom$vroom(
            input$upload$datapath,
            delim = "\t",
            show_col_types = FALSE
          )
        )

        chk_data <- validate$check_data(tmp, type = "discounting")
        if (is.character(chk_data)) {
          shiny$showNotification(
            paste(
              "Data are not in the correct format. Please refer to the documentation.",
              chk_data
            ),
            type = "error",
            duration = 10
          )
          return()
        } else {
          # Remove rows with NAs and notify if any dropped
          na_result <- validate$remove_na_rows(tmp)
          tmp <- na_result$data
          if (na_result$n_dropped > 0) {
            shiny$showNotification(
              paste(
                na_result$n_dropped,
                "row(s) with missing values were dropped from your data."
              ),
              type = "warning",
              duration = 8
            )
          }
          session$userData$data$discounting <- validate$obliterate_empty_cols(
            tmp
          )
        }
      } else if (type == "mixed_effects_demand") {
        session_logger$info(
          paste0("Reading mixed effects demand file: ", input$upload$name),
          category = "data_processing"
        )
        tmp <- switch(
          ext,
          csv = vroom$vroom(
            input$upload$datapath,
            delim = ",",
            show_col_types = FALSE
          ),
          tsv = vroom$vroom(
            input$upload$datapath,
            delim = "\t",
            show_col_types = FALSE
          )
        )

        chk_data <- validate$check_data(tmp, type = "mixed_effects_demand")
        if (is.character(chk_data)) {
          shiny$showNotification(
            paste(
              "Data are not in the correct format. Please refer to the documentation.",
              chk_data
            ),
            type = "error",
            duration = 10
          )
          return()
        } else {
          # Remove rows with NAs and notify if any dropped
          na_result <- validate$remove_na_rows(tmp)
          tmp <- na_result$data
          if (na_result$n_dropped > 0) {
            shiny$showNotification(
              paste(
                na_result$n_dropped,
                "row(s) with missing values were dropped from your data."
              ),
              type = "warning",
              duration = 8
            )
          }
          session$userData$data$mixed_effects_demand <- validate$obliterate_empty_cols(
            tmp
          )
        }
      }
    }) |>
      shiny$bindEvent(input[["upload"]])

  })
}
