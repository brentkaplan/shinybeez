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

#' Read an uploaded CSV or TSV file
#'
#' @param path File path to read
#' @param ext File extension ("csv" or "tsv")
#' @return Data frame, or NULL if reading fails
read_upload <- function(path, ext) {
  tryCatch(
    switch(
      ext,
      csv = vroom$vroom(path, delim = ",", show_col_types = FALSE),
      tsv = vroom$vroom(path, delim = "\t", show_col_types = FALSE)
    ),
    error = function(e) NULL
  )
}

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
        tmp <- read_upload(input$upload$datapath, ext)
        if (is.null(tmp)) {
          shiny$showNotification(
            "Unable to read the uploaded file. Please ensure it is a valid CSV or TSV file.",
            type = "error",
            duration = NULL
          )
          return()
        }
        # Normalize column names (lowercase, trim whitespace)
        colnames(tmp) <- tryCatch(trimws(tolower(colnames(tmp))), error = function(e) colnames(tmp))
        chk_data <- validate$check_data(tmp, type = "demand")
        if (is.character(chk_data)) {
          shiny$showNotification(
            paste(
              "Data are not in the correct format. Please refer to the documentation.",
              chk_data
            ),
            type = "error",
            duration = NULL
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
          shiny$showNotification(
            paste0("Data loaded: ", nrow(tmp), " rows, ", ncol(tmp), " columns."),
            type = "message",
            duration = 5
          )
        }
      } else if (type == "discounting") {
        session_logger$info(
          paste0("Reading discounting file: ", input$upload$name),
          category = "data_processing"
        )
        tmp <- read_upload(input$upload$datapath, ext)
        if (is.null(tmp)) {
          shiny$showNotification(
            "Unable to read the uploaded file. Please ensure it is a valid CSV or TSV file.",
            type = "error",
            duration = NULL
          )
          return()
        }
        # Normalize column names (lowercase, trim whitespace)
        colnames(tmp) <- tryCatch(trimws(tolower(colnames(tmp))), error = function(e) colnames(tmp))

        chk_data <- validate$check_data(tmp, type = "discounting")
        if (is.character(chk_data)) {
          shiny$showNotification(
            paste(
              "Data are not in the correct format. Please refer to the documentation.",
              chk_data
            ),
            type = "error",
            duration = NULL
          )
          return()
        } else {
          is_mcq_wide <- "subjectid" %in% colnames(tmp) && ncol(tmp) == 28
          is_mcq_long <- identical(colnames(tmp), c("subjectid", "questionid", "response"))
          if (is_mcq_wide || is_mcq_long) {
            # MCQ format: NAs are expected (missing/unanswered items).
            # score_mcq27() handles missing data via imputation — preserve as-is.
            n_missing <- sum(is.na(tmp))
            format_label <- if (is_mcq_wide) "MCQ wide" else "MCQ long"
            session_logger$info(
              paste0(
                "Discounting format: ", format_label,
                " (", nrow(tmp), " rows, ", ncol(tmp), " cols, ",
                n_missing, " NAs preserved for imputation)"
              ),
              category = "data_processing"
            )
            session$userData$data$discounting <- tmp
            shiny$showNotification(
              paste0("Data loaded: ", nrow(tmp), " rows, ", ncol(tmp), " columns."),
              type = "message",
              duration = 5
            )
          } else {
            # Non-MCQ formats: drop fully-NA rows and empty columns
            na_result <- validate$remove_na_rows(tmp)
            tmp <- na_result$data
            session_logger$info(
              paste0(
                "Discounting format: standard (",
                nrow(tmp), " rows, ", ncol(tmp), " cols, ",
                na_result$n_dropped, " NA rows dropped)"
              ),
              category = "data_processing"
            )
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
            shiny$showNotification(
              paste0("Data loaded: ", nrow(tmp), " rows, ", ncol(tmp), " columns."),
              type = "message",
              duration = 5
            )
          }
        }
      } else if (type == "mixed_effects_demand") {
        session_logger$info(
          paste0("Reading mixed effects demand file: ", input$upload$name),
          category = "data_processing"
        )
        tmp <- read_upload(input$upload$datapath, ext)
        if (is.null(tmp)) {
          shiny$showNotification(
            "Unable to read the uploaded file. Please ensure it is a valid CSV or TSV file.",
            type = "error",
            duration = NULL
          )
          return()
        }
        # Normalize column names (lowercase, trim whitespace)
        colnames(tmp) <- tryCatch(trimws(tolower(colnames(tmp))), error = function(e) colnames(tmp))

        chk_data <- validate$check_data(tmp, type = "mixed_effects_demand")
        if (is.character(chk_data)) {
          shiny$showNotification(
            paste(
              "Data are not in the correct format. Please refer to the documentation.",
              chk_data
            ),
            type = "error",
            duration = NULL
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
          shiny$showNotification(
            paste0("Data loaded: ", nrow(tmp), " rows, ", ncol(tmp), " columns."),
            type = "message",
            duration = 5
          )
        }
      }
    }) |>
      shiny$bindEvent(input[["upload"]])

  })
}
