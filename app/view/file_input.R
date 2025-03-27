box::use(
  bsicons,
  bslib,
  rhino,
  shiny,
  tools,
  vroom,
)

box::use(
  app/logic/validate,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    shiny$fileInput(
      # style = "padding-bottom: -10px;",
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
    ),
    # v2 feature
    # actionButton(
    #   style = "margin-bottom: 15px; margin-top: -15px;",
    #   inputId = ns("load_ex_data"),
    #   label = "Or Load Example Data"
    # ),
  )
}

#' @export
server <- function(id, type = "demand") {
  shiny$moduleServer(id, function(input, output, session) {

    shiny$observe({
      ext <- tools$file_ext(input$upload$name)
      if (type == "demand") {
        rhino$log$info(paste0("Reading demand file: ", input$upload$name))
        tmp <- switch(ext,
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
                      # validate("Invalid file; Please upload a .csv or .tsv file")
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
          session$userData$data$demand <- validate$obliterate_empty_cols(tmp)
        }
      } else if (type == "discounting") {
        rhino$log$info(paste0("Reading discounting file: ", input$upload$name))
        tmp <- switch(ext,
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
           # validate("Invalid file; Please upload a .csv or .tsv file")
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
          session$userData$data$discounting <- validate$obliterate_empty_cols(tmp)
        }
      }
    }) |>
      shiny$bindEvent(input[["upload"]])

    # v2 feature
    # observe({
    #   if (type == "demand") {
    #     print(paste0("Loading example data: ", type))
    #     session$userData$data$demand <-  read.csv("app/static/data/example_data.csv",
    #                                               check.names = FALSE)
    #     # print(session$userData$data$demand)
    #   } else {
    #
    #     if (type == "27-Item MCQ") {
    #       # session$userData$data$discounting <- readRDS("app/static/data/mcq27.rds")
    #       session$userData$data$discounting <- readRDS("app/static/data/mcq27.rds")
    #
    #     } else if (type == "5.5 Trial Delay Discounting") {
    #       # session$userData$data$discounting <- readRDS("app/static/data/five.fivetrial_dd.rds")
    #       session$userData$data$discounting <-  readRDS("app/static/data/five.fivetrial_dd.rds")
    #
    #     } # else if (type == "5.5 Trial Probability Discounting") {
    #     # session$userData$data$discounting <- readRDS("app/static/data/five.fivetrial_pd.rds")
    #     # return(readRDS("app/static/data/five.fivetrial_pd.rds"))
    #
    #   }
    # }) |>
    #   bindEvent(input[["load_ex_data"]])
  })
}

# .on_unload <- function(ns) {
#   log$info("Closing")
# }
