# A pt-BR/EU Excel export (`;` delimiter, decimal commas, R$ price headers) loads
# straight through the upload module instead of opening the mapper on one column.

box::use(
  testthat[...],
  shiny,
  jsonlite,
)

box::use(
  app / view / file_input,
)

upload_input <- function(name) {
  path <- testthat::test_path("fixtures", name)
  list(name = name, size = file.size(path), type = "text/csv", datapath = path)
}

describe("file_input with semicolon CSVs", {
  it("loads indifference points with decimal commas on the discounting tab", {
    shiny$testServer(file_input$server, args = list(type = "discounting"), {
      session$setInputs(upload = upload_input("semicolon-ip-decimal-comma.csv"))
      expect_null(mapper_request())
      dat <- session$userData$data$discounting
      expect_equal(colnames(dat), c("id", "x", "y"))
      expect_equal(dat$y[1:3], c(0.95, 0.8, 0.6))
    })
  })

  it("loads an R$-priced wide demand file on the demand tab", {
    shiny$testServer(file_input$server, args = list(type = "demand"), {
      session$setInputs(upload = upload_input("semicolon-demand-brl.csv"))
      expect_null(mapper_request())
      expect_false(is.null(session$userData$data$demand))
    })
  })

  it("logs the shape of a rejected upload without its column names", {
    db <- with_sqlite_telemetry()
    shiny$testServer(file_input$server, args = list(type = "demand"), {
      as_telemetry_session(session)
      session$setInputs(upload = upload_input("wide-qualtrics-apt.csv"))
      expect_false(is.null(mapper_request()))
    })
    rows <- read_event_rows(db, "validation_outcome")
    details <- jsonlite$fromJSON(rows$details)
    expect_equal(details$delim, "comma")
    expect_true(details$n_cols > 1)
    expect_true(is.logical(details$any_id_like))
  })
})
