# A wide indifference-point upload with a cell that is not a number is rejected with a
# notification and must NOT open the wide-to-long mapper: the mapper's parse_cells()
# reads text through readr::parse_number(), which turns "7,5" into 75 and a text cell
# into NA, so mapping cannot repair the file. The user has to fix the cell.

box::use(
  testthat[...],
  shiny,
)

box::use(
  app / view / file_input,
)

upload_from_lines <- function(lines, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  writeLines(lines, path)
  list(name = basename(path), size = file.size(path), type = "text/csv", datapath = path)
}

# box gives the view module its own locked copy of the shiny exports; swap
# showNotification there and collect what the module shows.
capture_notifications <- function(env = parent.frame()) {
  shown <- new.env(parent = emptyenv())
  shown$messages <- character()
  shiny_env <- environment(file_input$server)$shiny
  fn <- "showNotification"
  original <- shiny_env[[fn]]
  unlockBinding(fn, shiny_env)
  assign(fn, function(ui, ...) {
    shown$messages <- c(shown$messages, as.character(ui))
    invisible(NULL)
  }, envir = shiny_env)
  withr::defer({
    assign(fn, original, envir = shiny_env)
    lockBinding(fn, shiny_env)
  }, envir = env)
  shown
}

describe("file_input - a wide indifference-point file with a text cell", {
  it("notifies and leaves the mapper closed", {
    shown <- capture_notifications()
    upload <- upload_from_lines(c("id,1,5,10,100", "p1,0.95,0.80,0.60,0.20", "p2,0.90,n/a,0.55,0.15"))
    shiny$testServer(file_input$server, args = list(type = "discounting"), {
      session$setInputs(upload = upload)
      expect_null(mapper_request())
    })
    expect_true(any(grepl("not numbers", shown$messages, fixed = TRUE)))
  })

  it("still opens the mapper for a wide file whose problem is its headers", {
    upload <- upload_from_lines(c("id,d1,d5", "p1,0.9,0.8", "p2,0.8,0.7"))
    shiny$testServer(file_input$server, args = list(type = "discounting"), {
      session$setInputs(upload = upload)
      expect_equal(mapper_request()$target, "discounting")
    })
  })
})
