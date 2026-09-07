# Lifecycle of the wide-to-long mapper hook in file_input.R: a rejected upload
# opens a request, a confirmed mapping stores data exactly once through the same
# tail a long upload uses, and stale or superseded mappings are ignored.

box::use(
  testthat[...],
  shiny,
  jsonlite,
)

box::use(
  app / view / file_input,
  app / logic / reshape / spec,
)

upload_input <- function(name) {
  path <- testthat::test_path("fixtures", name)
  list(name = name, size = file.size(path), type = "text/csv", datapath = path)
}

template_input <- function(name) {
  path <- file.path(find_project_root(), "app/static/data/templates", name)
  list(name = name, size = file.size(path), type = "text/csv", datapath = path)
}

apt_result <- function(token, dat) {
  s <- spec$new_spec(
    "demand", "responseid",
    list(spec$new_series(paste0("apt_", 1:5), x = c(0, 0.5, 1, 5, 10))),
    x_source = "manual"
  )
  out <- spec$apply_spec(s, dat)
  list(data = out$data, spec = s, losses = out$losses, token = token,
       meta = list(name = "wide-qualtrics-apt.csv", ext = "csv", size = 1))
}

describe("file_input rejection opens a mapper request", {
  it("sets a request with the reason and token for a wide Qualtrics demand file", {
    shiny$testServer(file_input$server, args = list(type = "demand"), {
      session$setInputs(upload = upload_input("wide-qualtrics-apt.csv"))
      req <- mapper_request()
      expect_false(is.null(req))
      expect_equal(req$target, "demand")
      expect_equal(req$token, upload_token)
      expect_match(req$reason, "not `id`|price headers")
      expect_null(session$userData$data$demand)
    })
  })

  it("opens on the mixed-effects and discounting tabs too", {
    shiny$testServer(file_input$server, args = list(type = "mixed_effects_demand"), {
      session$setInputs(upload = upload_input("wide-me-with-covariates.csv"))
      expect_equal(mapper_request()$target, "mixed_effects_demand")
    })
    shiny$testServer(file_input$server, args = list(type = "discounting"), {
      session$setInputs(upload = upload_input("wide-ip-delays-named.csv"))
      expect_equal(mapper_request()$target, "discounting")
    })
  })

  it("records reshape events under the same module name as validation events", {
    db <- with_sqlite_telemetry()
    shiny$testServer(file_input$server, args = list(type = "mixed_effects_demand"), {
      as_telemetry_session(session)
      session$setInputs(upload = upload_input("wide-me-with-covariates.csv"))
      expect_equal(mapper_request()$target, "mixed_effects_demand")
    })
    validation <- jsonlite$fromJSON(read_event_rows(db, "validation_outcome")$details)
    reshape <- jsonlite$fromJSON(read_event_rows(db, "reshape")$details)
    expect_equal(validation$module, "mixed_effects")
    expect_equal(reshape$target, "mixed_effects")
  })

  it("never opens for MCQ-27 on any tab, nor for 5.5-Trial on the discounting tab", {
    mcq <- upload_input("discounting-mcq-minimal.csv")
    five <- upload_input("discounting-five-trial-dd-minimal.csv")
    for (type in c("demand", "mixed_effects_demand", "discounting")) {
      shiny$testServer(file_input$server, args = list(type = type), {
        session$setInputs(upload = mcq)
        expect_null(mapper_request(), info = paste(type, mcq$name))
      })
    }
    shiny$testServer(file_input$server, args = list(type = "discounting"), {
      session$setInputs(upload = five)
      expect_null(mapper_request())
      expect_false(is.null(session$userData$data$discounting))
    })
    # malformed MCQ: subjectid present but the wrong width still has subjectid
    shiny$testServer(file_input$server, args = list(type = "discounting"), {
      bad <- withr::local_tempfile(fileext = ".csv")
      write.csv(data.frame(subjectid = 1:2, q1 = c(1, 0), q2 = c(0, 1)), bad, row.names = FALSE)
      session$setInputs(upload = list(name = "bad-mcq.csv", size = 10, type = "text/csv", datapath = bad))
      expect_null(mapper_request())
      expect_null(session$userData$data$discounting)
    })
  })

  it("rejects a header-only file before the mapper, with an empty-data reason", {
    db <- with_sqlite_telemetry()
    shiny$testServer(file_input$server, args = list(type = "demand"), {
      as_telemetry_session(session)
      session$setInputs(upload = upload_input("wide-header-only.csv"))
      expect_null(mapper_request())
      expect_null(session$userData$data$demand)
    })
    rows <- read_event_rows(db, "validation_outcome")
    expect_equal(nrow(rows), 1)
    details <- jsonlite$fromJSON(rows$details)
    expect_equal(details$check_name, "empty_file")
    expect_match(details$reason, "no data rows")
  })

  it("never opens for any bundled template", {
    templates <- list.files(file.path(find_project_root(), "app/static/data/templates"), pattern = "\\.csv$")
    type_for <- function(f) if (grepl("demand", f)) "demand" else "discounting"
    for (f in templates) {
      shiny$testServer(file_input$server, args = list(type = type_for(f)), {
        session$setInputs(upload = template_input(f))
        expect_null(mapper_request(), info = f)
        expect_false(is.null(session$userData$data[[type_for(f)]]), info = f)
      })
    }
  })
})

describe("file_input mapper confirm and cancel", {
  it("stores the long frame and fires upload telemetry exactly once", {
    db <- with_sqlite_telemetry()
    shiny$testServer(file_input$server, args = list(type = "demand"), {
      as_telemetry_session(session)
      session$setInputs(upload = upload_input("wide-qualtrics-apt.csv"))
      req <- mapper_request()
      mapper$result(apt_result(req$token, req$dat))
      session$flushReact()
      stored <- session$userData$data$demand
      expect_equal(colnames(stored), c("id", "x", "y"))
      expect_equal(nrow(stored), 15)
    })
    uploads <- read_event_rows(db, "data_upload")
    expect_equal(nrow(uploads), 1)
    expect_true(isTRUE(jsonlite$fromJSON(uploads$details)$reshaped))
    reshape <- read_event_rows(db, "reshape")
    expect_equal(jsonlite$fromJSON(reshape$details[1])$outcome, "opened")
    expect_equal(jsonlite$fromJSON(reshape$details[2])$outcome, "confirmed")
  })

  it("ignores a stale confirm from an earlier upload", {
    shiny$testServer(file_input$server, args = list(type = "demand"), {
      session$setInputs(upload = upload_input("wide-qualtrics-apt.csv"))
      old <- mapper_request()
      session$setInputs(upload = upload_input("wide-two-commodities.csv"))
      expect_null(session$userData$data$demand)
      mapper$result(apt_result(old$token, old$dat))
      session$flushReact()
      expect_null(session$userData$data$demand)
    })
  })

  it("re-uploading the same file re-opens with a new token", {
    shiny$testServer(file_input$server, args = list(type = "demand"), {
      session$setInputs(upload = upload_input("wide-qualtrics-apt.csv"))
      first <- mapper_request()$token
      # A second choice of the same file posts a new input value (fresh datapath),
      # so bindEvent fires again.
      again <- upload_input("wide-qualtrics-apt.csv")
      again$datapath <- withr::local_tempfile(fileext = ".csv")
      file.copy(testthat::test_path("fixtures", "wide-qualtrics-apt.csv"), again$datapath)
      session$setInputs(upload = again)
      expect_equal(mapper_request()$token, first + 1L)
    })
  })

  it("a valid upload while a mapping is pending clears the request", {
    shiny$testServer(file_input$server, args = list(type = "demand"), {
      session$setInputs(upload = upload_input("wide-qualtrics-apt.csv"))
      expect_false(is.null(mapper_request()))
      session$setInputs(upload = upload_input("demand-minimal.csv"))
      expect_null(mapper_request())
      expect_false(is.null(session$userData$data$demand))
    })
  })

  it("cancel leaves nothing stored and records the outcome", {
    db <- with_sqlite_telemetry()
    shiny$testServer(file_input$server, args = list(type = "demand"), {
      as_telemetry_session(session)
      session$setInputs(upload = upload_input("wide-qualtrics-apt.csv"))
      req <- mapper_request()
      mapper$cancelled(list(token = req$token, reason = req$reason))
      session$flushReact()
      expect_null(session$userData$data$demand)
    })
    outcomes <- vapply(read_event_rows(db, "reshape")$details, function(d) jsonlite$fromJSON(d)$outcome, character(1))
    expect_equal(unname(outcomes), c("opened", "cancelled"))
  })

  it("a mapper error is recorded and nothing is stored", {
    db <- with_sqlite_telemetry()
    shiny$testServer(file_input$server, args = list(type = "demand"), {
      as_telemetry_session(session)
      session$setInputs(upload = upload_input("wide-qualtrics-apt.csv"))
      mapper$result(list(error = "boom", token = mapper_request()$token))
      session$flushReact()
      expect_null(session$userData$data$demand)
    })
    expect_equal(nrow(read_event_rows(db, "error")), 1)
  })

  it("a confirmed frame that fails the sufficiency check is rejected like a long upload", {
    shiny$testServer(file_input$server, args = list(type = "demand"), {
      session$setInputs(upload = upload_input("wide-qualtrics-apt.csv"))
      req <- mapper_request()
      res <- apt_result(req$token, req$dat)
      res$data <- res$data[res$data$x == 0, ]   # one price per id
      mapper$result(res)
      session$flushReact()
      expect_null(session$userData$data$demand)
    })
  })
})
