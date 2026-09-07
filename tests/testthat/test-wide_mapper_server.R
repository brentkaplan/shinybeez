box::use(
  testthat[...],
  shiny,
  vroom,
)

box::use(
  app / view / wide_mapper,
)

fixture <- function(name) {
  dat <- vroom$vroom(testthat::test_path("fixtures", name), show_col_types = FALSE)
  colnames(dat) <- trimws(tolower(colnames(dat)))
  dat
}

request_for <- function(dat, target = "demand", token = 1L) {
  list(
    dat = dat, reason = "The first column is not `id`", token = token, target = target,
    meta = list(name = "wide.csv", ext = "csv", size = 100)
  )
}

describe("wide_mapper server", {
  it("builds a valid spec from prefilled guesses plus manual prices and returns the long frame", {
    request <- shiny$reactiveVal(NULL)
    shiny$testServer(wide_mapper$server, args = list(request_r = request), {
      request(request_for(fixture("wide-qualtrics-apt.csv")))
      session$flushReact()
      # The client would post the prefilled inputs; testServer has no client, so set them.
      session$setInputs(
        opened = 1L, id_col = "responseid", x_source = "manual",
        series_cols_1 = paste0("apt_", 1:5), series_x_1 = "0 0.5 1 5 10", series_label_1 = "",
        group_col = ""
      )
      expect_true(isTRUE(validation()))
      expect_match(output$preview_status$html, "3 participants")
      session$setInputs(confirm = 1)
      res <- result()
      expect_equal(res$token, 1L)
      expect_equal(colnames(res$data), c("id", "x", "y"))
      expect_equal(nrow(res$data), 15)
      expect_equal(res$meta$name, "wide.csv")
      expect_null(cancelled())
    })
  })

  it("keeps confirm disabled and shows the message while the x count is wrong", {
    request <- shiny$reactiveVal(NULL)
    shiny$testServer(wide_mapper$server, args = list(request_r = request), {
      request(request_for(fixture("wide-qualtrics-apt.csv")))
      session$flushReact()
      session$setInputs(
        opened = 1L, id_col = "responseid", x_source = "manual",
        series_cols_1 = paste0("apt_", 1:5), series_x_1 = "0 0.5 1 5", series_label_1 = "", group_col = ""
      )
      expect_match(validation(), "5 columns selected but 4 prices entered")
      expect_match(output$footer$html, "disabled")
      session$setInputs(confirm = 1)
      expect_null(result())
    })
  })

  it("reads prices from headers when they parse", {
    request <- shiny$reactiveVal(NULL)
    shiny$testServer(wide_mapper$server, args = list(request_r = request), {
      request(request_for(fixture("wide-price-suffix.csv")))
      session$flushReact()
      session$setInputs(
        opened = 1L, id_col = "participant", x_source = "header",
        series_cols_1 = c("price_0", "price_0.5", "price_1", "price_5", "price_10"),
        series_label_1 = "", group_col = ""
      )
      expect_true(isTRUE(validation()))
      expect_equal(current_spec()$series[[1]]$x, c(0, 0.5, 1, 5, 10))
    })
  })

  it("cancel reports the token and reason and clears state", {
    request <- shiny$reactiveVal(NULL)
    shiny$testServer(wide_mapper$server, args = list(request_r = request), {
      request(request_for(fixture("wide-qualtrics-apt.csv"), token = 7L))
      session$flushReact()
      session$setInputs(cancel = 1)
      expect_equal(cancelled()$token, 7L)
      expect_match(cancelled()$reason, "not `id`")
      expect_null(state$req)
    })
  })

  it("a NULL request clears a pending mapping", {
    request <- shiny$reactiveVal(NULL)
    shiny$testServer(wide_mapper$server, args = list(request_r = request), {
      request(request_for(fixture("wide-qualtrics-apt.csv")))
      session$flushReact()
      expect_false(is.null(state$req))
      request(NULL)
      session$flushReact()
      expect_null(state$req)
    })
  })

  it("emits series and carried columns on the mixed-effects target", {
    request <- shiny$reactiveVal(NULL)
    shiny$testServer(wide_mapper$server, args = list(request_r = request), {
      dat <- fixture("wide-me-with-covariates.csv")
      request(request_for(dat, target = "mixed_effects_demand"))
      session$flushReact()
      session$setInputs(
        opened = 1L, id_col = "subject", x_source = "manual",
        series_cols_1 = paste0("apt_", 1:4), series_x_1 = "0,1,2,3", series_label_1 = "",
        keep_cols = c("age", "sex")
      )
      expect_true(isTRUE(validation()))
      expect_match(output$preview_status$html, "4 rows will also be dropped for missing age/sex")
      session$setInputs(confirm = 1)
      expect_equal(colnames(result()$data), c("id", "x", "y", "age", "sex"))
    })
  })

  it("adds and removes a series", {
    request <- shiny$reactiveVal(NULL)
    shiny$testServer(wide_mapper$server, args = list(request_r = request), {
      request(request_for(fixture("wide-two-commodities.csv")))
      session$flushReact()
      expect_equal(state$n_series, 2L)
      session$setInputs(add_series = 1)
      expect_equal(state$n_series, 3L)
      session$setInputs(remove_series = 1)
      expect_equal(state$n_series, 2L)
    })
  })

  it("a second request renders from its own guesses, not the previous modal's inputs", {
    request <- shiny$reactiveVal(NULL)
    shiny$testServer(wide_mapper$server, args = list(request_r = request), {
      request(request_for(fixture("wide-qualtrics-apt.csv"), token = 1L))
      session$flushReact()
      session$setInputs(
        opened = 1L, id_col = "responseid", x_source = "manual",
        series_cols_1 = paste0("apt_", 1:5), series_x_1 = "0 0.5 1 5 10", series_label_1 = "", group_col = ""
      )
      session$setInputs(cancel = 1)
      request(request_for(fixture("wide-price-suffix.csv"), token = 2L))
      session$flushReact()
      html <- output$series_ui$html
      expect_match(html, "price_0.5", fixed = TRUE)
      expect_false(grepl("apt_1", html, fixed = TRUE))
      expect_false(state$carry)
      session$setInputs(add_series = 1)
      expect_true(state$carry)
    })
  })

  it("a second request computes its spec from its own guesses until the client acknowledges the new modal", {
    request <- shiny$reactiveVal(NULL)
    shiny$testServer(wide_mapper$server, args = list(request_r = request), {
      request(request_for(fixture("wide-qualtrics-apt.csv"), token = 1L))
      session$flushReact()
      session$setInputs(
        opened = 1L, id_col = "responseid", x_source = "manual",
        series_cols_1 = paste0("apt_", 1:5), series_x_1 = "0 0.5 1 5 10", series_label_1 = "", group_col = ""
      )
      session$setInputs(cancel = 1)
      request(request_for(fixture("wide-price-suffix.csv"), token = 2L))
      session$flushReact()
      # no client acknowledgement yet: guesses, not the first modal's inputs
      cs <- current_spec()
      expect_equal(cs$id_col, "participant")
      expect_equal(cs$series[[1]]$cols, c("price_0", "price_0.5", "price_1", "price_5", "price_10"))
      expect_true(header_x_available())
      expect_true(isTRUE(validation()))
      expect_match(output$x_source_ui$html, 'value="header" checked="checked"', fixed = TRUE)
      # client acknowledges and posts the (stale-looking) old values: still ignored until they match token 2
      session$setInputs(
        opened = 2L, id_col = "participant", x_source = "header",
        series_cols_1 = c("price_0", "price_0.5", "price_1", "price_5", "price_10"), series_label_1 = "",
        group_col = ""
      )
      expect_true(isTRUE(validation()))
      expect_equal(current_spec()$series[[1]]$x, c(0, 0.5, 1, 5, 10))
    })
  })

  it("offers header prices only when every selected column name carries one", {
    request <- shiny$reactiveVal(NULL)
    shiny$testServer(wide_mapper$server, args = list(request_r = request), {
      request(request_for(fixture("wide-qualtrics-apt.csv")))
      session$flushReact()
      session$setInputs(
        opened = 1L, id_col = "responseid", series_cols_1 = paste0("apt_", 1:5),
        series_label_1 = "", group_col = ""
      )
      expect_false(header_x_available())
      expect_false(grepl("Read from column names", output$x_source_ui$html, fixed = TRUE))
      request(request_for(fixture("wide-price-suffix.csv"), token = 2L))
      session$flushReact()
      session$setInputs(
        opened = 2L, id_col = "participant",
        series_cols_1 = c("price_0", "price_0.5", "price_1", "price_5", "price_10")
      )
      expect_true(header_x_available())
      expect_match(output$x_source_ui$html, "Read from column names", fixed = TRUE)
    })
  })

  it("resolves modal input ids under a nested namespace", {
    request <- shiny$reactiveVal(NULL)
    outer <- function(id, request_r) {
      shiny$moduleServer(id, function(input, output, session) {
        wide_mapper$server("mapper", request_r)
      })
    }
    shiny$testServer(outer, args = list(request_r = request), {
      request(request_for(fixture("wide-qualtrics-apt.csv")))
      session$flushReact()
      session$setInputs(`mapper-opened` = 1L, `mapper-id_col` = "responseid")
      expect_equal(input[["mapper-id_col"]], "responseid")
    })
  })
})
