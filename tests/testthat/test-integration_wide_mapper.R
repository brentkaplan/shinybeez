# Upload a Qualtrics-style wide purchase task on the Demand tab, reshape it in the
# modal, and fit. Gated like the other integration tests.

describe("Demand - wide-to-long mapper", {
  skip_if_not_integration()
  app <- create_app_driver()
  result_id <- ns_id("demand", "results_table_demand", "model_results_table")
  # Every modal input id carries the reshape request's token (see rid() in
  # app/view/wide_mapper.R); file_input.R bumps the token on every upload. On this tab
  # the first upload is token 1, the second is token 2.
  mapper <- function(token, input) ns_id("demand", "upload_demand", "mapper", sprintf("r%d_%s", token, input))

  it("opens the modal for a wide Qualtrics export", {
    require_app(app)
    navigate_to_tab(app, "Demand")
    app$upload_file(!!ids$demand$upload := fixture_path("wide-qualtrics-apt.csv"))
    app$wait_for_js("document.querySelector('.modal.show') !== null", timeout = 10000)
    # id_col renders inside the modal's server-rendered body, so wait for it to post
    expect_equal(app$wait_for_value(input = mapper(1, "id_col"), timeout = 5000), "responseid")
    # the radio is a renderUI output; its value posts after the modal is visible
    expect_equal(app$wait_for_value(input = mapper(1, "x_source"), timeout = 5000), "manual")
  })

  it("enables confirm once prices are entered and loads the long data", {
    require_app(app)
    app$set_inputs(!!mapper(1, "series_x_1") := "0, 0.5, 1, 5, 10")
    app$wait_for_idle(duration = 500)
    app$wait_for_js(
      sprintf(
        "(function(){var b=document.getElementById('%s');return b!==null && !b.disabled;})()", mapper(1, "confirm")
      ),
      timeout = 5000
    )
    app$click(selector = paste0("#", mapper(1, "confirm")))
    wait_for_datatable(app)
    wait_for_notification(app, "message")
    html <- app$get_html(".datatables")
    expect_true(any(grepl("<td", html, fixed = TRUE)))
  })

  it("fits a pooled demand curve on the reshaped data", {
    require_app(app)
    app$click(selector = paste0("#", ids$demand$calculate))
    wait_for_output(app, result_id, timeout_ms = 15000)
    expect_demand_results(app, result_id, n_rows = 1)
  })

  it("a second wide file in the same session gets its own prefill", {
    require_app(app)
    app$upload_file(!!ids$demand$upload := fixture_path("wide-price-suffix.csv"))
    app$wait_for_js("document.querySelector('.modal.show') !== null", timeout = 10000)
    app$wait_for_idle(duration = 500)
    # request 2's inputs (r2_*) are brand-new ids the client has never posted a value for
    # under request 1, so there is no stale value to race with - the guessed prefill shows
    # up as soon as the modal's own uiOutputs render.
    expect_equal(app$wait_for_value(input = mapper(2, "id_col"), timeout = 5000), "participant")
    expect_equal(app$wait_for_value(input = mapper(2, "x_source"), timeout = 5000), "header")
    app$wait_for_js(
      sprintf(
        "(function(){var b=document.getElementById('%s');return b!==null && !b.disabled;})()", mapper(2, "confirm")
      ),
      timeout = 5000
    )
    app$click(selector = paste0("#", mapper(2, "cancel")))
    app$wait_for_idle(duration = 500)
  })

  it("maps a misnamed long file end to end", {
    require_app(app)
    # third upload on this tab, so the request token is 3
    app$upload_file(!!ids$demand$upload := fixture_path("long-misnamed.csv"))
    app$wait_for_js("document.querySelector('.modal.show') !== null", timeout = 10000)
    expect_equal(app$wait_for_value(input = mapper(3, "layout"), timeout = 5000), "long")
    expect_equal(app$wait_for_value(input = mapper(3, "long_x_col"), timeout = 5000), "price")
    app$wait_for_js(
      sprintf(
        "(function(){var b=document.getElementById('%s');return b!==null && !b.disabled;})()", mapper(3, "confirm")
      ),
      timeout = 5000
    )
    app$click(selector = paste0("#", mapper(3, "confirm")))
    wait_for_datatable(app)
    wait_for_notification(app, "message")
    expect_true(any(grepl("<td", app$get_html(".datatables"), fixed = TRUE)))
  })

  local_app_stop()
})
