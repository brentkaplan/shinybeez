# Upload a Qualtrics-style wide purchase task on the Demand tab, reshape it in the
# modal, and fit. Gated like the other integration tests.

describe("Demand - wide-to-long mapper", {
  skip_if_not_integration()
  app <- create_app_driver()
  result_id <- ns_id("demand", "results_table_demand", "model_results_table")
  mapper <- function(input) ns_id("demand", "upload_demand", "mapper", input)

  it("opens the modal for a wide Qualtrics export", {
    require_app(app)
    navigate_to_tab(app, "Demand")
    app$upload_file(!!ids$demand$upload := fixture_path("wide-qualtrics-apt.csv"))
    app$wait_for_js("document.querySelector('.modal.show') !== null", timeout = 10000)
    expect_equal(app$get_value(input = mapper("id_col")), "responseid")
    expect_equal(app$get_value(input = mapper("x_source")), "manual")
  })

  it("enables confirm once prices are entered and loads the long data", {
    require_app(app)
    app$set_inputs(!!mapper("series_x_1") := "0, 0.5, 1, 5, 10")
    app$wait_for_idle(duration = 500)
    app$wait_for_js(
      sprintf("(function(){var b=document.getElementById('%s');return b!==null && !b.disabled;})()", mapper("confirm")),
      timeout = 5000
    )
    app$click(selector = paste0("#", mapper("confirm")))
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

  local_app_stop()
})
