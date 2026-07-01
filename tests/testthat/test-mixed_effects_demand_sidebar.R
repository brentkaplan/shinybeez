# Regression test for app/view/mixed_effects_demand_sidebar.R
#
# Telemetry signature c371dccc (`could not find function "ns"`, 4 occurrences).
# `sidebar_server()` bound the module namespacer with `ns <- session$NS`, but a
# Shiny session exposes the lowercase `session$ns`; `session$NS` is NULL. Because
# R skips non-function bindings during function-call lookup, every `ns(...)` in
# the server raised `could not find function "ns"`, breaking the exponentiated
# Mixed-Effects Demand path (the k-value selector rendered by output$k_value_mixed).

box::use(
  shiny[reactive, testServer],
  testthat[describe, expect_no_error, expect_true, it],
)

box::use(
  app / view / mixed_effects_demand_sidebar,
)

describe("mixed_effects_demand_sidebar$sidebar_server()", {
  it("renders the exponentiated k-value input without a namespacing error", {
    testServer(
      mixed_effects_demand_sidebar$sidebar_server,
      args = list(data_reactive = reactive(NULL)),
      {
        # Selecting the exponentiated model reveals the k-value selectInput,
        # whose inputId is built with ns("k_mixed"). With the `session$NS`
        # typo this raised `could not find function "ns"`; the fix
        # (`ns <- session$ns`) lets the output render cleanly.
        session$setInputs(model_choice = "exponentiated")
        expect_no_error(output$k_value_mixed)
        expect_true(!is.null(output$k_value_mixed))
      }
    )
  })
})
