# Regression test for production error c371dccc:
#
#   could not find function "ns"
#
# app/view/mixed_effects_demand_sidebar.R bound the namespace helper with
# `ns <- session$NS`. There is no `$NS` on a session object — it is `session$ns` — so `ns`
# was NULL. R skips non-function bindings when resolving a call, so `ns("k_mixed")` walked
# past the NULL and failed to find any function named `ns`, producing the message above.
#
# It only fires on the exponentiated model: output$k_value_mixed is the sole server-side
# renderUI that calls ns(), and it is gated on `input$model_choice == "exponentiated"`
# (mixed_effects_demand_sidebar.R:326-335). That matches the production signature, which the
# 2026-07-01 investigation described as fully breaking the exponentiated Mixed-Effects
# Demand path.

box::use(
  testthat[...],
  shiny,
)

box::use(
  app / view / mixed_effects_demand_sidebar,
)

describe("mixed effects demand sidebar namespacing", {
  it("renders the k-value input for the exponentiated model (c371dccc)", {
    shiny$testServer(
      mixed_effects_demand_sidebar$sidebar_server,
      args = list(data_reactive = shiny$reactive(NULL)),
      {
        # Selecting the exponentiated equation is the only path that calls ns() server-side.
        session$setInputs(model_choice = "exponentiated")

        # Pre-fix this raised: could not find function "ns"
        expect_no_error(output$k_value_mixed)
        expect_true(nzchar(as.character(output$k_value_mixed$html)))
      }
    )
  })

  it("namespaces the k-value input id under the module", {
    shiny$testServer(
      mixed_effects_demand_sidebar$sidebar_server,
      args = list(data_reactive = shiny$reactive(NULL)),
      {
        session$setInputs(model_choice = "exponentiated")

        # A working ns() prefixes the module id; a broken one could not have rendered at all.
        expect_match(
          as.character(output$k_value_mixed$html),
          "k_mixed",
          fixed = TRUE
        )
      }
    )
  })

  it("renders nothing for a non-exponentiated model", {
    shiny$testServer(
      mixed_effects_demand_sidebar$sidebar_server,
      args = list(data_reactive = shiny$reactive(NULL)),
      {
        session$setInputs(model_choice = "exponential")
        expect_no_error(output$k_value_mixed)
      }
    )
  })
})
