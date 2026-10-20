# Tests for the aesthetic render gate (app/view/shared/aesthetic_gate.R)
#
# The harness mirrors the mixed-effects navpanel wiring: a refit observer (priority 10) pushes
# the expected selections, a bindEvent'd plot reactive reads the live colour input, and the
# render waits on the gate.

box::use(
  shiny,
  testthat[...],
)

box::use(
  app / view / shared / aesthetic_gate,
)

expected_selections <- list(color = "dose", linetype = "", facet = "", shape = "")

harness <- function(timeout_ms = 3000) {
  function(input, output, session) {
    fit <- shiny$reactiveVal(NULL)
    gate <- aesthetic_gate$new(
      function() {
        list(
          color = input$color, linetype = input$linetype,
          facet = input$facet, shape = input$shape
        )
      },
      timeout_ms = timeout_ms
    )
    shiny$observe(gate$push(expected_selections), priority = 10) |>
      shiny$bindEvent(fit())
    plot_r <- shiny$reactive(input$color) |>
      shiny$bindEvent(input$go, fit())
    shown <- shiny$reactive({
      shiny$req(gate$open())
      plot_r()
    })
    refit <- function(n) {
      fit(n)
      session$flushReact()
    }
    # What the render would show: the plot's colour, or "BLOCKED" while the gate is shut.
    shown_or_blocked <- function() {
      tryCatch(shown(), shiny.silent.error = function(e) "BLOCKED")
    }
  }
}

describe("aesthetic_gate", {
  it("renders the new colour once the browser echoes the pushed selection", {
    shiny$testServer(harness(), {
      session$setInputs(color = "drug")
      refit(1)
      expect_equal(shown_or_blocked(), "BLOCKED")
      session$setInputs(color = "dose")
      expect_equal(shown_or_blocked(), "dose")
    })
  })

  it("keeps the gate shut on an Update Plot click before the echo, then renders the new colour", {
    shiny$testServer(harness(), {
      session$setInputs(color = "drug")
      refit(1)
      session$setInputs(go = 1)
      expect_equal(shown_or_blocked(), "BLOCKED")
      expect_false(gate$open())
      session$setInputs(color = "dose")
      expect_equal(shown_or_blocked(), "dose")
    })
  })

  it("opens by itself when the browser never echoes within the timeout", {
    shiny$testServer(harness(timeout_ms = 3000), {
      session$setInputs(color = "drug")
      refit(1)
      expect_false(gate$open())
      session$elapse(2900)
      expect_false(gate$open())
      session$elapse(200)
      expect_true(gate$open())
    })
  })

  it("never closes when the pushed selections already match the live inputs", {
    shiny$testServer(harness(), {
      session$setInputs(color = "dose")
      refit(1)
      expect_true(gate$open())
      expect_equal(shown_or_blocked(), "dose")
    })
  })

  it("treats unset inputs as None when comparing", {
    shiny$testServer(harness(), {
      session$setInputs(color = "dose", linetype = "", facet = NULL)
      refit(1)
      expect_true(gate$open())
    })
  })

  it("exposes the pending selections while closed and NULL once open", {
    shiny$testServer(harness(), {
      session$setInputs(color = "drug")
      refit(1)
      expect_equal(gate$pending(), expected_selections)
      session$setInputs(color = "dose")
      expect_null(gate$pending())
    })
  })

  it("re-arms the timeout for a later push", {
    shiny$testServer(harness(timeout_ms = 3000), {
      session$setInputs(color = "drug")
      refit(1)
      session$setInputs(color = "dose")
      expect_true(gate$open())
      session$setInputs(color = "drug")
      refit(2)
      expect_false(gate$open())
      session$elapse(2900)
      expect_false(gate$open())
      session$elapse(200)
      expect_true(gate$open())
    })
  })
})
