# Tests for the shared plot Layers section (app/view/shared/plot_layers.R)

box::use(
  shiny,
  testthat[...],
)

box::use(
  app / logic / plot_style,
  app / view / shared / plot_layers,
)

# updateNumericInput() is a silent no-op under testServer: MockShinySession has no
# sendInputMessage(). Install a recorder on a root session and pass it as `session =`.
# Recorded ids are un-namespaced and values arrive as strings.
recording_session <- function() {
  root <- shiny$MockShinySession$new()
  root$sent <- list()
  root$sendInputMessage <- function(input_id, message) {
    root$sent[[length(root$sent) + 1]] <- list(id = input_id, value = as.numeric(message$value))
    invisible(NULL)
  }
  root
}

describe("plot_layers$ui", {
  html <- as.character(plot_layers$ui("layers"))

  it("renders every control under the module namespace", {
    for (id in c(
      "show_population", "prom_population", "show_individual", "prom_individual", "show_observed",
      "pop_alpha", "pop_width", "ind_alpha", "ind_width", "obs_alpha", "obs_size"
    )) {
      expect_match(html, paste0("layers-", id), fixed = TRUE, info = id)
    }
  })

  it("seeds the sliders from prominence_defaults and the numerics from style_defaults", {
    expect_match(html, 'data-from="75"', fixed = TRUE)
    expect_match(html, 'data-from="25"', fixed = TRUE)
    expect_match(html, 'id="layers-pop_alpha"[^>]*value="0.9"', perl = TRUE)
    expect_match(html, 'id="layers-ind_width"[^>]*value="0.6"', perl = TRUE)
    expect_match(html, 'id="layers-obs_size"[^>]*value="2"', perl = TRUE)
  })

  it("labels the accordion as overriding the sliders", {
    expect_match(html, "Advanced (overrides the sliders)", fixed = TRUE)
  })
})

describe("plot_layers$server", {
  it("returns the engine defaults before any input arrives", {
    shiny$testServer(plot_layers$server, args = list(id = "layers", engine = "beezdemand_nlme"), {
      expect_equal(style(), plot_style$style_defaults("beezdemand_nlme"))
    })
  })

  it("pushes slider moves into the numeric boxes", {
    root <- recording_session()
    shiny$testServer(
      plot_layers$server,
      args = list(id = "layers", engine = "beezdemand_nlme"),
      session = root,
      {
        # The client sends the initial slider values on connect; under testServer that
        # first flush is the ignoreInit run, so send it before the move under test.
        session$setInputs(prom_population = 75, prom_individual = 25)
        expect_length(root$sent, 0)

        session$setInputs(prom_population = 100)
        expect_equal(root$sent[[1]], list(id = "pop_alpha", value = 1))
        expect_equal(root$sent[[2]], list(id = "pop_width", value = 1.2))

        session$setInputs(prom_individual = 0)
        expect_equal(root$sent[[3]], list(id = "ind_alpha", value = 0.1))
        expect_equal(root$sent[[4]], list(id = "ind_width", value = 0.2))
      }
    )
  })

  it("treats the numeric boxes as the source of truth and validates them", {
    shiny$testServer(plot_layers$server, args = list(id = "layers", engine = "beezdemand_nlme"), {
      session$setInputs(
        show_population = TRUE, show_individual = TRUE, show_observed = FALSE,
        pop_alpha = 0.5, pop_width = 2, ind_alpha = 9, ind_width = NA, obs_alpha = 0.6, obs_size = 2
      )
      s <- style()$layers
      expect_equal(s$population, list(show = TRUE, alpha = 0.5, width = 2))
      expect_equal(s$individual, list(show = TRUE, alpha = 1, width = 0.6))
      expect_false(s$observed$show)
    })
  })

  it("does not write back to a slider when a numeric box is edited", {
    root <- recording_session()
    shiny$testServer(
      plot_layers$server,
      args = list(id = "layers", engine = "beezdemand_nlme"),
      session = root,
      {
        session$setInputs(prom_population = 75, prom_individual = 25)
        session$setInputs(prom_population = 50)
        n_before <- length(root$sent)
        session$setInputs(pop_alpha = 0.33)
        expect_equal(length(root$sent), n_before)
        expect_equal(style()$layers$population$alpha, 0.33)
      }
    )
  })
})
