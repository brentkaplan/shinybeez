# The app shell wires the consent module in: the GA tag is no longer emitted by R (the
# browser loads it only at the usage level), the page carries the config the JS handshake
# needs, the shield nav item and the banner slot are present, and the Welcome page explains
# the levels and offers a way back to the choice.

box::use(
  testthat[...],
  withr,
)

box::use(
  app / main,
)

# The config script lives in <head>, which as.character() on a page drops; render the
# whole document the way Shiny serves it.
render_ui <- function(env = character()) {
  withr::with_envvar(
    c(R_CONFIG_ACTIVE = "default", TELEMETRY_ENABLED = "FALSE", env),
    withr::with_dir(
      test_path("..", ".."),
      {
        rendered <- htmltools::renderTags(main$ui("app"))
        paste(as.character(rendered$head), as.character(rendered$html))
      }
    )
  )
}

describe("main$ui() consent wiring", {
  it("emits the handshake config instead of a Google tag", {
    html <- render_ui(c(GA_ENABLED = "TRUE", GA_MEASUREMENT_ID = "G-TEST"))
    expect_match(html, "window.SHINYBEEZ", fixed = TRUE)
    expect_match(html, '"gaId":"G-TEST"', fixed = TRUE)
    expect_match(html, '"platform":"web"', fixed = TRUE)
    expect_match(html, '"policy":1', fixed = TRUE)
    expect_no_match(html, "googletagmanager", fixed = TRUE)
  })

  it("emits an empty gaId when GA is off", {
    html <- render_ui(c(GA_ENABLED = "FALSE"))
    expect_match(html, '"gaId":""', fixed = TRUE)
  })

  it("places the shield nav item and the banner slot", {
    html <- render_ui()
    expect_match(html, 'id="app-consent-open"', fixed = TRUE)
    expect_match(html, 'id="app-consent-banner"', fixed = TRUE)
  })
})

describe("welcome.html privacy copy", {
  html <- paste(readLines(file.path(project_root, "app/static/html/welcome.html")), collapse = "\n")

  it("has the Privacy and analytics section and the change-your-choice button", {
    expect_match(html, 'id="privacy-analytics"', fixed = TRUE)
    expect_match(html, 'id="consent-open-welcome"', fixed = TRUE)
    expect_match(html, "Anonymous usage statistics are the exception", fixed = TRUE)
    expect_match(html, "Diagnostics only", fixed = TRUE)
  })
})
