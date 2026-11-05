# Connect Cloud, 2026-10-19: every cold start renders the UI with an empty sass cache, and
# font_google()'s default (local = TRUE) downloaded Fraunces and Fira Mono from Google inside
# that first render. When Google was slow the single R process blocked for minutes, the proxy
# 503'd the app's own assets after 10 s, and Shiny never connected. The fonts must be linked
# for the browser to fetch, not downloaded by the server.

box::use(
  testthat[...],
)

box::use(
  app / logic / theme,
)

describe("app_theme fonts", {
  it("links the Google fonts instead of downloading them on the server", {
    deps <- bslib::bs_theme_dependencies(theme$app_theme())
    fonts <- Filter(function(d) d$name %in% c("Fraunces", "Fira_Mono"), deps)

    expect_length(fonts, 2L)
    for (d in fonts) {
      expect_match(d$head %||% "", "https://fonts.googleapis.com/css2", fixed = TRUE)
      expect_null(d$stylesheet)
    }
  })
})
