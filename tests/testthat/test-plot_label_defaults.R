# The demand and discounting Plot Settings sidebars must ship real title and axis text,
# not the old "title" / "x" / "y" placeholders.

box::use(
  htmltools,
  testthat[...],
  shiny,
)

box::use(
  app / view / demand_results_table,
  app / view / discounting_results_table,
)

# Pull the value attribute of the text input `id` out of rendered HTML.
input_value <- function(html, id) {
  tag <- regmatches(html, regexpr(sprintf('<input[^>]*id="%s"[^>]*>', id), html))
  expect_length(tag, 1)
  sub('.*value="([^"]*)".*', "\\1", tag)
}

describe("demand plot label defaults", {
  it("ships real title and axis text", {
    html <- htmltools$renderTags(demand_results_table$ui("d"))$html
    expect_identical(input_value(html, "d-title"), "Demand Curve")
    expect_identical(input_value(html, "d-xtext"), "Price")
    expect_identical(input_value(html, "d-ytext"), "Consumption")
  })
})

describe("discounting plot label defaults", {
  it("ships real title and axis text on the regression plot sidebar", {
    data_r <- shiny$reactiveValues(data_d = data.frame(id = "1", x = 1, y = 1))
    args <- list(
      data_r = data_r,
      eq = shiny$reactive("hyperbolic"),
      agg = shiny$reactive("Mean"),
      type = shiny$reactive("Indifference Point Regression"),
      calculate_btn = shiny$reactive(0)
    )

    shiny$testServer(discounting_results_table$server, args = args, {
      html <- as.character(output$results_box$html)
      expect_identical(input_value(html, session$ns("title")), "")
      expect_identical(input_value(html, session$ns("xtext")), "Delay")
      expect_identical(input_value(html, session$ns("ytext")), "Indifference Point")
    })
  })
})
