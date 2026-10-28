# After a failed reshape the discounting navpanel sets data_d to NULL (see
# test-navpanel_prepare_guard.R). The results box used to render its empty tab shell
# for that NULL; it now waits for data like the data box does.

box::use(
  testthat[...],
  shiny,
)

box::use(
  app / view / discounting_results_table,
)

module_args <- function(data_r) {
  list(
    data_r = data_r,
    eq = shiny$reactive("Mazur"),
    agg = shiny$reactive("Ind"),
    type = shiny$reactive("Indifference Point Regression"),
    calculate_btn = shiny$reactiveVal(NULL)
  )
}

describe("discounting results box with no prepared data", {
  it("renders nothing while data_d is NULL and renders once data arrives", {
    data_r <- shiny$reactiveValues(data_d = NULL)

    shiny$testServer(discounting_results_table$server, args = module_args(data_r), {
      expect_error(output$results_box, class = "shiny.silent.error")

      data_r$data_d <- data.frame(
        id = factor(c("p1", "p1", "p1")), x = c(1, 5, 100), y = c(0.9, 0.7, 0.2)
      )
      session$flushReact()
      expect_false(is.null(output$results_box))
    })
  })
})
