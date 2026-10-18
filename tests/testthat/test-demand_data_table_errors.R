# 2026-10-15 (demand_empirical_measures :: Problem with argument `data`:): beezdemand
# raises cli errors whose e$message is only the header. The handlers logged and showed
# e$message, so the user (and telemetry) saw "Problem with argument `data`:" and never
# the reason. A duplicate price within one id reproduces it: descriptives and a Pooled
# fit accept the data, get_empirical_measures() does not.

box::use(
  testthat[...],
  shiny,
)

box::use(
  app / view / demand_data_table,
)

duplicated_price_data <- function() {
  prices <- c(0.01, 0.1, 1, 10, 100)
  d <- data.frame(
    id = rep(c("1", "2"), each = 5),
    x = rep(prices, 2),
    y = c(10, 8, 5, 2, 0, 12, 9, 6, 3, 1)
  )
  rbind(d, d[1, ])
}

describe("demand data table error notifications", {
  it("shows the cause of a beezdemand error, not just its header", {
    shown <- character()
    # box gives the view module its own locked copy of the shiny exports, so
    # swap showNotification there.
    shiny_env <- environment(demand_data_table$server)$shiny
    fn <- "showNotification"
    original <- shiny_env[[fn]]
    unlockBinding(fn, shiny_env)
    assign(fn, function(ui, ...) {
      shown <<- c(shown, as.character(ui))
      invisible(NULL)
    }, envir = shiny_env)
    withr::defer({
      assign(fn, original, envir = shiny_env)
      lockBinding(fn, shiny_env)
    })
    shiny$testServer(
      demand_data_table$server,
      args = list(
        isgroup = shiny$reactive(FALSE),
        data_r = shiny$reactiveValues(data_d = duplicated_price_data())
      ),
      {
        session$userData$data <- list(demand = TRUE)
        expect_null(empirical_r())
      }
    )
    expect_true(any(grepl("Duplicates found where id", shown, fixed = TRUE)))
  })
})
