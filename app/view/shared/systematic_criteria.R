#' Shared Systematic Criteria Component
#'
#' Reusable UI for systematic criteria settings used in demand analysis.

box::use(
  bslib,
  DT,
  shiny
)

#' Create systematic criteria sidebar UI
#'
#' @param id Module namespace ID
#' @param type Type of criteria ("demand" or "discounting")
#' @return bslib sidebar element
#' @export
sidebar_ui <- function(id, type = "demand") {
  ns <- shiny$NS(id)

  if (type == "demand") {
    bslib$sidebar(
      title = "Systematic Settings",
      open = FALSE,
      shiny$numericInput(
        inputId = ns("deltaq"),
        label = "Criteria #1: Relative change (DeltaQ)",
        value = 0.025,
        min = 0,
        max = 1,
        step = 0.005
      ),
      shiny$numericInput(
        inputId = ns("bounce"),
        label = "Criteria #2: Price to price increases (Bounce)",
        value = 0.10,
        min = 0,
        max = 1,
        step = 0.01
      ),
      shiny$numericInput(
        inputId = ns("reversals"),
        label = "Criteria #3: Number of reversals (Reversals)",
        value = 0,
        min = 0,
        max = 4,
        step = 1
      ),
      shiny$numericInput(
        inputId = ns("ncons0"),
        label = "Criteria #3a: Consecutive 0s for reversals",
        value = 2,
        min = 0,
        max = 4,
        step = 1
      )
    )
  } else {
    # Discounting criteria
    bslib$sidebar(
      title = "Systematic Settings",
      open = FALSE,
      shiny$numericInput(
        inputId = ns("c1"),
        label = "Criteria #1: Local change",
        value = 0.2,
        min = 0,
        max = 1,
        step = 0.005
      ),
      shiny$numericInput(
        inputId = ns("c2"),
        label = "Criteria #2: Global trend",
        value = 0.10,
        min = 0,
        max = 1,
        step = 0.01
      )
    )
  }
}

#' Systematic criteria panel with sidebar and table
#'
#' @param id Module namespace ID
#' @param type Type of criteria ("demand" or "discounting")
#' @return Card with sidebar layout
#' @export
panel_ui <- function(id, type = "demand") {
  ns <- shiny$NS(id)

  bslib$card(
    style = "border: none;",
    bslib$layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar_ui(id, type = type),
      DT$DTOutput(ns("table"))
    )
  )
}

#' Get current criteria values as a reactive
#'
#' @param id Module namespace ID
#' @param type Type of criteria ("demand" or "discounting")
#' @return Reactive list with criteria values
#' @export
server <- function(id, type = "demand") {
  shiny$moduleServer(id, function(input, output, session) {
    # Return reactive with criteria values
    if (type == "demand") {
      shiny$reactive({
        list(
          deltaq = input$deltaq,
          bounce = input$bounce,
          reversals = input$reversals,
          ncons0 = input$ncons0
        )
      })
    } else {
      shiny$reactive({
        list(
          c1 = input$c1,
          c2 = input$c2
        )
      })
    }
  })
}
