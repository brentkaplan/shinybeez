#' Wide-to-long column mapper modal
#'
#' Opened by file_input when an upload fails template validation. Owns the modal,
#' builds a reshape spec from the inputs, previews the long frame, and hands the
#' converted frame back through `result()`. Knows nothing about storage or telemetry.

box::use(
  bslib,
  bsicons,
  DT[DTOutput, renderDT],
  shiny,
  utils,
)

box::use(
  app / logic / reshape / detect,
  app / logic / reshape / spec,
)

`%||%` <- function(a, b) if (is.null(a)) b else a

x_noun <- function(target) if (target == "discounting") "delays" else "prices"
x_label <- function(target) if (target == "discounting") "Delays" else "Prices"
cols_label <- function(target) if (target == "discounting") "Delay columns" else "Price columns"

#' Static UI: none. The modal is built server-side on request.
#' @export
ui <- function(id) {
  NULL
}

series_block <- function(ns, target, i, cols, series, show_manual_condition) {
  bslib$card(
    class = "mb-2",
    bslib$card_body(
      shiny$textInput(
        ns(paste0("series_label_", i)),
        "Series name (used as the group when there is more than one series)",
        value = series$label %||% ""
      ),
      shiny$selectizeInput(
        ns(paste0("series_cols_", i)),
        cols_label(target),
        choices = cols,
        selected = series$cols,
        multiple = TRUE,
        options = list(plugins = list("remove_button"), placeholder = "Pick the response columns in order")
      ),
      shiny$conditionalPanel(
        condition = show_manual_condition,
        shiny$textAreaInput(
          ns(paste0("series_x_", i)),
          paste(x_label(target), "in column order (comma, space or newline separated)"),
          value = if (!is.null(series$x)) paste(series$x, collapse = ", ") else "",
          rows = 2
        )
      )
    )
  )
}

modal_ui <- function(ns, req, guess) {
  dat <- req$dat
  cols <- colnames(dat)
  target <- req$target
  manual_condition <- sprintf("input['%s'] == 'manual'", ns("x_source"))
  other_cols <- setdiff(cols, c(guess$id_col, unlist(lapply(guess$series, `[[`, "cols"))))

  extras <- switch(
    target,
    demand = shiny$selectInput(
      ns("group_col"), "Group column (optional)",
      choices = c("None" = "", other_cols), selected = ""
    ),
    mixed_effects_demand = shiny$selectizeInput(
      ns("keep_cols"), "Columns to carry along as covariates or factors (optional)",
      choices = other_cols, selected = NULL, multiple = TRUE,
      options = list(plugins = list("remove_button"))
    ),
    discounting = NULL
  )

  shiny$modalDialog(
    title = "Reshape your data",
    size = "xl",
    easyClose = FALSE,
    footer = shiny$uiOutput(ns("footer")),
    shiny$p(
      "This file doesn't match a shinybeez template: ", shiny$em(req$reason),
      " Tell us how to reshape it. ",
      shiny$span(class = "text-muted", sprintf("%d rows × %d columns detected.", nrow(dat), ncol(dat)))
    ),
    shiny$p(
      class = "text-muted small",
      "Already one row per observation? Rename your columns to match the long template on the Welcome tab instead."
    ),
    shiny$selectInput(
      ns("id_col"), "Participant id column",
      choices = cols, selected = guess$id_col %||% cols[1]
    ),
    shiny$h6(cols_label(target)),
    shiny$uiOutput(ns("series_ui")),
    if (target != "discounting") {
      shiny$div(
        class = "mb-3",
        shiny$actionButton(ns("add_series"), "Add another series", class = "btn-outline-secondary btn-sm"),
        shiny$actionButton(ns("remove_series"), "Remove last series", class = "btn-outline-secondary btn-sm ms-2")
      )
    },
    shiny$radioButtons(
      ns("x_source"), x_label(target),
      choices = c("Read from column names" = "header", "Enter them" = "manual"),
      selected = guess$x_source, inline = TRUE
    ),
    shiny$uiOutput(ns("pairs_ui")),
    extras,
    shiny$h6("Preview"),
    shiny$uiOutput(ns("preview_status")),
    DTOutput(ns("preview"))
  )
}

#' @export
server <- function(id, request_r) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    state <- shiny$reactiveValues(req = NULL, guess = NULL, n_series = 0L)
    result <- shiny$reactiveVal(NULL)
    cancelled <- shiny$reactiveVal(NULL)

    shiny$observeEvent(request_r(), ignoreNULL = FALSE, {
      req <- request_r()
      if (is.null(req)) {
        if (!is.null(state$req)) shiny$removeModal()
        state$req <- NULL
        return()
      }
      guess <- detect$guess_spec(req$dat, req$target)
      state$req <- req
      state$guess <- guess
      state$n_series <- length(guess$series)
      shiny$showModal(modal_ui(ns, req, guess))
    })

    shiny$observeEvent(input$add_series, {
      state$n_series <- state$n_series + 1L
    })
    shiny$observeEvent(input$remove_series, {
      if (state$n_series > 1L) state$n_series <- state$n_series - 1L
    })

    # Series blocks are re-rendered when their number changes; carry the current
    # inputs across so adding a series does not reset the others to the guess.
    output$series_ui <- shiny$renderUI({
      req <- state$req
      shiny$req(req)
      n <- state$n_series
      manual_condition <- sprintf("input['%s'] == 'manual'", ns("x_source"))
      shiny$isolate({
        shiny$tagList(lapply(seq_len(n), function(i) {
          guessed <- if (i <= length(state$guess$series)) state$guess$series[[i]] else spec$new_series(character(0))
          current <- spec$new_series(
            input[[paste0("series_cols_", i)]] %||% guessed$cols,
            x = if (!is.null(input[[paste0("series_x_", i)]])) {
              spec$parse_x_text(input[[paste0("series_x_", i)]])
            } else {
              guessed$x
            },
            label = input[[paste0("series_label_", i)]] %||% guessed$label
          )
          series_block(ns, req$target, i, colnames(req$dat), current, manual_condition)
        }))
      })
    })

    # Until the client posts the modal's inputs, id_col is NULL: use the guess.
    current_spec <- shiny$reactive({
      req <- state$req
      shiny$req(req)
      guess <- state$guess
      live <- !is.null(input$id_col)
      x_source <- input$x_source %||% guess$x_source
      series <- lapply(seq_len(state$n_series), function(i) {
        guessed <- if (i <= length(guess$series)) guess$series[[i]] else spec$new_series(character(0))
        cols <- if (live) input[[paste0("series_cols_", i)]] %||% character(0) else guessed$cols
        x <- if (x_source == "header") {
          detect$x_from_names(cols)
        } else if (live) {
          spec$parse_x_text(input[[paste0("series_x_", i)]])
        } else {
          guessed$x %||% numeric(0)
        }
        label <- if (live) trimws(input[[paste0("series_label_", i)]] %||% "") else guessed$label
        spec$new_series(cols, x = x, label = label)
      })
      group_col <- if (live) input$group_col else NULL
      if (is.null(group_col) || !nzchar(group_col)) group_col <- NULL
      spec$new_spec(
        target = req$target,
        id_col = if (live) input$id_col else guess$id_col,
        series = series,
        group_col = group_col,
        keep_cols = if (live) input$keep_cols %||% character(0) else character(0),
        x_source = x_source
      )
    })

    validation <- shiny$reactive({
      req <- state$req
      shiny$req(req)
      spec$validate_spec(current_spec(), req$dat)
    })

    preview <- shiny$reactive({
      if (!isTRUE(validation())) return(NULL)
      spec$apply_spec(current_spec(), state$req$dat)
    })

    output$pairs_ui <- shiny$renderUI({
      cs <- current_spec()
      noun <- x_noun(cs$target)
      shiny$tagList(lapply(seq_along(cs$series), function(i) {
        s <- cs$series[[i]]
        n_cols <- length(s$cols)
        n_x <- length(s$x)
        ok <- n_cols > 0 && n_cols == n_x && !anyNA(s$x)
        pairs <- if (ok) {
          paste0(s$cols, " → ", s$x, collapse = ", ")
        } else {
          ""
        }
        shiny$p(
          class = if (ok) "text-success small" else "text-warning small",
          shiny$strong(sprintf("%d columns · %d %s", n_cols, n_x, noun)),
          if (nzchar(pairs)) shiny$span(": ", pairs)
        )
      }))
    })

    output$preview_status <- shiny$renderUI({
      v <- validation()
      if (!isTRUE(v)) {
        return(shiny$div(class = "alert alert-warning py-2", bsicons$bs_icon("exclamation-triangle"), " ", v))
      }
      p <- preview()
      cs <- current_spec()
      n_cols <- sum(vapply(cs$series, function(s) length(s$cols), numeric(1)))
      msg <- sprintf(
        "%d series × %d participants × %d response columns → %s rows",
        length(cs$series), p$n_ids, n_cols, format(nrow(p$data), big.mark = ",")
      )
      if (p$losses$n_na_y > 0) msg <- paste0(msg, sprintf(" · %d empty responses dropped", p$losses$n_na_y))
      if (p$losses$n_na_keep > 0) {
        msg <- paste0(msg, sprintf(
          " · %d row%s will also be dropped for missing %s",
          p$losses$n_na_keep, if (p$losses$n_na_keep == 1) "" else "s",
          paste(cs$keep_cols, collapse = "/")
        ))
      }
      shiny$p(class = "text-muted small", msg)
    })

    output$preview <- renderDT(server = FALSE, {
      p <- preview()
      shiny$req(p)
      utils$head(p$data, 10)
    }, options = list(dom = "t", ordering = FALSE), rownames = FALSE)

    output$footer <- shiny$renderUI({
      valid <- isTRUE(validation())
      shiny$tagList(
        shiny$actionButton(ns("cancel"), "Cancel", class = "btn-outline-secondary"),
        if (valid) shiny$downloadButton(ns("download"), "Download long CSV", class = "btn-outline-primary"),
        shiny$actionButton(ns("confirm"), "Use these data", class = "btn-primary", disabled = !valid)
      )
    })

    output$download <- shiny$downloadHandler(
      filename = function() paste0(sub("\\.[^.]+$", "", state$req$meta$name), "-long.csv"),
      content = function(file) {
        utils$write.csv(spec$apply_spec(current_spec(), state$req$dat)$data, file, row.names = FALSE)
      }
    )

    shiny$observeEvent(input$confirm, {
      req <- state$req
      shiny$req(req)
      cs <- current_spec()
      if (is.character(spec$validate_spec(cs, req$dat))) return()   # button was disabled
      out <- tryCatch(spec$apply_spec(cs, req$dat), error = function(e) e)
      if (inherits(out, "error")) {
        result(list(error = conditionMessage(out), token = req$token))
        return()
      }
      shiny$removeModal()
      state$req <- NULL
      result(list(data = out$data, spec = cs, losses = out$losses, token = req$token, meta = req$meta))
    })

    shiny$observeEvent(input$cancel, {
      req <- state$req
      shiny$req(req)
      shiny$removeModal()
      state$req <- NULL
      cancelled(list(token = req$token, reason = req$reason))
    })

    list(result = result, cancelled = cancelled)
  })
}
