#' Wide-to-long column mapper modal
#'
#' Opened by file_input when an upload fails template validation. Owns the modal,
#' builds a reshape spec from the inputs, previews the long frame, and hands the
#' converted frame back through `result()`. Knows nothing about storage or telemetry.
#' There is no static UI: the modal is built server-side on each request.

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
x_col_label <- function(target) if (target == "discounting") "Delay column" else "Price column"
y_col_label <- function(target) {
  if (target == "discounting") "Indifference point column" else "Consumption column"
}

# Every modal input id carries the request token, so a later request can never
# read a value the previous modal posted. Before the client posts a value the
# input is NULL and the code falls back to the guess.
rid <- function(token, name) sprintf("r%d_%s", as.integer(token), name)

# JS condition for conditionalPanel: show the typed-prices box only in manual mode
manual_condition <- function(ns, token) sprintf("input['%s'] == 'manual'", ns(rid(token, "x_source")))

series_block <- function(ns, token, target, i, cols, series, show_manual_condition, show_label) {
  label_input <- shiny$textInput(
    ns(rid(token, paste0("series_label_", i))),
    "Series name (used as the group when there is more than one series)",
    value = series$label %||% ""
  )
  bslib$card(
    class = "mb-2",
    bslib$card_body(
      if (show_label) label_input else shiny$div(style = "display:none", label_input),
      shiny$selectizeInput(
        ns(rid(token, paste0("series_cols_", i))),
        cols_label(target),
        choices = cols,
        selected = series$cols,
        multiple = TRUE,
        options = list(plugins = list("remove_button"), placeholder = "Pick the response columns in order")
      ),
      shiny$conditionalPanel(
        condition = show_manual_condition,
        shiny$textAreaInput(
          ns(rid(token, paste0("series_x_", i))),
          paste(x_label(target), "in column order (comma, space or newline separated)"),
          value = if (!is.null(series$x)) paste(series$x, collapse = ", ") else "",
          rows = 2
        )
      )
    )
  )
}

wide_body <- function(ns, req, guess) {
  cols <- colnames(req$dat)
  target <- req$target
  token <- req$token
  other_cols <- setdiff(cols, c(guess$id_col, unlist(lapply(guess$series, `[[`, "cols"))))

  extras <- switch(
    target,
    demand = shiny$selectInput(
      ns(rid(token, "group_col")), "Group column (optional)",
      choices = c("None" = "", other_cols), selected = ""
    ),
    mixed_effects_demand = shiny$selectizeInput(
      ns(rid(token, "keep_cols")), "Columns to carry along as covariates or factors (optional)",
      choices = other_cols, selected = NULL, multiple = TRUE,
      options = list(plugins = list("remove_button"))
    ),
    discounting = NULL
  )

  shiny$tagList(
    shiny$p(
      class = "text-muted small",
      "Already one row per observation? Switch the layout above, or rename your columns to match ",
      "the long template on the Welcome tab. Column names are shown lowercased."
    ),
    shiny$selectInput(
      ns(rid(token, "id_col")), "Participant id column",
      choices = cols, selected = guess$id_col %||% cols[1]
    ),
    shiny$h6(cols_label(target)),
    shiny$uiOutput(ns("series_ui")),
    if (target != "discounting") {
      shiny$div(
        class = "mb-3",
        shiny$actionButton(ns(rid(token, "add_series")), "Add another series", class = "btn-outline-secondary btn-sm"),
        shiny$actionButton(
          ns(rid(token, "remove_series")), "Remove last series", class = "btn-outline-secondary btn-sm ms-2"
        )
      )
    },
    shiny$uiOutput(ns("x_source_ui")),
    shiny$uiOutput(ns("pairs_ui")),
    extras
  )
}

long_body <- function(ns, req, guess) {
  cols <- colnames(req$dat)
  target <- req$target
  token <- req$token
  other_cols <- setdiff(cols, c(guess$id_col, guess$x_col, guess$y_col))

  cands <- detect$partitioning_candidates(
    req$dat, guess$id_col, exclude = c(guess$x_col, guess$y_col)
  )
  group_select <- function(label) {
    shiny$tagList(
      shiny$selectInput(
        ns(rid(token, "long_group_col")), label,
        choices = spec$group_choices(other_cols, cands), selected = guess$group_col %||% ""
      ),
      shiny$uiOutput(ns("long_group_note"))
    )
  }

  extras <- switch(
    target,
    demand = group_select("Group column (optional)"),
    mixed_effects_demand = shiny$tagList(
      group_select("Series column (optional)"),
      shiny$selectizeInput(
        ns(rid(token, "long_keep_cols")), "Columns to carry along as covariates or factors (optional)",
        choices = other_cols, selected = NULL, multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    ),
    discounting = NULL
  )

  shiny$tagList(
    shiny$selectInput(
      ns(rid(token, "long_id_col")), "Participant id column",
      choices = cols, selected = guess$id_col %||% cols[1]
    ),
    shiny$selectInput(
      ns(rid(token, "long_x_col")), x_col_label(target),
      choices = cols, selected = guess$x_col %||% cols[1]
    ),
    shiny$selectInput(
      ns(rid(token, "long_y_col")), y_col_label(target),
      choices = cols, selected = guess$y_col %||% cols[1]
    ),
    extras
  )
}

# The body is rendered server-side so the layout radio can swap it; everything below the
# preview heading is the same for both layouts.
modal_ui <- function(ns, req, layout0) {
  dat <- req$dat
  reason <- sub("\\.\\s*$", "", req$reason)

  shiny$modalDialog(
    title = "Reshape your data",
    size = "xl",
    easyClose = FALSE,
    footer = shiny$uiOutput(ns("footer")),
    shiny$p(
      "This file doesn't match a shinybeez template: ", shiny$em(reason), ". Tell us how to reshape it. ",
      shiny$span(class = "text-muted", sprintf("%d rows × %d columns detected.", nrow(dat), ncol(dat)))
    ),
    shiny$radioButtons(
      ns(rid(req$token, "layout")), "How is this file laid out?",
      choices = c(
        "One row per observation (long)" = "long",
        "One row per participant (wide)" = "wide"
      ),
      selected = layout0, inline = TRUE
    ),
    shiny$uiOutput(ns("body")),
    shiny$h6("Preview"),
    shiny$uiOutput(ns("preview_status")),
    DTOutput(ns("preview"))
  )
}

#' @export
server <- function(id, request_r) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    state <- shiny$reactiveValues(
      req = NULL, guess = NULL, guess_long = NULL, layout0 = "wide",
      n_series = 0L, carry = FALSE, seen = list()
    )
    result <- shiny$reactiveVal(NULL)
    cancelled <- shiny$reactiveVal(NULL)

    # Reads a modal input by NAME for the CURRENT request only: an old request's input
    # (e.g. a previous modal's "confirm" click, or its typed prices) lives under a
    # DIFFERENT id (rid(old_token, name)) and is never visible here. NULL until the
    # client has posted a value for this request's id (or there is no current request).
    req_input <- function(name) {
      req <- state$req
      if (is.null(req)) return(NULL)
      input[[rid(req$token, name)]]
    }

    # A `selectizeInput(multiple = TRUE)` posts NULL when the user removes every
    # option, which is indistinguishable from an input the client has not posted yet.
    # `state$seen` records which series pickers have posted at least once for the
    # current request, so a NULL afterwards means "cleared", not "use the guess".
    series_cols <- function(token, i, guessed_cols) {
      id <- rid(token, paste0("series_cols_", i))
      posted <- input[[id]]
      if (!is.null(posted)) posted else if (isTRUE(state$seen[[id]])) character(0) else guessed_cols
    }

    shiny$observe({
      req <- state$req
      if (is.null(req)) return()
      for (i in seq_len(state$n_series)) {
        id <- rid(req$token, paste0("series_cols_", i))
        if (!is.null(input[[id]]) && !isTRUE(state$seen[[id]])) state$seen[[id]] <- TRUE
      }
    })

    shiny$observeEvent(request_r(), ignoreNULL = FALSE, {
      req <- request_r()
      if (is.null(req)) {
        if (!is.null(state$req)) shiny$removeModal()
        state$req <- NULL
        return()
      }
      guess <- detect$guess_spec_wide(req$dat, req$target)
      state$carry <- FALSE
      state$seen <- list()
      state$req <- req
      state$guess <- guess
      state$guess_long <- detect$guess_spec_long(req$dat, req$target)
      state$layout0 <- if (is.null(detect$detect_long(req$dat, req$target))) "wide" else "long"
      state$n_series <- length(guess$series)
      shiny$showModal(modal_ui(ns, req, state$layout0))
    })

    # The layout the modal is showing. The radio is NULL until the client posts it, so the
    # detected layout stands until the user changes it.
    layout <- shiny$reactive({
      shiny$req(state$req)
      req_input("layout") %||% state$layout0
    })

    output$body <- shiny$renderUI({
      req <- state$req
      shiny$req(req)
      if (layout() == "long") long_body(ns, req, state$guess_long) else wide_body(ns, req, state$guess)
    })

    # The body renders once per layout, but the participant id is a live input: a note saying
    # "commodity splits each participant" is false the moment the user picks a different id
    # column. Both the sections and the note follow the current selection.
    long_partition <- shiny$reactive({
      req <- state$req
      shiny$req(req)
      if (layout() != "long" || req$target == "discounting") return(NULL)
      g <- state$guess_long
      id_col <- req_input("long_id_col") %||% g$id_col
      x_col <- req_input("long_x_col") %||% g$x_col
      y_col <- req_input("long_y_col") %||% g$y_col
      list(
        id_col = id_col,
        cands = detect$partitioning_candidates(req$dat, id_col, exclude = c(x_col, y_col)),
        other_cols = setdiff(colnames(req$dat), c(id_col, x_col, y_col))
      )
    })

    shiny$observe({
      lp <- long_partition()
      shiny$req(lp)
      req <- state$req
      # Isolated: this observer WRITES the group input, and reading it reactively would make
      # that write re-trigger the observer. NULL means the client has not posted yet, so the
      # guess still stands.
      current <- shiny$isolate(req_input("long_group_col"))
      wanted <- if (is.null(current)) state$guess_long$group_col %||% "" else current
      shiny$updateSelectInput(
        session, rid(req$token, "long_group_col"),
        choices = spec$group_choices(lp$other_cols, lp$cands),
        selected = if (wanted %in% lp$other_cols) wanted else ""
      )
    })

    output$long_group_note <- shiny$renderUI({
      lp <- long_partition()
      if (is.null(lp) || length(lp$cands) == 0) return(NULL)
      req <- state$req
      col <- lp$cands[1]
      cells <- table(as.character(req$dat[[lp$id_col]]), as.character(req$dat[[col]]))
      chosen <- req_input("long_group_col") %||% state$guess_long$group_col %||% ""
      shiny$div(
        class = "form-text",
        spec$partition_note(
          col, ncol(cells), as.vector(cells)[1], req$target, identical(chosen, col)
        )
      )
    })

    # Single selects always post a value, so NULL means "not posted yet for this request"
    # and the guess stands; "" is the user choosing None.
    long_spec_from_inputs <- function(req) {
      g <- state$guess_long
      group_col <- req_input("long_group_col")
      group_col <- if (is.null(group_col)) g$group_col else if (nzchar(group_col)) group_col else NULL
      spec$new_spec(
        target = req$target,
        layout = "long",
        id_col = req_input("long_id_col") %||% g$id_col,
        x_col = req_input("long_x_col") %||% g$x_col,
        y_col = req_input("long_y_col") %||% g$y_col,
        group_col = group_col,
        keep_cols = req_input("long_keep_cols") %||% character(0)
      )
    }

    shiny$observeEvent(req_input("add_series"), {
      state$carry <- TRUE
      state$n_series <- state$n_series + 1L
    })
    shiny$observeEvent(req_input("remove_series"), {
      if (state$n_series > 1L) {
        state$carry <- TRUE
        state$n_series <- state$n_series - 1L
      }
    })

    # Series blocks are re-rendered when their number changes; carry the current
    # inputs across so adding a series does not reset the others to the guess. A fresh
    # reshape request (state$carry FALSE) always renders from its own guess; the previous
    # request's inputs are never read because they live under a different rid().
    output$series_ui <- shiny$renderUI({
      req <- state$req
      shiny$req(req)
      n <- state$n_series
      carry <- state$carry
      condition <- manual_condition(ns, req$token)
      shiny$isolate({
        shiny$tagList(lapply(seq_len(n), function(i) {
          guessed <- if (i <= length(state$guess$series)) state$guess$series[[i]] else spec$new_series(character(0))
          current <- if (carry) {
            x_text <- req_input(paste0("series_x_", i))
            spec$new_series(
              series_cols(req$token, i, guessed$cols),
              x = if (!is.null(x_text)) spec$parse_x_text(x_text) else guessed$x,
              label = req_input(paste0("series_label_", i)) %||% guessed$label
            )
          } else {
            guessed
          }
          series_block(ns, req$token, req$target, i, colnames(req$dat), current, condition, show_label = n > 1)
        }))
      })
    })

    current_spec <- shiny$reactive({
      req <- state$req
      shiny$req(req)
      if (layout() == "long") return(long_spec_from_inputs(req))
      guess <- state$guess
      x_source <- req_input("x_source") %||% guess$x_source
      series <- lapply(seq_len(state$n_series), function(i) {
        guessed <- if (i <= length(guess$series)) guess$series[[i]] else spec$new_series(character(0))
        cols <- series_cols(req$token, i, guessed$cols)
        x <- if (x_source == "header") {
          detect$x_from_names(cols)
        } else {
          x_text <- req_input(paste0("series_x_", i))
          if (!is.null(x_text)) spec$parse_x_text(x_text) else guessed$x %||% numeric(0)
        }
        label <- trimws(req_input(paste0("series_label_", i)) %||% guessed$label)
        spec$new_series(cols, x = x, label = label)
      })
      group_col <- req_input("group_col")
      if (is.null(group_col) || !nzchar(group_col)) group_col <- NULL
      spec$new_spec(
        target = req$target,
        id_col = req_input("id_col") %||% guess$id_col,
        series = series,
        group_col = group_col,
        keep_cols = req_input("keep_cols") %||% character(0),
        x_source = x_source
      )
    })

    # "Read from column names" is only offered when every series' selected headers
    # actually carry a price/delay (detect$series_header_x_available()); an item-index
    # run like apt_1..apt_5 suffix-parses to numbers too, but those are positions, not
    # prices, and each series is judged on its own.
    header_x_available <- shiny$reactive({
      cs <- current_spec()
      detect$series_header_x_available(lapply(cs$series, `[[`, "cols"))
    })

    output$x_source_ui <- shiny$renderUI({
      req <- state$req
      shiny$req(req)
      available <- header_x_available()
      choices <- c("Enter them" = "manual")
      if (available) choices <- c("Read from column names" = "header", choices)
      current <- shiny$isolate(req_input("x_source")) %||% state$guess$x_source
      if (!current %in% choices) current <- "manual"
      shiny$tagList(
        shiny$radioButtons(
          ns(rid(req$token, "x_source")), x_label(req$target), choices = choices, selected = current, inline = TRUE
        ),
        if (!available) {
          shiny$p(
            class = "text-muted small",
            "The column names do not contain ", x_noun(req$target), ", so enter them below."
          )
        }
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
      shiny$p(class = "text-muted small", spec$preview_summary(current_spec(), preview()))
    })

    output$preview <- renderDT(server = FALSE, {
      p <- preview()
      shiny$req(p)
      utils$head(p$data, 10)
    }, options = list(dom = "t", ordering = FALSE), rownames = FALSE)

    # The download button is stateless (it only ever writes preview() for whichever
    # request is current, and is only rendered while that preview is valid), so it is
    # the one modal control that keeps a static id rather than an rid()-namespaced one.
    output$footer <- shiny$renderUI({
      req <- state$req
      shiny$req(req)
      valid <- isTRUE(validation())
      shiny$tagList(
        shiny$actionButton(ns(rid(req$token, "cancel")), "Cancel", class = "btn-outline-secondary"),
        if (valid) shiny$downloadButton(ns("download"), "Download long CSV", class = "btn-outline-primary"),
        shiny$actionButton(
          ns(rid(req$token, "confirm")), "Use these data", class = "btn-primary", disabled = !valid
        )
      )
    })

    output$download <- shiny$downloadHandler(
      filename = function() paste0(sub("\\.[^.]+$", "", state$req$meta$name), "-long.csv"),
      content = function(file) {
        p <- preview()
        shiny$req(p)
        utils$write.csv(p$data, file, row.names = FALSE)
      }
    )

    shiny$observeEvent(req_input("confirm"), {
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

    shiny$observeEvent(req_input("cancel"), {
      req <- state$req
      shiny$req(req)
      shiny$removeModal()
      state$req <- NULL
      cancelled(list(token = req$token, reason = req$reason))
    })

    list(result = result, cancelled = cancelled)
  })
}
