box::use(
  bsicons,
  bslib,
  shiny,
  tools,
  vroom,
)

box::use(
  app / logic / logging_utils,
  app / logic / telemetry_utils,
  app / logic / validate,
  app / view / wide_mapper,
)

#' Read an uploaded CSV or TSV file
#'
#' @param path File path to read
#' @param ext File extension ("csv" or "tsv")
#' @return Data frame, or NULL if reading fails
read_upload <- function(path, ext) {
  tryCatch(
    switch(
      ext,
      csv = vroom$vroom(path, delim = ",", show_col_types = FALSE),
      tsv = vroom$vroom(path, delim = "\t", show_col_types = FALSE)
    ),
    error = function(e) NULL
  )
}

#' Size of an uploaded file in MB, or NA when nothing has been uploaded
#'
#' At session start `input$upload` is NULL. `NULL$size` is NULL, and
#' `NULL / (1024 * 1024)` is `numeric(0)` - so the old inline
#' `if (file_size_mb > max_size_mb)` aborted with "argument is of length zero"
#' on every single session start (abc75312). Returning NA_real_ makes the
#' comparison well-defined; the `req()` in the observer stops it being reached
#' at all.
#'
#' @param upload The `input$upload` value (NULL before a file is chosen)
#' @return File size in MB, or NA_real_ if there is no upload
#' @export
upload_size_mb <- function(upload) {
  if (is.null(upload) || is.null(upload$size) || length(upload$size) != 1) {
    return(NA_real_)
  }
  upload$size / (1024 * 1024)
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    shiny$fileInput(
      inputId = ns("upload"),
      label = bslib$tooltip(
        trigger = list(
          "Upload your file (csv or tsv) ",
          bsicons$bs_icon("info-circle")
        ),
        "If you are unsure of the format, please use the downloadable
              templates on the welcome page."
      ),
      accept = c(".csv", ".tsv")
    )
  )
}

#' @export
server <- function(id, type = "demand") {
  shiny$moduleServer(id, function(input, output, session) {
    # Create session-specific logger
    session_logger <- logging_utils$create_session_logger(session)

    module_label <- switch(
      type,
      demand = "demand",
      discounting = "discounting",
      mixed_effects_demand = "mixed_effects"
    )
    read_label <- switch(
      type,
      demand = "demand",
      discounting = "discounting",
      mixed_effects_demand = "mixed effects demand"
    )

    # Wide-to-long mapper. One instance per file_input; opened by setting a
    # request, closed by setting NULL. Every upload bumps the token so a confirm
    # from an older modal can never store data over a newer upload.
    #
    # upload_token is exposed as a plain integer, not a reactiveVal, so it's
    # read as `upload_token` (no parens) from testServer. A bare local mutated
    # with `<<-` would not work for that: testServer evaluates the test body
    # against a *clone* of the module's environment taken right after
    # moduleServer() returns, before any upload runs, so a `<<-` counter would
    # read back frozen at its initial value forever. An active binding backed
    # by a small mutable environment is preserved as a live reference by that
    # clone, so reads from the test body see the current count.
    token_state <- new.env(parent = emptyenv())
    token_state$value <- 0L
    makeActiveBinding("upload_token", function() token_state$value, environment())
    mapper_request <- shiny$reactiveVal(NULL)
    mapper <- wide_mapper$server("mapper", mapper_request)

    notify_error <- function(msg) {
      shiny$showNotification(msg, type = "error", duration = NULL)
    }

    # Everything after a successful check_data(): NA rows, sufficiency (demand),
    # store, telemetry, notify. Shared by the direct path and the mapper path so
    # storage and telemetry happen exactly once per accepted upload.
    finish_standard <- function(tmp, meta, reshaped = FALSE) {
      na_result <- validate$remove_na_rows(tmp)
      tmp <- na_result$data
      if (type == "discounting") {
        session_logger$info(
          paste0(
            "Discounting format: standard (",
            nrow(tmp), " rows, ", ncol(tmp), " cols, ",
            na_result$n_dropped, " NA rows dropped)"
          ),
          category = "data_processing"
        )
      }
      if (nrow(tmp) == 0) {
        telemetry_utils$track_validation(
          module_label, "failure", "all_na_rows",
          "All rows contained missing values", session
        )
        notify_error("All rows contained missing values and were removed. Please check your data.")
        return(invisible(FALSE))
      }
      if (na_result$n_dropped > 0) {
        shiny$showNotification(
          paste(na_result$n_dropped, "row(s) with missing values were dropped from your data."),
          type = "warning",
          duration = 8
        )
      }
      if (type == "demand") {
        # Re-check on the frame we are about to STORE. Dropping rows above can
        # leave an id with too few price points to fit a curve.
        chk_enough <- validate$check_demand_sufficiency(tmp)
        if (is.character(chk_enough)) {
          telemetry_utils$track_validation(
            "demand", "failure", "insufficient_price_points", chk_enough, session
          )
          notify_error(chk_enough)
          return(invisible(FALSE))
        }
      }
      session$userData$data[[type]] <- tmp
      telemetry_utils$track_data_upload(
        file_info = list(
          size = meta$size, type = meta$ext,
          rows = nrow(tmp), cols = ncol(tmp), reshaped = reshaped
        ),
        session = session
      )
      telemetry_utils$track_validation(module_label, "success", session = session)
      shiny$showNotification(
        paste0("Data loaded: ", nrow(tmp), " rows, ", ncol(tmp), " columns."),
        type = "message",
        duration = 5
      )
      invisible(TRUE)
    }

    # MCQ and Qualtrics 5.5-Trial: NAs are structural (unanswered items,
    # non-administered trials). Preserve the frame as-is.
    finish_fixed_discounting <- function(tmp, meta) {
      is_mcq_wide <- "subjectid" %in% colnames(tmp) && ncol(tmp) == 28
      is_mcq_long <- identical(colnames(tmp), c("subjectid", "questionid", "response"))
      n_missing <- sum(is.na(tmp))
      format_label <- if (is_mcq_wide) "MCQ wide" else if (is_mcq_long) "MCQ long" else "Qualtrics 5.5-Trial"
      session_logger$info(
        paste0(
          "Discounting format: ", format_label,
          " (", nrow(tmp), " rows, ", ncol(tmp), " cols, ",
          n_missing, " NAs preserved)"
        ),
        category = "data_processing"
      )
      session$userData$data$discounting <- tmp
      telemetry_utils$track_data_upload(
        file_info = list(size = meta$size, type = meta$ext, rows = nrow(tmp), cols = ncol(tmp)),
        session = session
      )
      telemetry_utils$track_validation("discounting", "success", session = session)
      shiny$showNotification(
        paste0("Data loaded: ", nrow(tmp), " rows, ", ncol(tmp), " columns."),
        type = "message",
        duration = 5
      )
      invisible(TRUE)
    }

    # MCQ-27 (`subjectid`) is unambiguous on every tab. `responseid` marks a
    # Qualtrics 5.5-Trial file only on the discounting tab; on demand and
    # mixed-effects it is just the id column of a Qualtrics purchase-task export,
    # which is exactly the file the mapper exists for.
    is_fixed_schema <- function(tmp) {
      cols <- colnames(tmp)
      "subjectid" %in% cols || (type == "discounting" && "responseid" %in% cols)
    }

    rejection_message <- function(reason) {
      paste("Data are not in the correct format. Please refer to the documentation.", reason)
    }

    # A rejected frame opens the mapper unless it carries a fixed-schema marker.
    reject_or_map <- function(tmp, reason, meta) {
      telemetry_utils$track_validation(module_label, "failure", "check_data", reason, session)
      if (is_fixed_schema(tmp)) {
        notify_error(rejection_message(reason))
        return(invisible(FALSE))
      }
      mapper_request(list(dat = tmp, reason = reason, token = upload_token, target = type, meta = meta))
      telemetry_utils$track_reshape(type, "opened", session = session)
      invisible(TRUE)
    }

    shiny$observe({
      # This observer runs once at session start, before any file exists. Without
      # this req(), input$upload is NULL, input$upload$size / (1024 * 1024) is
      # numeric(0), and `if (numeric(0) > 20)` aborts with "argument is of length
      # zero" - abc75312, on every session start.
      shiny$req(input$upload)

      token_state$value <- token_state$value + 1L
      mapper_request(NULL)

      # Reject files larger than 20 MB
      max_size_mb <- 20
      file_size_mb <- upload_size_mb(input$upload)
      if (!is.na(file_size_mb) && file_size_mb > max_size_mb) {
        size_msg <- paste0(
          "File is too large (", round(file_size_mb, 1), " MB). ",
          "Maximum allowed size is ", max_size_mb, " MB."
        )
        telemetry_utils$track_validation(
          type, "failure", "file_size", size_msg, session
        )
        notify_error(size_msg)
        return()
      }

      # Snapshot the upload's metadata: the tails may run later, from the mapper's
      # confirm observer, when input$upload could already describe a newer file.
      meta <- list(
        name = input$upload$name,
        ext = tools$file_ext(input$upload$name),
        size = input$upload$size
      )
      session_logger$info(
        paste0("Reading ", read_label, " file: ", meta$name),
        category = "data_processing"
      )
      tmp <- read_upload(input$upload$datapath, meta$ext)
      if (is.null(tmp)) {
        telemetry_utils$track_validation(
          module_label, "failure", "file_read",
          "Unable to read file as CSV or TSV", session
        )
        notify_error("Unable to read the uploaded file. Please ensure it is a valid CSV or TSV file.")
        return()
      }
      # Normalize column names (lowercase, trim whitespace)
      colnames(tmp) <- tryCatch(trimws(tolower(colnames(tmp))), error = function(e) colnames(tmp))
      # Remove phantom columns (all-NA) before validation - but NOT for the
      # discounting formats whose all-NA columns are STRUCTURAL (5.5-Trial item
      # columns, MCQ's 28-column width).
      if (type != "discounting" || !validate$preserves_empty_cols(tmp)) {
        tmp <- validate$obliterate_empty_cols(tmp)
      }

      chk_data <- validate$check_data(tmp, type = type)
      if (is.character(chk_data)) {
        reject_or_map(tmp, chk_data, meta)
        return()
      }
      if (type == "discounting" && is_fixed_schema(tmp)) {
        finish_fixed_discounting(tmp, meta)
        return()
      }
      finish_standard(tmp, meta)
    }) |>
      shiny$bindEvent(input[["upload"]])

    # Mapper confirmed: drop stale results, re-validate the long frame through the
    # same check_data() a long upload passes, then run the shared tail.
    shiny$observeEvent(mapper$result(), {
      res <- mapper$result()
      if (!identical(res$token, upload_token)) {
        session_logger$info("Ignoring a reshape result from a superseded upload", category = "data_processing")
        return()
      }
      if (!is.null(res$error)) {
        telemetry_utils$track_error(
          paste("wide_mapper apply_spec failed:", res$error),
          error_context = "wide_mapper", session = session
        )
        notify_error(paste("Could not convert the data:", res$error))
        return()
      }
      chk <- validate$check_data(res$data, type = type)
      if (is.character(chk)) {
        telemetry_utils$track_error(
          paste("wide_mapper output failed check_data:", chk),
          error_context = "wide_mapper", session = session
        )
        notify_error(rejection_message(chk))
        return()
      }
      telemetry_utils$track_reshape(
        type, "confirmed",
        summary = list(
          n_series = length(res$spec$series),
          x_source = res$spec$x_source,
          n_cols_in = sum(vapply(res$spec$series, function(s) length(s$cols), numeric(1))),
          n_rows_out = nrow(res$data),
          n_dropped = res$losses$n_na_y
        ),
        session = session
      )
      finish_standard(res$data, res$meta, reshaped = TRUE)
    })

    shiny$observeEvent(mapper$cancelled(), {
      cancel <- mapper$cancelled()
      if (!identical(cancel$token, upload_token)) return()
      telemetry_utils$track_reshape(type, "cancelled", session = session)
      notify_error(rejection_message(cancel$reason))
    })
  })
}
