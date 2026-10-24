box::use(
  bslib,
  jsonlite,
  rhino,
  sass,
  shiny
)

box::use(
  app / logic / async / daemons,
  app / logic / async / task,
  app / logic / async / workers,
  app / logic / logging_utils,
  app / logic / startup_info,
  app / logic / telemetry_utils,
  app / logic / theme,
  app / view / consent,
  app / view / mixed_effects_demand_coordinator,
  app / view / demand,
  app / view / discounting,
  app / view / info,
)

# One daemon pool per process (SHINYBEEZ_DAEMONS; 0 = synchronous fits).
if (daemons$async_enabled()) {
  daemons$start_daemons()
  shiny$onStop(daemons$stop_daemons)
}

# Once per process, before any session: what this process is actually running as.
rhino$log$info(startup_info$startup_line())

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  # Initialize logging system
  logging_utils$init_logging()
  rhino$log$info("Starting Shinybeez UI initialization")

  # Initialize telemetry once per process. ui() runs on every page load, and
  # re-initialising here opened a new SQLite connection each time.
  telemetry_utils$get_telemetry()

  # Log UI initialization
  logging_utils$log_structured(
    level = "info",
    message = "UI initialization started",
    category = "app_lifecycle"
  )

  # Google Analytics is never emitted by R. The browser (app/js/index.js) loads the tag only
  # when the session's consent level is `usage`; this inline config gives it the measurement
  # id, the platform and the consent policy version for the handshake.
  ga_config <- config::get("google_analytics")
  ga_id <- if (isTRUE(ga_config$enabled) && is.character(ga_config$measurement_id)) {
    ga_config$measurement_id
  } else {
    ""
  }
  consent_config_tag <- shiny$tags$script(shiny$HTML(paste0(
    "window.SHINYBEEZ = ",
    jsonlite$toJSON(
      list(gaId = ga_id, platform = startup_info$platform(), policy = consent$CURRENT_POLICY),
      auto_unbox = TRUE
    ),
    ";"
  )))

  bslib$page_navbar(
    theme = theme$app_theme(),
    header = shiny$tags$head(
      consent_config_tag,
      # Add telemetry JavaScript if enabled
      if (telemetry_utils$is_telemetry_enabled()) {
        tryCatch(
          {
            shiny.telemetry::use_telemetry()
          },
          error = function(e) {
            # Gracefully handle if shiny.telemetry is not available
            rhino$log$warn("Could not initialize telemetry UI: {e$message}")
            NULL
          }
        )
      }
    ),
    footer = shiny$tags$footer(
      class = "app-footer",
      # The consent banner renders here, above the credit line, while no choice is stored.
      consent$banner_ui(ns("consent")),
      "Developed and Built by Brent Kaplan · ",
      shiny$tags$a(
        href = "https://codedbx.com",
        target = "_blank",
        rel = "noopener noreferrer",
        "codedbx.com"
      )
    ),
    title = shiny$tags$span(
      style = "font-size: 200%;",
      shiny$tags$img(
        src = "static/img/hex-shinybeez.png",
        width = "95px",
        height = "auto",
        class = "me-3",
        alt = "Shinybeez - Behavioral Economic Easy Demand and Discounting"
      ),
      "Shinybeez"
    ),
    id = "nav",
    sidebar = bslib$sidebar(
      id = "sidebar",
      open = TRUE,
      width = 350,
      shiny$conditionalPanel(
        "input.nav === 'Welcome'",
        shiny$includeHTML("app/static/html/welcome_sidebar.html")
      ),
      shiny$conditionalPanel(
        "input.nav === 'Demand'",
        demand$sidebar_ui(ns("demand")),
      ),
      shiny$conditionalPanel(
        "input.nav === 'Discounting'",
        discounting$sidebar_ui(ns("discounting"))
      ),
      shiny$conditionalPanel(
        "input.nav === 'MixedEffectsDemand'",
        mixed_effects_demand_coordinator$sidebar_ui(ns("mixed_effects_demand"))
      )
    ),
    bslib$nav_panel(
      "Welcome",
      shiny$includeHTML("app/static/html/welcome.html")
    ),
    bslib$nav_panel(
      value = "Demand",
      title = "Demand",
      demand$navpanel_ui(ns("demand"))
    ),
    bslib$nav_panel(
      value = "Discounting",
      title = "Discounting",
      discounting$navpanel_ui(ns("discounting"))
    ),
    bslib$nav_panel(
      value = "MixedEffectsDemand",
      title = "Mixed Effects Demand",
      mixed_effects_demand_coordinator$navpanel_ui(ns("mixed_effects_demand"))
    ),
    bslib$nav_spacer(),
    consent$nav_ui(ns("consent")),
    bslib$nav_item(
      bslib$input_dark_mode(id = "dark_mode", mode = "dark")
    ),
    info$ui(ns("info"))
  )
}

#' Handler for errors Shiny could not recover from
#'
#' Replaces shiny.telemetry's own `track_errors` handler, whose rows carried the raw
#' condition message under a key the analytics consumer never reads. This one goes through
#' `track_error()`, so the message is scrubbed and lands as a normal `error` row with
#' `context = "unhandled"`. It must never throw: it runs while the session is already dying.
#' @param session_telemetry The tracker from `telemetry_utils$create_session_telemetry()`.
#' @return A function of one condition, for `shiny::onUnhandledError()`.
#' @export
unhandled_error_handler <- function(session_telemetry) {
  function(e) {
    tryCatch(
      {
        message <- tryCatch(conditionMessage(e), error = function(err) NULL)
        if (is.null(message) || !nzchar(message)) {
          message <- paste("unhandled", class(e)[1])
        }
        session_telemetry$track_error(message, error_context = "unhandled")
      },
      error = function(err) NULL
    )
    invisible(NULL)
  }
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create session-specific logger and telemetry tracker
    session_logger <- logging_utils$create_session_logger(session)
    session_telemetry <- telemetry_utils$create_session_telemetry(session)
    session$userData$.start_time <- Sys.time()

    # shiny.telemetry's session (input/login/logout observers) and the session_start row are
    # started by the consent module once the browser has sent the session's consent level.
    shiny$onUnhandledError(unhandled_error_handler(session_telemetry), session = session)
    consent$server("consent", session_telemetry)

    # Log session start
    session_logger$info("New user session started", "session_lifecycle")
    session_logger$user_activity("Session initialized", module = "main")

    # Initialize reactive data storage
    session$userData$data <- shiny$reactiveValues()

    # Track last active tab for session_end telemetry (plain variable, not reactive)
    .last_tab <- "Welcome"

    # Track navigation changes (log initial tab and all subsequent changes)
    # Note: nav input is non-namespaced at root level, access via rootScope
    shiny$observe({
      # Access root session's input for non-namespaced navbar
      root_session <- session$rootScope()
      nav_value <- root_session$input$nav

      if (!is.null(nav_value)) {
        # Use isolate to prevent infinite loop, but trigger on nav change
        shiny$isolate({
          session_logger$user_activity(
            action = paste("Navigated to tab:", nav_value),
            input_id = "nav",
            input_value = nav_value,
            module = "main"
          )

          # Track navigation in telemetry
          session_telemetry$track_navigation(nav_value)

          .last_tab <<- nav_value
        })
      }
    })

    # Observers for client-side telemetry events relayed from JavaScript
    # JS sets inputs at root scope; use rootScope()$input since main server is namespaced
    root_input <- session$rootScope()$input

    shiny$observeEvent(root_input[["_telemetry_dt_export"]], {
      evt <- root_input[["_telemetry_dt_export"]]
      module <- sub("-.*", "", evt$table_id)
      session_telemetry$track_export(
        export_type = evt$export_type,
        module = module,
        file_format = evt$export_type
      )
    })

    shiny$observeEvent(root_input[["_telemetry_plot_download"]], {
      evt <- root_input[["_telemetry_plot_download"]]
      shiny$req(telemetry_utils$is_plot_download(evt$filename, evt$id))
      session_telemetry$track_export(
        export_type = "plot",
        module = telemetry_utils$plot_download_module(evt$filename, evt$id),
        file_format = telemetry_utils$plot_download_format(evt$filename, evt$id)
      )
    })

    shiny$observeEvent(root_input[["_telemetry_client_error"]], {
      evt <- root_input[["_telemetry_client_error"]]
      context <- paste0("client_js:", evt$source, ":", evt$line)
      session_telemetry$track_error(
        error_message = evt$message,
        error_context = context
      )
    })

    # Initialize modules with error handling and logging
    tryCatch(
      {
        session_logger$info("Initializing demand module", "module_init")
        demand_fit_task <- task$make_fit_task(workers$fit_demand_fixed_worker)
        demand$sidebar_server("demand", fit_task = demand_fit_task)
        demand$navpanel_server("demand", fit_task = demand_fit_task)
        session_logger$info(
          "Demand module initialized successfully",
          "module_init"
        )
      },
      error = function(e) {
        session_logger$error_enhanced(
          "Failed to initialize demand module",
          error_object = e,
          context = "module_initialization",
          user_action = "app_startup"
        )
      }
    )

    tryCatch(
      {
        session_logger$info("Initializing discounting module", "module_init")
        discounting$sidebar_server("discounting")
        discounting$navpanel_server("discounting")
        session_logger$info(
          "Discounting module initialized successfully",
          "module_init"
        )
      },
      error = function(e) {
        session_logger$error_enhanced(
          "Failed to initialize discounting module",
          error_object = e,
          context = "module_initialization",
          user_action = "app_startup"
        )
      }
    )

    # Mixed Effects Demand module with enhanced logging
    tryCatch(
      {
        session_logger$info(
          "Initializing mixed effects demand module",
          "module_init"
        )

        mmd_fit_task <- task$make_fit_task(workers$fit_mixed_worker)

        mmd_sidebar_reactives <- mixed_effects_demand_coordinator$sidebar_server(
          "mixed_effects_demand",
          data_reactive = shiny$reactive(
            session$userData$data$mixed_effects_demand
          ),
          fit_task = mmd_fit_task
        )

        mixed_effects_demand_coordinator$navpanel_server(
          "mixed_effects_demand",
          sidebar_reactives = mmd_sidebar_reactives,
          fit_task = mmd_fit_task
        )

        session_logger$info(
          "Mixed effects demand module initialized successfully",
          "module_init"
        )
      },
      error = function(e) {
        session_logger$error_enhanced(
          "Failed to initialize mixed effects demand module",
          error_object = e,
          context = "module_initialization",
          user_action = "app_startup"
        )
      }
    )

    # Initialize info module
    tryCatch(
      {
        session_logger$info("Initializing info module", "module_init")
        info$server("info")
        session_logger$info(
          "Info module initialized successfully",
          "module_init"
        )
      },
      error = function(e) {
        session_logger$error_enhanced(
          "Failed to initialize info module",
          error_object = e,
          context = "module_initialization",
          user_action = "app_startup"
        )
      }
    )

    # Log successful server initialization
    session_logger$info("All modules initialized successfully", "app_lifecycle")

    # Enhanced session end logging
    session$onSessionEnded(function() {
      session_logger$info("User session ended", "session_lifecycle")
      session_logger$user_activity("Session terminated", module = "main")

      # Track session end in telemetry, then forget the session's consent (later callbacks
      # that still write, such as shiny.telemetry's logout, are dropped as closed).
      session_telemetry$track_event(
        "session_end",
        list(
          session_duration = telemetry_utils$session_duration_secs(
            session$userData$.start_time
          ),
          last_tab = .last_tab,
          consent = session_telemetry$consent()
        )
      )
      telemetry_utils$end_session_consent(session)

      rhino$log$info("Session stopped: {session$token}")
    })
  })
}
