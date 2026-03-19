box::use(
  bslib,
  rhino,
  sass,
  shiny
)

box::use(
  app / logic / logging_utils,
  app / logic / telemetry_utils,
  app / view / mixed_effects_demand_coordinator,
  app / view / demand,
  app / view / discounting,
  app / view / info,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  # Initialize logging system
  logging_utils$init_logging()
  rhino$log$info("Starting Shinybeez UI initialization")

  # Initialize telemetry system
  telemetry_utils$init_telemetry()

  # Log UI initialization
  logging_utils$log_structured(
    level = "info",
    message = "UI initialization started",
    category = "app_lifecycle"
  )

  # Get Google Analytics config from environment-aware config.yml
  ga_config <- config::get("google_analytics")
  ga_enabled <- isTRUE(ga_config$enabled)
  ga_id <- ga_config$measurement_id

  # Build GA script tag if enabled and ID is set
  ga_script_tag <- if (ga_enabled && !is.null(ga_id) && nzchar(ga_id)) {
    shiny$tagList(
      shiny$tags$script(
        async = NA,
        src = paste0("https://www.googletagmanager.com/gtag/js?id=", ga_id)
      ),
      shiny$tags$script(shiny$HTML(sprintf(
        "
        window.dataLayer = window.dataLayer || [];
        function gtag() { dataLayer.push(arguments); }
        gtag('js', new Date());
        gtag('config', '%s');
        function trackPage(pageName) {
          gtag('event', 'page_view', {
            page_title: pageName,
            page_location: window.location.href,
            event_category: 'Navigation'
          });
        }
      ",
        ga_id
      )))
    )
  } else {
    NULL
  }

  bslib$page_navbar(
    header = shiny$tags$head(
      ga_script_tag,
      # Invert plot images in dark mode so they blend with the dark background
      shiny$tags$style(shiny$HTML("
        [data-bs-theme='dark'] .esquisse-container img,
        [data-bs-theme='dark'] .shiny-plot-output img {
          filter: invert(0.88) hue-rotate(180deg);
        }
      ")),
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
    bslib$nav_item(
      bslib$input_dark_mode(id = "dark_mode", mode = "dark")
    ),
    info$ui(ns("info"))
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create session-specific logger and telemetry tracker
    session_logger <- logging_utils$create_session_logger(session)
    session_telemetry <- telemetry_utils$create_session_telemetry(session)

    # Initialize telemetry session tracking
    if (telemetry_utils$is_telemetry_enabled()) {
      tryCatch(
        {
          telemetry <- telemetry_utils$get_telemetry()
          if (!is.null(telemetry)) {
            telemetry$start_session(session = session, track_values = FALSE)
          }
        },
        error = function(e) {
          session_logger$warn("Failed to start telemetry session: {e$message}")
        }
      )
    }

    # Log session start
    session_logger$info("New user session started", "session_lifecycle")
    session_logger$user_activity("Session initialized", module = "main")

    # Track session start in telemetry
    session_telemetry$track_event(
      "session_start",
      list(
        url_search = session$clientData$url_search,
        url = session$clientData$url_hostname
      )
    )

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

    # Initialize modules with error handling and logging
    tryCatch(
      {
        session_logger$info("Initializing demand module", "module_init")
        demand$sidebar_server("demand")
        demand$navpanel_server("demand")
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

        mmd_sidebar_reactives <- mixed_effects_demand_coordinator$sidebar_server(
          "mixed_effects_demand",
          data_reactive = shiny$reactive(
            session$userData$data$mixed_effects_demand
          )
        )

        mixed_effects_demand_coordinator$navpanel_server(
          "mixed_effects_demand",
          sidebar_reactives = mmd_sidebar_reactives
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

      # Track session end in telemetry
      session_telemetry$track_event(
        "session_end",
        list(
          session_duration = difftime(
            Sys.time(),
            session$startTime,
            units = "secs"
          ),
          last_tab = .last_tab
        )
      )

      rhino$log$info("Session stopped: {session$token}")
    })
  })
}
