#' Consent banner, settings modal and shield nav item
#'
#' The one place a session's consent level is decided. The browser sends a handshake on
#' connect (`consent_level`: the stored choice, or the platform default when there is none,
#' see app/js/index.js); this module applies it through the session tracker, shows the banner
#' when the level is a default rather than a choice, applies the banner buttons and the
#' settings modal, and tells the browser which level to persist (`shinybeez-consent`).
#'
#' Design: docs/superpowers/specs/2026-10-24-analytics-consent-tiers-design.md §6 and §9.

box::use(
  bslib,
  shiny,
)

box::use(
  app / logic / startup_info,
  app / logic / telemetry_schema,
)

# Bump to re-show the banner to everyone (a stored choice with an older policy counts as no
# choice). Mirrored in app/js/index.js through window.SHINYBEEZ.policy, which main.R emits.
# Bump rule: any change to what a level sends, or to the copy that describes it.
#' @export
CURRENT_POLICY <- 2L # nolint: object_name_linter.

banner_copy <- function(platform) {
  base <- paste(
    "shinybeez is free and maintained by one researcher.",
    "Anonymous counts of which tools get used decide what I build next and what needs fixing.",
    "Your data, column names and file names are never sent.",
    "Change this any time from the shield icon."
  )
  if (identical(platform, "desktop")) {
    paste(base, "Your data and results never leave your computer.")
  } else {
    base
  }
}

# Modal order: the recommended level first, "Nothing" last. The browser JS sets the gtag
# config flags that make "advertising features off" true (app/js/index.js, loadGA()).
level_descriptions <- list(
  usage = paste(
    "Usage and diagnostics (recommended): which tools and settings are used, plus diagnostics.",
    "Page views also go to Google Analytics, with advertising features off."
  ),
  diagnostic = paste(
    "Diagnostics only: errors, failed uploads and timings, as counts.",
    "No Google Analytics, no cookies."
  ),
  none = "Nothing: sends nothing except one anonymous record of this choice."
)

#' Shield icon in the navbar that opens the settings modal
#' @export
nav_ui <- function(id) {
  ns <- shiny$NS(id)
  bslib$nav_item(
    shiny$actionLink(
      ns("open"),
      shiny$tags$span(class = "visually-hidden", "Privacy settings"),
      icon = shiny$icon("shield-halved"),
      `aria-label` = "Privacy settings",
      title = "Privacy settings"
    )
  )
}

#' Slot for the banner; rendered only while there is no stored choice
#' @export
banner_ui <- function(id) {
  shiny$uiOutput(shiny$NS(id)("banner"))
}

choice_button <- function(input_id, label, level, class) {
  shiny$actionButton(input_id, label, class = paste("btn-sm", class), `data-consent-level` = level)
}

build_banner <- function(ns, default_level, platform) {
  accept_label <- if (identical(default_level, "diagnostic")) "Allow usage data" else "OK"
  shiny$div(
    class = "consent-banner",
    role = "region",
    `aria-label` = "Privacy choice",
    shiny$p(class = "consent-copy mb-0", banner_copy(platform)),
    shiny$div(
      class = "consent-actions",
      choice_button(ns("accept"), accept_label, "usage", "btn-primary"),
      choice_button(ns("diagnostic"), "Diagnostics only", "diagnostic", "btn-outline-secondary"),
      choice_button(ns("nothing"), "Nothing", "none", "btn-outline-secondary"),
      shiny$actionLink(ns("details"), "Details", class = "consent-details")
    )
  )
}

settings_modal <- function(ns, current) {
  if (!current %in% telemetry_schema$LEVELS) {
    current <- "none"
  }
  shiny$modalDialog(
    title = "Privacy and analytics",
    shiny$p(
      "shinybeez is free and maintained by one researcher.",
      "Anonymous counts of which tools get used decide what I build next and what needs fixing.",
      "Choose what the app may send:"
    ),
    shiny$radioButtons(
      ns("level"),
      label = NULL,
      choiceNames = unname(level_descriptions),
      choiceValues = names(level_descriptions),
      selected = current
    ),
    shiny$p(
      class = "small text-muted mb-0",
      paste0("Policy version ", CURRENT_POLICY, ". "),
      shiny$tags$a(
        href = "#privacy-analytics", `data-bs-dismiss` = "modal",
        "Privacy and analytics on the Welcome page"
      )
    ),
    footer = shiny$tagList(
      shiny$modalButton("Cancel"),
      # data-consent-radio lets the browser persist the chosen level at click time, before
      # the server round trip (app/js/index.js), the same way the banner buttons do.
      shiny$actionButton(
        ns("save"), "Save", class = "btn-primary", `data-consent-radio` = ns("level")
      )
    ),
    easyClose = TRUE
  )
}

#' Module server
#'
#' @param session_telemetry Tracker from `telemetry_utils$create_session_telemetry()`; its
#'   `set_consent(level, source)` and `consent()` are the only methods used.
#' @param client Browser messenger: a list with `notify(level, policy)`. The default sends
#'   the `shinybeez-consent` custom message; tests inject a recorder because the mock session
#'   has no `sendCustomMessage()`.
#' @param show_modal,remove_modal Injected for the same reason (the mock has no `sendModal()`).
#' @param platform `web` or `desktop`; decides the banner copy.
#' @export
server <- function(
  id, session_telemetry, client = NULL,
  show_modal = shiny$showModal, remove_modal = shiny$removeModal,
  platform = startup_info$platform()
) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    if (is.null(client)) {
      client <- list(
        notify = function(level, policy) {
          session$sendCustomMessage("shinybeez-consent", list(level = level, policy = policy))
        }
      )
    }
    root_input <- session$rootScope()$input

    show_banner <- shiny$reactiveVal(FALSE)
    default_level <- shiny$reactiveVal("usage")
    # Milliseconds since the epoch of the last choice made in this session. A `stored`
    # handshake older than this (a reconnect re-reading localStorage before the browser
    # persisted the new choice) is ignored.
    applied_ts <- shiny$reactiveVal(0)

    shiny$observeEvent(root_input$consent_level, {
      msg <- root_input$consent_level
      level <- msg$level
      source <- msg$source
      ts <- suppressWarnings(as.numeric(msg$ts))
      if (is.null(level) || !level %in% telemetry_schema$LEVELS || is.null(source)) {
        return()
      }
      if (identical(source, "stored") && (length(ts) != 1L || is.na(ts) || ts < applied_ts())) {
        return()
      }
      # A choice stored under an older policy (a page left open across a deployment that
      # bumped CURRENT_POLICY) is no longer a choice: apply it as a default and re-ask.
      policy <- suppressWarnings(as.integer(msg$policy))
      if (identical(source, "stored") && (length(policy) != 1L || is.na(policy) || policy < CURRENT_POLICY)) {
        source <- "default"
      }
      session_telemetry$set_consent(level, source)
      if (source %in% c("default", "default_eu")) {
        default_level(level)
        show_banner(TRUE)
      } else {
        show_banner(FALSE)
      }
    })

    banner_tag <- shiny$reactive({
      if (!show_banner()) {
        return(NULL)
      }
      build_banner(ns, default_level(), platform)
    })
    output$banner <- shiny$renderUI(banner_tag())

    apply_choice <- function(level, source) {
      applied_ts(as.numeric(Sys.time()) * 1000)
      session_telemetry$set_consent(level, source)
      show_banner(FALSE)
      client$notify(level, CURRENT_POLICY)
    }

    shiny$observeEvent(input$accept, apply_choice("usage", "banner"))
    shiny$observeEvent(input$diagnostic, apply_choice("diagnostic", "banner"))
    shiny$observeEvent(input$nothing, apply_choice("none", "banner"))

    open_settings <- function() {
      show_modal(settings_modal(ns, session_telemetry$consent()))
    }
    shiny$observeEvent(input$open, open_settings())
    shiny$observeEvent(input$details, open_settings())
    shiny$observeEvent(root_input$consent_open, open_settings())

    shiny$observeEvent(input$save, {
      level <- input$level
      if (!is.null(level) && level %in% telemetry_schema$LEVELS) {
        apply_choice(level, "settings")
      }
      remove_modal()
    })
  })
}
