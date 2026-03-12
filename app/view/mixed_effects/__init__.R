#' Mixed Effects Demand View Module
#'
#' This module provides the UI and server functions for the Mixed Effects Demand tab.
#' It is decomposed into smaller components for maintainability.

# Re-export the main interface functions from the current monolithic module
# These will be replaced with component-based implementations

box::use(
  .. /
    mixed_effects_demand[
      sidebar_ui,
      sidebar_server,
      navpanel_ui,
      navpanel_server
    ]
)

#' @export
box::export(sidebar_ui, sidebar_server, navpanel_ui, navpanel_server)
