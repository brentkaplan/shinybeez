#' Mixed Effects Demand Module - Coordinator
#'
#' This module coordinates the sidebar and navpanel submodules.
#' It re-exports their UI and server functions for use by main.R.

box::use(
  . /
    mixed_effects_demand_sidebar[
      sidebar_ui,
      sidebar_server
    ],
  . /
    mixed_effects_demand_navpanel[
      navpanel_ui,
      navpanel_server
    ]
)

#' @export
box::export(
  sidebar_ui,
  sidebar_server,
  navpanel_ui,
  navpanel_server
)
