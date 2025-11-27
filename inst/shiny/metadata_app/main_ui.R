main_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    titlePanel("SeaTrack Metadata Utility"),
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          id = ns("sidebar_tabs"),
          tabPanel("Setup",
        folder_selector_ui(ns("folder")),
        hr(),
        actionButton(ns("get_locations"), "Refresh locations")
          )
      )),
      mainPanel(
        add_busy_spinner(spin = "fading-circle", position = "bottom-left"),
        use_busy_spinner(spin = "fading-circle", position = "bottom-left", spin_id = ns("main_spinner"), color = "red"),
        logger_ui(ns("logger"))
      )
    )
  )
}
