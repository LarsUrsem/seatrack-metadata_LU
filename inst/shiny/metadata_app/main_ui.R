main_ui <- function(id) {
  ns <- NS(id)
  theme <- bs_theme(version = 5, preset = "bootstrap")
  theme <- bs_theme_update(theme,
    secondary = "#7499BA", font_scale = NULL,
    `enable-rounded` = FALSE, preset = "bootstrap"
  )
  page_fluid(
    shinyjs::useShinyjs(),
    layout_columns(

      titlePanel("SeaTrack Metadata Utility"),
      card(
        card_header("Seatrack folder:"),
        folder_selector_ui(ns("folder"))
      ),
      card(
        card_header("Loaded data:"),
        layout_columns(
          tagList(
            uiOutput(ns("location_label")),
            uiOutput(ns("data_saved_label"))
          ),
          fillable = FALSE,
          row_heights = "40%", min_height = "40%", max_height = "40%"
        ),
        layout_columns(actionButton(ns("save_all_btn"), "Save changes"), row_heights = "20%", min_height = "20%", max_height = "20%")
      ),
      card(
        card_header("Details:"),
        textInput(ns("user_full_name"), "Your name")
      ),
      col_widths = c(2, 4, 3, 3),
      min_height = "20%"
    ),
    navset_card_tab(
      id = ns("tabs"),
      nav_panel(
        "Manage loggers",
        manage_logger_ui(ns("manage_loggers"))
      ),
      nav_panel(
        "Manage metadata",
        manage_metadata_ui(ns("manage_metadata"))
      ),
      full_screen = TRUE
    ),
    add_busy_spinner(spin = "fading-circle", position = "bottom-left"),
    use_busy_spinner(spin = "fading-circle", position = "bottom-left", spin_id = ns("main_spinner"), color = "red"),
    card(logger_ui(ns("logger"))),
    theme = theme,
    tags$head(tags$style(
    HTML(
      "
        .bslib-full-screen-enter {
          bottom: var(--bslib-full-screen-enter-bottom);
        }
      "
    )
  ))
  )
}
