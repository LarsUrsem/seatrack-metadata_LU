main_server <- function(id) {
    ns <- NS(id)
    moduleServer(id, function(input, output, session) {
        busy <- reactiveVal(FALSE)
        unsaved <- reactiveVal(FALSE)
        user_full_name <- reactiveVal("")
        all_locations <- reactiveVal(list())
        # Logger module
        logger_server <- logger_server("logger")

        sea_track_path <- reactiveVal()
        # Folder selector
        folder_select <- folder_selector_server("folder", busy, all_locations)
        observeEvent(folder_select, {
            sea_track_path(folder_select$folder)
        })
        manager_loggers <- manage_logger_server("manage_loggers", busy, all_locations, unsaved)
        manage_metadata <- manage_metadata_server("manage_metadata", busy, all_locations, unsaved)
        # Export nonresponsive
        # Mastersheet viewer
        # Import partner metadata
        # Upload to database

        observeEvent(busy(), {
            if (busy()) {
                show_spinner(spin_id = ns("main_spinner"), session = session)
            } else {
                hide_spinner(spin_id = ns("main_spinner"), session = session)
            }
        })

        observeEvent(all_locations(), {
            n_loc <- length(all_locations())
            output$location_label <- renderUI(p(
                paste(n_loc, "locations loaded")
            ))
        })

        observeEvent(unsaved(), {
            if (unsaved()) {
                output$data_saved_label <- renderUI(p(
                    "Unsaved changes!"
                ))
                shinyjs::enable("save_all_btn")
            } else {
                output$data_saved_label <- renderUI(p(
                    "No unsaved changes"
                ))
                shinyjs::disable("save_all_btn")
            }
        })

        observeEvent(input$save_all_btn, {
            busy(TRUE)
            locations <- all_locations()
            unique_sheets_only <- list()
            seen_paths <- c()
            for (i in seq_along(locations)) {
                x <- locations[[i]]
                if (x$modified && !x$path %in% seen_paths) {
                    unique_sheets_only <- c(unique_sheets_only, x)
                    seen_paths <- c(seen_paths, x$path)
                }
            }
            for (i in seq_along(unique_sheets_only)){
                x <- unique_sheets_only[[i]]
                new_x <- save_master_sheet(x, modified_only = TRUE)
                unique_sheets_only[[i]] <- new_x
                log_info(paste0("Saved master import sheet: ", basename(new_x$path)))
                locations <- modify_master_import_in_list(locations, new_x)
            }
            all_locations(locations)
            unsaved(FALSE)
            busy(FALSE)
        })
    })
}
