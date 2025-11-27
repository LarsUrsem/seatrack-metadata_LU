main_server <- function(id) {
    ns <- NS(id)
    moduleServer(id, function(input, output, session) {
        busy <- reactiveVal(FALSE)
        all_locations <- reactiveVal(list())
        # Logger module
        logger_server <- logger_server("logger")

        # Folder selector
        sea_track_path <- folder_selector_server("folder")

        # Refresh locations


        load_promise <- reactiveVal(NULL)

        observeEvent({list(sea_track_path(), input$get_locations)}, {
            new_path <- sea_track_path()
            if (!is.null(new_path)) {
                locations_path <- file.path(new_path, "Locations")
                shiny_dir <- getwd()
                if (dir.exists(locations_path)) {
                    log_info("Loading locations")
                    busy(TRUE)

                    future <- future_promise({
                        # start_logging(shiny_dir, "app.log")
                        all_files <- load_all_master_import(
                            combine = FALSE, skip = c("Blomstrand", "Keysite Vestland", "Lowestoft", "Iceland_processed_metadata", "not_processed", distinct = FALSE)
                        )
                        return(all_files)
                    }) |> then(\(x) all_locations(x)) |> finally(\() {
                        log_success("Finished loading locations")
                        busy(FALSE)
                    })
                    load_promise(future)
                } else {
                    log_error("No location folder in seatrack folder")
                }
            }
        })

        observeEvent(busy(), {
            if (busy()) {
                show_spinner(spin_id = ns("main_spinner"), session = session)
                block("folder", session = session)
                block(id = "get_locations", session = session)
            } else {
                hide_spinner(spin_id = ns("main_spinner"), session = session)
                unblock("folder", session = session)
                unblock(id = "get_locations", session = session)
            }
        })

        observeEvent(all_locations(), {
            n_loc <- length(all_locations())
            print(n_loc)
        })
    })
}


