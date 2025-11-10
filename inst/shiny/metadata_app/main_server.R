main_server <- function(id) {
    ns <- NS(id)
    moduleServer(id, function(input, output, session) {
        # Logger module
        logger_server <- logger_server("logger")


        # Folder selector
        sea_track_path <- folder_selector_server("folder")

        # Refresh locations

        busy <- reactiveVal(FALSE)
        all_locations <- reactiveVal(list())

        observeEvent(sea_track_path(), {
            new_path <- sea_track_path()
            get_all_locations(new_path, all_locations, busy)

            print("Checked path")
        })

        observeEvent(busy(), {
            print(busy)
            if (busy()) {
                show_spinner(spin_id = ns("main_spinner"), session = session)
            } else {
                hide_spinner(spin_id = ns("main_spinner"), session = session)
            }
        })

        observeEvent(all_locations(), {
            n_loc <- length(all_locations())
            print(n_loc)
        })
    })
}

get_all_locations <- function(new_path = NULL, success_callback = function() {}, busy_callback = function() {}) {
    if (!is.null(new_path)) {
        locations_path <- file.path(new_path, "Locations")
        shiny_dir <- getwd()
        if (dir.exists(locations_path)) {
            log_info("Loading locations")
            busy_callback(TRUE)
            load_promise <- future_promise({
                start_logging(log_dir = shiny_dir)
                all_files <- load_all_master_import(
                    combine = FALSE, skip = c("Blomstrand", "Keysite Vestland", "Lowestoft", "Iceland_processed_metadata", "not_processed")
                )
                return(all_files)
            })
            then(load_promise, onFulfilled = success_callback)
            finally(load_promise, function() {
                log_success("Finished loading locations")
                busy_callback(FALSE)
            })
        } else {
            log_error("No location folder in seatrack folder")
        }
    }
}
