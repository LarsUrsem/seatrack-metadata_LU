folder_selector_ui <- function(id) {
    ns <- NS(id)
    tagList(
        layout_columns(
            verbatimTextOutput(ns("text")),
            fillable = TRUE,
            row_heights = "40%",
            min_height = "40%", max_height = "40%"
        ),
        layout_columns(
            shinyDirButton(ns("select_folder_btn"), "Select Folder", "Choose folder"),
            actionButton(ns("get_locations"), "Refresh"),
            fill = TRUE,
            fillable = TRUE,
            col_widths = c(6, 6),
            row_heights = "20%", min_height = "20%", max_height = "20%"
        )
    )
}

folder_selector_server <- function(id, busy, all_locations) {
    moduleServer(id, function(input, output, session) {
        volumes <- c(Home = fs::path_home(), getVolumes()())
        shinyDirChoose(input, "select_folder_btn",
            roots = volumes, session = session,
            allowDirCreate = FALSE
        )
        folder <- reactiveVal(NULL)

        seatrack_dir <- Sys.getenv("SEA_TRACK_FOLDER", NA)
        if (is.na(seatrack_dir)) {
            seatrack_dir <- NULL
        } else {
            folder(seatrack_dir)
        }

        observeEvent(busy(), {
            if (busy()) {
                shinyjs::disable("select_folder_btn")
                shinyjs::disable("get_locations")
            } else {
                shinyjs::enable("select_folder_btn")
                shinyjs::enable("get_locations")
            }
        })

        folder_check <- reactive({
            if (!is.null(folder()) || length(folder()) > 0) {
                shinyjs::enable("get_locations")
                return(folder())
            } else {
                shinyjs::disable("get_locations")
                return("No value set")
            }
        })

        observeEvent(input$select_folder_btn, {
            path <- shinyFiles::parseDirPath(volumes, input$select_folder_btn)
            # log$add(paste("Selected folder:", p))
            if (length(path) > 0) {
                folder(path)
            } else {
                folder(NULL)
            }
        })

        get_locations <- function(clear = FALSE) {
            new_path <- folder()
            set_sea_track_folder(new_path)

            all_locations(list())
            if (!is.null(new_path)) {
                locations_path <- file.path(new_path, "Locations")

                shiny_dir <- getwd()
                if (dir.exists(locations_path)) {
                    log_info("Loading locations")
                    busy(TRUE)
                    future <- future_promise({
                        log_path <- getShinyOption("logging_path", "app.log")
                        log_appender(appender_file_safe(log_path))
                        log_threshold(INFO)
                        set_sea_track_folder(new_path, save_path = FALSE)

                        # start_logging(shiny_dir, "app.log", silent = TRUE)
                        all_files <- load_all_master_import(
                            combine = FALSE, skip = c("Blomstrand", "Keysite Vestland", "Lowestoft", "Iceland_processed_metadata", "not_processed", distinct = FALSE, use_stored = !clear)
                        )
                        return(all_files)
                    }) |>
                        then(\(x) all_locations(x)) |>
                        finally(\() {
                            all_locations(all_locations()[order(names(all_locations()))])
                            log_success("Finished loading locations")
                            busy(FALSE)
                        })
                    load_promise(future)
                } else {
                    log_error("No location folder in seatrack folder")
                }
            }
        }

        load_promise <- reactiveVal(NULL)
        observeEvent(
            folder(),
            {
                get_locations()
            }
        )

        observeEvent(
            input$get_locations,
            {
                get_locations(TRUE)
            }
        )

        output$text <- renderText({
            folder_check()
        })
        return(list(folder = reactive(folder())))
    })
}
