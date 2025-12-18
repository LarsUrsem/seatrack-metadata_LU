manage_partner_metadata_ui <- function(id) {
    ns <- NS(id)
    tagList(
        uiOutput(ns("message")),
        layout_columns(
            selectInput(
                ns("select_metadata_file"),
                NULL,
                NULL,
                selectize = TRUE
            ),
            col_widths = c(6, 3),
            fillable = FALSE,
            actionButton(ns("update_master_btn"), "Update master metadata"),
        ),
        br(),
        mod_dt_tabs_ui(ns("viewer"))
    )
}

manage_partner_metadata_server <- function(id, busy, all_locations, unsaved, current_location_idx, current_location_name) {
    moduleServer(id, function(input, output, session) {
        current_metadata_path <- reactiveVal(NA)
        current_metadata <- reactiveVal(NULL)
        metadata_tables <- reactiveVal(list())

        observeEvent(busy(), {
            if (busy()) {
                shinyjs::disable("update_master_btn")
                shinyjs::disable("select_metadata_file")
            } else {
                shinyjs::enable("update_master_btn")
                shinyjs::enable("select_metadata_file")
            }
        })

        observeEvent(current_location_name(), {
            if (!is.na(current_location_name())) {
                not_processed <- get_location_unprocessed(current_location_name())

                if (!is.null(not_processed) && length(not_processed) > 0) {
                    choices <- not_processed
                    names(choices) <- basename(not_processed)
                    shinyjs::show("select_metadata_file")
                    shinyjs::show("viewer")
                    shinyjs::show("update_master_btn")
                    updateSelectInput(inputId = "select_metadata_file", choices = choices)

                    current_metadata_path(input$select_metadata_file)

                    output$message <- renderUI({
                        p("Select unprocessed metadata file:")
                    })
                } else {
                    updateSelectInput(inputId = "select_metadata_file", choices = c(), selected = "")
                    shinyjs::hide("select_metadata_file")
                    shinyjs::hide("viewer")
                    shinyjs::hide("update_master_btn")

                    # Clearing choices does not change selection - so trigger these manually
                    current_metadata_path(NA)
                    current_metadata(NULL)
                    metadata_tables(list())

                    output$message <- renderUI({
                        p("No unprocessed metadata files available")
                    })
                }
            }
        })

        observeEvent(input$select_metadata_file, {
            if (input$select_metadata_file != "") {
                current_metadata_path(input$select_metadata_file)
            } else {
                current_metadata_path(NA)
            }
        })

        observeEvent(current_metadata_path(), {
            if (!is.na(current_metadata_path()) & current_metadata_path() != "") {
                partner_metadata <- tryCatch(
                    {
                        load_partner_metadata(current_metadata_path())
                    },
                    error = function(e) {
                        log_error(paste("ERROR", e))
                        return(NULL)
                    }
                )
                if (is.null(partner_metadata)) {
                    current_metadata(NULL)
                    return()
                }
                current_metadata(partner_metadata)
            } else {
                current_metadata(NULL)
            }
        })

        observeEvent(current_metadata(),
            {
                if (!is.null(current_metadata())) {
                    metadata_tables(current_metadata()$data)
                    shinyjs::enable("update_master_btn")
                } else {
                    metadata_tables(list())
                    shinyjs::disable("update_master_btn")
                }
            },
            ignoreNULL = FALSE
        )

        observeEvent(input$update_master_btn, {
            busy(TRUE)
            locations <- all_locations()

            partner_result <- tryCatch(
                {
                    handle_partner_metadata(
                        colony = current_location_name(),
                        new_metadata = current_metadata(),
                        master_import = all_locations()[[current_location_idx()]]
                    )
                },
                error = function(e) {
                    log_error(paste("ERROR: ", e))
                    return(NULL)
                }
            )
            if (is.null(partner_result)) {
                busy(FALSE)
                return()
            }

            new_locations <- modify_master_import_in_list(locations, partner_result$master_import)

            all_locations(new_locations)
            unsaved(TRUE)
            busy(FALSE)
        })

        mod_dt_tabs_server("viewer", metadata_tables)
    })
}
