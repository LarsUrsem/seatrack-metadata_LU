manage_metadata_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h5("Manage metadata:"),
        layout_columns(
            selectInput(
                ns("select_location"),
                "Select location:",
                NULL,
                selectize = TRUE
            ),
            col_widths = c(4)
        ),
        br(),
        navset_card_pill(
            id = ns("tabs"),
            nav_panel(
                title = "Master metadata",
                mod_dt_tabs_ui(ns("viewer"))
            ),
            nav_panel(
                title = "Partner metadata",
                manage_partner_metadata_ui(ns("partner"))
            ),
        )
    )
}

manage_metadata_server <- function(id, busy, all_locations, unsaved) {
    moduleServer(id, function(input, output, session) {
        current_location_idx <- reactiveVal(NA)
        current_location_name <- reactiveVal(NA)
        metadata_tables <- reactiveVal(list())

        observeEvent(busy(), {
            if (busy()) {
                shinyjs::disable("select_location")
            } else {
                shinyjs::enable("select_location")
            }
        })

        observeEvent(all_locations(), {
            choices <- seq_along(names(all_locations()))
            names(choices) <- names(all_locations())

            updateSelectInput(inputId = "select_location", choices = choices)
        })

        observeEvent(input$select_location, {
            if (input$select_location != "") {
                current_location_idx(as.numeric(input$select_location))
                current_location_name(names(all_locations())[current_location_idx()])
            }
        })

        observeEvent(current_location_name(), {
            if (!is.na(current_location_name())) {
                metadata_tables(all_locations()[[current_location_idx()]]$data)
            } else {
                metadata_tables(list())
            }
        })

        manage_partner_metadata <- manage_partner_metadata_server(
            "partner", busy,
            all_locations, unsaved, current_location_idx, current_location_name
        )

        mod_dt_tabs_server("viewer", metadata_tables)
    })
}
