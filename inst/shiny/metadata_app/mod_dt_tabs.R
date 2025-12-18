mod_dt_tabs_ui <- function(id) {
    ns <- NS(id)

    navset_tab(
        id = ns("tabs")
    )
}


mod_dt_tabs_server <- function(id, tables) {
    moduleServer(id, function(input, output, session) {
        current_ids <- reactiveVal(c())


        observeEvent(tables(), {
            for (existing_id in current_ids()) {
                nav_remove("tabs", target = existing_id)
            }
            current_ids <- c()
            if (length(tables) == 0) {
                return()
            }

            for (i in seq_along(tables())) {
                nm <- names(tables())[i]
                current_ids(c(current_ids(), nm))
                select_tab <- FALSE
                if (i == 1) {
                    select_tab <- TRUE
                }

                local({
                    nm <- nm
                    nav_insert(
                        "tabs",
                        select = select_tab,
                        nav_panel(
                            title = nm,
                            id = nm,
                            br(),
                            DT::renderDT(
                                DT::datatable(
                                    tables()[[nm]],
                                    rownames = FALSE,
                                    style = "auto",
                                    options = list(
                                        autoWidth = TRUE,
                                        pageLength = 10,
                                        columnDefs = list(list(
                                            targets = "_all",
                                            render = DT::JS(
                                                "function(data, type, row, meta) {",
                                                "return type === 'display' && data != null && data.length > 30 ?",
                                                "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                                "}"
                                            )
                                        ))
                                    )
                                )
                            )
                        )
                    )
                })
            }
        })
    })
}
