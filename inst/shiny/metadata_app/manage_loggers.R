# manage_logger_ui <- function(id) {
#     ns <- NS(id)
#     tagList(
#         h5("Search Logger ID:"),
#         layout_columns(
#             div(
#                 textInput(ns("logger_search"), label = NULL, placeholder = ""),
#                 tags$style(HTML("
#                 .shiny-input-container{
#                 width:100% !important;
#                 height: 100% !important;}
#                 #main-manage_loggers-logger_search{
#                     height:100%;
#                 }")),
#                 style = "height:100%;align-content: center;"
#             ),
#             div(
#                 style = "height:100%;align-content: center;", actionButton(ns("submit_search_btn"), "Search")
#             ),
#             col_widths = c(4, 1),
#             fillable = FALSE,
#             fill = TRUE,
#             max_height = "2rem",
#             row_height = "2rem"
#         ),
#         br(),
#         uiOutput(ns("edit_session_buttons")),
#         uiOutput(ns("search_result")),
#     )
# }

manage_logger_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h5("Search Logger ID:"),
        layout_columns(
            textInput(ns("logger_search"), label = NULL, placeholder = ""),
            actionButton(ns("submit_search_btn"), "Search"),
            col_widths = c(4, 1),
            fillable = FALSE,
            fill = TRUE,
            max_height = "2rem",
            row_height = "2rem"
        ),
        uiOutput(ns("edit_session_buttons")),
        uiOutput(ns("search_result")),
    )
}

manage_logger_server <- function(id, busy, all_locations, unsaved) {
    moduleServer(id, function(input, output, session) {
        search_results <- reactiveVal(list())
        ns <- NS(id)
        btn_obs_list <- list()

        observeEvent(
            {
                list(busy(), all_locations())
            },
            {
                if (busy() || length(all_locations()) == 0) {
                    shinyjs::disable("submit_search_btn")
                } else {
                    shinyjs::enable("submit_search_btn")
                }
            }
        )

        observeEvent(
            {
                input$submit_search_btn
            },
            {
                do_search()
            }
        )


        observeEvent(
            {
                search_results()
            },
            {
                if (length(search_results()) == 0) {
                    shinyjs::hideElement("edit_session_buttons")
                    output$search_result <- renderUI({
                        h4("No results found")
                    })
                    return()
                }

                all_result <- search_results()

                open_result <- all_result[sapply(all_result, function(x) x$open)]
                closed_result <- all_result[!sapply(all_result, function(x) x$open)]

                if (length(open_result) > 1) {
                    open_session_warning <- strong("MULTIPLE OPEN SESSIONS FOUND")
                } else {
                    open_session_warning <- c()
                }
                if (length(open_result) == 1) {
                    shinyjs::showElement("edit_session_buttons")
                } else {
                    shinyjs::hideElement("edit_session_buttons")
                }


                output$search_result <- renderUI({
                    tagList(
                        h4("Search results:"),
                        open_session_warning,
                        display_sessions(open_result, "Open sessions"),
                        display_sessions(closed_result, "Closed sessions")
                    )
                })
            }
        )

        do_search <- function() {
            if (nchar(input$logger_search) == 0) {
                log_warn("No input")
                return()
            }
            busy(TRUE)
            log_info(paste("Searching for logger", input$logger_search))
            logger_search_result <- get_logger_from_metadata(input$logger_search, all_locations())
            print(logger_search_result)

            if (length(logger_search_result) > 0) {
                log_info(paste("Logger", input$logger_search, "found"))
                logger_search_result <- lapply(logger_search_result, function(x) {
                    x$open <- is.na(x$data$download_date) & is.na(x$data$shutdown_date)
                    return(x)
                })
                print(logger_search_result)

                search_results(logger_search_result)
            } else {
                log_info(paste("Logger", input$logger_search, "not found in master startups"))
                search_results(list())
            }
            busy(FALSE)
        }

        buttons_to_generate <- list(
            list(btn_name = "set_unused_btn", btn_label = "Unused", btn_type = "Not used"),
            list(btn_name = "set_nonresponsive_btn", btn_label = "Nonresponsive", btn_type = "Nonresponsive"),
            list(btn_name = "set_downloaded_btn", btn_label = "Downloaded", btn_type = "Succesfully downloaded")
        )

        output$edit_session_buttons <-
            renderUI(tagList(
                strong("Click a button to close the logger's open session"),
                do.call(
                    layout_columns,
                    c(
                        lapply(buttons_to_generate, function(x) {
                            if (is.null(btn_obs_list[[x$btn_name]])) {
                                btn_obs_list[[x$btn_name]] <<- observeEvent(input[[x$btn_name]], {
                                    all_result <- search_results()
                                    locations <- all_locations()
                                    open_result <- all_result[sapply(all_result, function(x) x$open)][[1]]
                                    end_session_result <- end_logger_session(open_result$data$logger_serial_no, x$btn_type, comment = input$logger_close_comment, master_sheet = locations[[open_result$list_index]])

                                    new_locations <- modify_master_import_in_list(locations, end_session_result$master_sheet)

                                    all_locations(new_locations)
                                    unsaved(TRUE)
                                    do_search()
                                })
                            }
                            actionButton(paste("main", id, x$btn_name, sep = "-"), x$btn_label)
                        }),
                        list(textInput(paste("main", id, "logger_close_comment", sep = "-"), NULL, "", placeholder = "Add comment.."),
                            col_widths = breakpoints(xs = c(2, 2, 2, 5)),
                            fill = TRUE
                        )
                    ),
                )
            ))
    })
}



display_sessions <- function(sessions, title = "") {
    selected_cols <- c("logger_serial_no", "logger_model", "production_year", "download_type", "download_date", "shutdown_date", "comment")
    if (length(sessions) == 0) {
        return(list())
    } else {
        session_display <- tagList(
            strong(title),
            lapply(sessions, function(x) {
                tagList(
                    p(x$path),
                    DT::renderDT({
                        DT::datatable(
                            x$data[, selected_cols],
                            rownames = FALSE,
                            style = "auto",
                            selection = "none",
                            options = list(
                                dom = "t",
                                ordering = FALSE
                            )
                        )
                    }),
                )
            })
        )
    }
}
