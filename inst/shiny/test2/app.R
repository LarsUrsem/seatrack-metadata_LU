library(shiny)

module_ui <- function(id) {
    ns <- NS(id)
    tagList(
        actionButton(ns("regular_button"), "Non dynamic button"),
        uiOutput(ns("dynamic_button_container")),
        actionButton(ns("browse"), "browse"),
    )
}

module_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id)
        btn_obs_list <- list()

        regular_listener <- observeEvent(input$regular_button, {
            print("Non dynamic button")
        })

        buttons_to_generate <- list(list(btn_name = "dynamic_button", btn_label = "Dynamic button"), list(btn_name = "another_dynamic_button", btn_label = "Another dynamic button"))

        output$dynamic_button_container <- renderUI({
            buttons <- lapply(buttons_to_generate, function(x) {
                if (is.null(btn_obs_list[[x$btn_name]])) {
                    btn_obs_list[[x$btn_name]] <<- observeEvent(input[[x$btn_name]], {
                        print(x$btn_name)
                        print(ns("foo"))
                        print(session$id)
                    })
                }

                actionButton(paste0("main-module-", x$btn_name), x$btn_label)
            })
        })

        observeEvent(
            {
                input$browse
            },
            {
                browser()
            }
        )
    })
}

main_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        module <- module_server("module")
    })
}

main_ui <- function(id) {
    ns <- NS(id)
    fluidPage(
        module_ui(ns("module"))
    )
}

shinyApp(
    ui = main_ui("main"),
    server = function(input, output, session) {
        main_server("main")
    }
)
