library(shiny)

shinyApp(
    ui = fluidPage(
        tagList(
            actionButton("regular_button", "Non dynamic button"),
            uiOutput("dynamic_button_container"),
            actionButton("browse", "browse"),
        )
    ),
    server = function(input, output) {
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
                    })
                }

                actionButton(x$btn_name, x$btn_label)
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
    }
)
