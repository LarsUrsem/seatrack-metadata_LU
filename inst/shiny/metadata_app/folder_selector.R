folder_selector_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h3("Seatrack folder"),
        verbatimTextOutput(ns("text")),
        shinyDirButton(ns("btn"), "Select SeaTrack Folder", "Choose folder")
    )
}

folder_selector_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        volumes <- c(Home = fs::path_home(), getVolumes()())
        shinyDirChoose(input, "btn",
            roots = volumes, session = session,
            allowDirCreate = FALSE
        )
        folder <- reactiveVal(seatrackRmetadata:::the$sea_track_folder)



        folder_check <- reactive({
            if (!is.null(folder()) || length(folder()) > 0) {
                return(folder())
            } else {
                return("No value set")
            }
        })

        observeEvent(input$btn, {
            path <- shinyFiles::parseDirPath(volumes, input$btn)
            # log$add(paste("Selected folder:", p))
            if (length(path) > 0) {
                set_sea_track_folder(path)
                folder(path)
            } else {
                set_sea_track_folder(NULL)
                folder(NULL)
            }
        })

        output$text <- renderText({
            folder_check()
        })
        return(reactive(folder()))
    })
}
