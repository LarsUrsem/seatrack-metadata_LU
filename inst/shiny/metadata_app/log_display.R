ReadLastLines <- function(x, n, ...) {
    con <- file(x)
    open(con)
    out <- scan(con, n, what = "char(0)", sep = "\n", quiet = TRUE, ...)

    while (TRUE) {
        tmp <- scan(con, 1, what = "char(0)", sep = "\n", quiet = TRUE)
        if (length(tmp) == 0) {
            close(con)
            break
        }
        out <- c(out[-1], tmp)
    }
    out
}

logger_ui <- function(id) {
    ns <- NS(id)

    tagList(
        uiOutput(ns("logpath")),
        verbatimTextOutput(ns("log"))
    )
}

logger_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # log_path <- paste0("seatrack_functions_log_", Sys.Date(), ".txt")
        # start_logging(log_file = log_path)

        log_path <- "app.log"

        log_file <- reactiveFileReader(
            session = session,
            filePath = log_path,
            intervalMillis = 1000,
            readFunc = function(filepath) {
                return(paste(ReadLastLines(filepath, 10), collapse = "\n"))
            }
        )

        output$logpath <- renderUI(h3(
            HTML(
                paste(
                    "Log", em(log_path)
                )
            )
        ))
        output$log <- renderText(log_file())
    })
}
