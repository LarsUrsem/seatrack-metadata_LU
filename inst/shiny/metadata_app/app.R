library(shiny)
library(shinyFiles)
library(seatrackRmetadata)
library(shinybusy)
library(promises)
library(future)

source("main_ui.R")
source("main_server.R")
source("folder_selector.R")
# source("file_selector.R")
source("log_display.R")
plan(multisession)

library(logger)

appender_file_safe <- function(file) {
    force(file)
    function(line) {
        # make sure folder exists
        dir <- dirname(file)
        if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

        con <- file(file, open = "a")
        writeLines(line, con)
        close(con)
    }
}

log_appender(appender_file_safe("app.log"))
log_threshold(INFO)

shinyApp(
    ui = main_ui("main"),
    server = function(input, output, session) {
        main_server("main")
    }
)
