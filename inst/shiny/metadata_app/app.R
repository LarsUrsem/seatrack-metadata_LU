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

shinyApp(
    ui = main_ui("main"),
    server = function(input, output, session) {
        main_server("main")
    }
)
