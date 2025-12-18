library(shiny)
library(bslib)
library(shinyFiles)

library(shinybusy)
library(promises)
library(future)

source("main_ui.R")
source("main_server.R")
source("folder_selector.R")
source("manage_loggers.R")
source("log_display.R")
source("manage_metadata.R")
source("manage_partner_metadata.R")

source("appender_file_safe.R")
source("mod_dt_tabs.R")

plan(multisession)

library(logger)
readRenviron(".Renviron")
library(seatrackRmetadata)

log_path <- getShinyOption("logging_path", "app.log")
log_appender(appender_file_safe(log_path))
log_threshold(INFO)



shinyApp(
    ui = main_ui("main"),
    server = function(input, output, session) {
        # bs_themer()
        main_server("main")
    }
)
