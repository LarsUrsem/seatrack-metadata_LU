#' Run the Shiny App
#'
#' @export
run_app <- function() {
    app_dir <- system.file("shiny/metadata_app", package = "seatrackRmetadata")
    if (app_dir == "") stop("Could not find Shiny app directory.", call. = FALSE)
    shiny::runApp(app_dir, display.mode = "normal")
}
