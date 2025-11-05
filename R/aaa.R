#' Start logging to a file
#'
#' This function initializes logging to a specified directory.
#'
#' @param log_dir A character string specifying the directory where the log file will be saved. If NULL, the log file will be saved in the current working directory.
#' @param log_file A character string specifying the name of the log file. Default is "seatrack_functions_log.txt".
#' @return None
#'
#' @examples
#' \dontrun{
#' start_logging("/path/to/log/directory")
#' }
#' @export
#' @concept setup
start_logging <- function(log_dir = NULL, log_file = paste0("seatrack_functions_log_", Sys.Date(), ".txt")) {
    if (!is.null(log_dir)) {
        if (!dir.exists(log_dir)) {
            dir.create(log_dir, recursive = TRUE)
        }
        log_file <- file.path(log_dir, log_file)
    }

    log_appender(appender_tee(log_file))
    log_threshold(INFO)
    log_info("Logging started. Log file: ", log_file)
}


#' Set the base directory for the sea track folder
#'
#' This function sets a global variable used by other functions.
#' It also sets system locale to allow the handling of Norwegian characters in filenames.
#'
#' @param dir A character string specifying the path to the base directory.
#' @param language Character string specifying system language to add utf8 encoding to.
#'
#' @return None
#' @examples
#' \dontrun{
#' set_sea_track_folder("/path/to/sea/track/folder")
#' }
#' @export
#' @concept setup
set_sea_track_folder <- function(dir, language = "English_United Kingdom") {
    if (!dir.exists(dir)) {
        stop("The specified directory does not exist.")
    }

    the$sea_track_folder <- dir
    if (file.exists(".Renviron")) {
        environ_lines <- readLines(".Renviron")
    } else {
        environ_lines <- c()
    }

    environ_lines <- c(environ_lines, paste0("SEA_TRACK_FOLDER = '", dir, "'"))

    writeLines(unique(environ_lines), ".Renviron")

    log_info("Sea track folder set to: ", the$sea_track_folder)
    if (!grepl("utf", tolower(Sys.getlocale()), fixed = TRUE)) {
        log_info("Forcing locale to allow handling of Norwegian characters")
        Sys.setlocale("LC_CTYPE", paste0(language, ".utf8"))
    }
}

#' Get All Locations
#'
#' Retrieves a list of all locations (colonies) organized by country from the Sea Track folder.
#'
#' @return A named list where each element is a vector of colony names for a country.
#' @details The function expects the global variable `the$sea_track_folder` to be set, and looks for a "Locations" subfolder within it.
#' Each country is represented as a subdirectory within "Locations", and each colony is a subdirectory within its respective country folder.
#' If `the$sea_track_folder` is not set, the function will stop with an error message.
#' @examples
#' \dontrun{
#' set_sea_track_folder("/path/to/sea_track")
#' colonies <- get_all_locations()
#' print(colonies)
#' }
#' @export
#' @concept setup
get_all_locations <- function() {
    if (is.null(the$sea_track_folder)) {
        stop("Sea track folder is not set. Please use set_sea_track_folder() to set it.")
    }
    locations_path <- file.path(the$sea_track_folder, "Locations")
    if (!dir.exists(locations_path)) {
        stop("Locations folder not found in the sea track folder.")
    }

    countries <- list.dirs(locations_path, full.names = FALSE, recursive = FALSE)
    countries <- sort(countries)
    all_locations <- lapply(countries, function(country) {
        country_path <- file.path(locations_path, country)
        colonies <- list.dirs(country_path, full.names = FALSE, recursive = FALSE)
        return(colonies)
    })
    names(all_locations) <- countries
    return(all_locations)
}

### STARTUP HERE ###

the <- new.env(parent = emptyenv())
seatrack_dir <- Sys.getenv("SEA_TRACK_FOLDER", NA)
if (!is.na(seatrack_dir)) {
    set_sea_track_folder(seatrack_dir)
} else {
    the$sea_track_folder <- NULL
    log_info("Seatrack folder is not set for this project. Make sure to set it with: `set_sea_track_folder`")
}
the$master_sheet_paths <- list()