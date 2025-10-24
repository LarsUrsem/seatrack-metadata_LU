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
    log_info("Sea track folder set to: ", the$sea_track_folder)
    if (!grepl("utf", tolower(Sys.getlocale()), fixed = TRUE)) {
        log_info("Forcing locale to allow handling of Norwegian characters")
        Sys.setlocale("LC_CTYPE", paste0(language, ".utf8"))
    }
}

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

#' Get the path of the master import file
#'
#' This function constructs a path to the master import file for a given colony.
#' On finding the path to a colony's master import sheets, it will be stored in the internal environment for later use.
#'
#' @param colony A character string specifying the name of the colony.
#' @param use_stored If TRUE, use a pre-existing path, rather than searching for a new one. Defaults to TRUE.
#' @return A character string representing the path to the master import file.
#' @examples
#' \dontrun{
#' get_master_import_folder("ColonyA")
#' }
#' @export
#' @concept metadata
get_master_import_path <- function(colony, use_stored = TRUE) {
    if (is.null(the$sea_track_folder)) {
        stop("Sea track folder is not set. Please use set_sea_track_folder() to set it.")
    }
    if (use_stored && colony %in% names(the$master_sheet_paths)) {
        return(the$master_sheet_paths[[colony]])
    }
    # Get the path to the master import folder
    master_import_folder <- file.path(the$sea_track_folder, "Database", "Imports_Metadata")

    # List all files in the master import folder
    files <- list.files(master_import_folder, pattern = "^[^~].*\\.xlsx$")

    # Split the filenames to get colony names
    colony_names <- sapply(strsplit(files, "_"), `[`, 2)

    # Try a straight match
    colony_bool <- colony == colony_names

    if (!any(colony_bool)) {
        # Try a within string match
        colony_bool <- grepl(colony, colony_names)
    }

    if (!any(colony_bool)) {
        # stop(paste("Colony", colony, "not found in the master import folder. Available colonies are:", paste(colony_names, collapse = ", ")))
        # Try the country of the colony
        all_country_colonies <- get_all_locations()
        colony_bool <- sapply(all_country_colonies, function(country_colonies) {
            return(colony %in% country_colonies)
        })
        country <- names(colony_bool)[colony_bool]
        country_file_bool <- colony_names == country
        if (any(country_file_bool)) {
            colony_file_name <- files[country_file_bool]
        } else {
            # Final fallback, open the file and check
            log_info("Master import file for colony '", colony, "' not found by location or country. Checking intended_location")
            colony_file_name <- character()
            for (import_file in files) {
                log_trace("Check ", import_file)
                import_file_path <- file.path(master_import_folder, import_file)
                master_import_list <- load_master_import(file_path = import_file_path)
                master_import_startup <- master_import_list$data$STARTUP_SHUTDOWN
                location_bool <- grepl(colony, master_import_startup$intended_location)
                log_trace("Checked ", import_file)
                if (any(location_bool)) {
                    colony_file_name <- import_file
                    break
                }
            }
        }
    } else {
        colony_file_name <- files[colony_bool]
    }

    if (length(colony_file_name) == 0) {
        log_warn("Master import file for colony '", colony, "' not found")
        the$master_sheet_paths[[colony]] <- NULL
        return(NULL)
    }

    full_colony_file_path <- file.path(master_import_folder, colony_file_name)
    log_success("Master import file for colony '", colony, "' found at: ", full_colony_file_path)
    the$master_sheet_paths[[colony]] <- full_colony_file_path
    return(full_colony_file_path)
}



#' Get paths to all startup Excel files
#'
#' This function retrieves paths to all Excel files in the "Starttime files and stored loggers" subdirectory of the sea track folder.
#'
#' @return A character vector containing paths to all Excel files in the specified subdirectory.
#' @examples
#' \dontrun{
#' all_xlsx_files <- get_startup_paths()
#' }
#' @export
#' @concept startups
get_startup_paths <- function() {
    if (is.null(the$sea_track_folder)) {
        stop("Sea track folder is not set. Please use set_sea_track_folder() to set it.")
    }
    start_time_path <- file.path(the$sea_track_folder, "Starttime files and stored loggers")
    subfolders <- rev(list.dirs(start_time_path, full.names = TRUE, recursive = FALSE))
    ignored_folders <- c("starttimes for other projects")
    for (ignored_folder in ignored_folders) {
        subfolders <- subfolders[!grepl(ignored_folder, subfolders, fixed = TRUE)]
    }
    all_xlsx_list <- lapply(subfolders, function(folder) {
        files <- list.files(folder, pattern = "^[^~].*\\.xlsx$", full.names = TRUE)
        return(files)
    })
    all_xlsx_list <- unlist(all_xlsx_list)
    return(all_xlsx_list)
}

#' Load specified sheets from an Excel file into a list of data frames
#'
#' This function reads specified sheets from an Excel file and returns them as a list of data frames.
#' It provides options to skip rows, force date columns to be of Date type, and drop unnamed columns.
#'
#' @param file_path A character string specifying the path to the Excel file.
#' @param sheets A character vector specifying the names of the sheets to be read.
#' @param skip_rows An integer specifying the number of rows to skip at the beginning of each sheet. Default is 0.
#' @param force_date A logical indicating whether to attempt to convert date columns to Date type. Default is TRUE.
#' @param drop_unnamed A logical indicating whether to drop unnamed columns (columns with no header). Default is TRUE.
#' @param col_types A list the same length as sheets, containing either NULL or a numeric vector of classes as in `openxlsx2::read_xlsx`.
#' @param col_types A list the same length as sheets, containing either NULL or a character vector of column names to be forced into uppercase.
#' @return A `LoadedMetadata` object, with a list of tibbles corresponding to each sheet in `data` and the original workbook in `wb``
#' @examples
#' \dontrun{
#' sheets_data <- load_sheets_as_list("path/to/file.xlsx", c("Sheet1", "Sheet2"), skip = 1)
#' }
#' @export
#' @concept utility
load_sheets_as_list <- function(file_path, sheets, skip_rows = 0, force_date = TRUE, drop_unnamed = TRUE, 
col_types = rep(NULL, length(sheets)), col_upper = rep(NULL, length(sheets))) {
    if (!file.exists(file_path)) {
        stop("The specified file does not exist.")
    }
    log_trace("Loading file: ", file_path)
    wb <- openxlsx2::wb_load(file_path)
    # Iterate through sheets and read data
    data_list <- lapply(1:length(sheets), function(sheet_index) {
        sheet <- sheets[sheet_index]
        sheet_col_types <- col_types[[sheet_index]]
        sheet_upper <- col_upper[[sheet_index]]
        log_trace("Loading sheet: ", sheet)

        if (!is.null(sheet_col_types)) {
            col_range <- 1:length(sheet_col_types)
        } else {
            col_range <- NULL
        }

        arg_list <- list(
            file = wb,
            sheet = sheet,
            start_row = skip_rows + 1,
            skip_empty_rows = TRUE,
            cols = col_range,
            na.strings = c("", "End", "end", "none")
        )
        if (!is.null(sheet_col_types)) {
            arg_list$types <- sheet_col_types
        }

        sheet_df <- do.call(openxlsx2::wb_to_df, arg_list)
        log_trace("Loaded sheet: ", sheet)

        current_sheet <- tibble(sheet_df[, !is.na(names(sheet_df))])

        if (force_date) {
            # keep dates as dates only
            # Get columns where the class is POSIXt and the column name contains "date"
            datetime_cols <- sapply(current_sheet, inherits, what = "POSIXt")
            date_cols <- datetime_cols & sapply(names(current_sheet), function(x) grepl("date", x, ignore.case = TRUE))
            # Convert those columns to Date
            current_sheet[date_cols] <- lapply(current_sheet[date_cols], as.Date)

            # I think this is a single use case, and if anything it is easier to handle the time later
            # # Get columns where the class is POSIXt and the year is before 1900
            # time_cols <- datetime_cols & sapply(current_sheet[datetime_cols], function(x) any(x < as.POSIXct("1900-01-01", tz = "UTC")))

            # # Convert those columns to character (to preserve time information)
            # current_sheet[time_cols] <- lapply(current_sheet[time_cols], function(x) format(x, "%H:%M:%S"))
        }

        if (drop_unnamed) {
            # unnamed_cols <- grepl("^\\.\\.\\.", names(current_sheet)) | is.na(names(current_sheet))
            unnamed_cols <- grepl("NA.", names(sheet_df))
            if (sum(unnamed_cols > 0)) {
                # Drop columns that are unnamed (i.e., their names start with "..." or are NA)
                log_trace("Dropping ", sum(unnamed_cols), " unnamed columns from sheet: ", sheet)
                current_sheet <- current_sheet[, !unnamed_cols]
            }
        }
        if (!is.null(sheet_upper)) {
            for (col_name in sheet_upper) {
                current_sheet[[col_name]] <- toupper(current_sheet[[col_name]])
            }
        }

        return(current_sheet)
    })
    names(data_list) <- sheets
    loaded_sheets <- LoadedWB$new(data = data_list, wb = wb)
    return(loaded_sheets)
}

#' Load nonresponsive logger sheet for current year
#'
#' This function loads the record of unresponsive loggers. If the filepath provided does not exist, it initialises new sheets.
#' @param file_path String indicating from where the file should be loaded from.
#' @return A LoadedWB containing the unresponsive logger data.
#'
#' @concept nonresponsive
load_nonresponsive_sheet <- function(file_path) {
    # Check if file already exists

    if (file.exists(file_path)) {
        # If so, load it
        wb <- openxlsx2::wb_load(file_path)
        loaded_sheet <- openxlsx2::read_xlsx(wb)
    } else {
        wb <- openxlsx2::wb_workbook()
        loaded_sheet <- tibble(
            logger_serial_no = character(),
            logger_model = character(),
            producer = character(),
            production_year = numeric(),
            project = character(),
            starttime_gmt = as.POSIXct(character()),
            logging_mode = numeric(),
            days_delayed = numeric(),
            programmed_gmt_time = as.POSIXct(character()),
            download_type = character(),
            download_date = as.Date(character()),
            intended_species = character(),
            intended_location = character(),
            comment = character(),
            priority = character(),
            sent = as.POSIXct(character())
        )
        }

    return(LoadedWB$new(data = list(sheet1 = loaded_sheet), wb = wb))
}

#' Load multiple nonresponsive logger sheets
#'
#' This function loads nonresponsive logger sheets for multiple file paths and manufacturers.
#'
#' @param file_paths A character vector of file paths to load.
#' @param manufacturers A character vector of manufacturers, same length as file_paths.
#' @return A named list of tibbles, each containing nonresponsive logger data for the corresponding manufacturer.
#' @examples
#' \dontrun{
#' file_paths <- c("lotek.xlsx", "migratetech.xlsx")
#' manufacturers <- c("Lotek", "Migrate Technology")
#' nonresponsive_list <- load_nonresponsive(file_paths, manufacturers)
#' }
#' @export
#' @concept nonresponsive
load_nonresponsive <- function(file_paths, manufacturers) {
    if (length(file_paths) != length(manufacturers)) {
        stop("file_paths and manufacturers must be the same length.")
    }
    sheets_list <- lapply(seq_along(file_paths), function(i) {
        load_nonresponsive_sheet(file_paths[i])
    })
    names(sheets_list) <- tolower(manufacturers)
    sheet_collection <- LoadedWBCollection$new(sheets_list = sheets_list)

    return(sheet_collection)
}

#' Find logger instances in files
#'
#' This function tries to find a logger ID in all master import files. You can either provide an already loaded list of master import sheets, or the function will load them itself.
#'
#' @param logger_id logger ID of desired logger
#' @param all_master_import_list list of master import sheets, as returned by `load_all_master_import(combine = FALSE)`. If not provided, this will be generated inside the function
#'
#' @return list of all matches, where each element of the list is a list containing the row of data, the file path and the index.
#' @concept loggers
get_logger_from_metadata <- function(logger_id, all_master_import_list = NULL) {
    if (is.null(all_master_import_list)) {
        all_master_import_list <- load_all_master_import(combine = FALSE)
    }
    search_result <- lapply(seq_along(all_master_import_list), function(i) {
        import_sheet <- all_master_import_list[[i]]
        logger_idx <- which(import_sheet$data$STARTUP_SHUTDOWN$logger_serial_no == logger_id)
        if (length(logger_idx) > 0) {
            data_list <- lapply(logger_idx, function(logger_idx_i) {
                import_sheet$data$STARTUP_SHUTDOWN[logger_idx_i, ]
            })
            data_rows <- do.call(rbind, data_list)
            return(list(path = import_sheet$path, data = data_rows, list_index = i, row_index = logger_idx))
        }
    })
    search_result_nonull <- search_result[which(!sapply(search_result, is.null))]
    return(search_result_nonull)
}

#' Load master import file for all colonies
#'
#' This function attempts to load all master import sheets available in the seatrack folder.
#' @param combine Boolean determining whether or not to combine the sheets into a single dataframe.
#' @param skip character vector of location names to not load.
#' @return If combine is TRUE: A tibble consisting of combined metadata and startup_shutdown sheets, with an extra column for path appended to each.
#'  Otherwise a list where every element is a LoadedWB object.
#' @export
#' @concept metadata
load_all_master_import <- function(combine = TRUE, skip = c()) {
    if (is.null(the$sea_track_folder)) {
        stop("Sea track folder is not set. Please use set_sea_track_folder() to set it.")
    }
    all_colony <- unlist(get_all_locations())
    all_colony <- all_colony[!all_colony %in% skip]

    all_paths <- lapply(all_colony, get_master_import_path)

    no_path <- all_colony[which(sapply(all_paths, is.null))]
    if (length(no_path) > 0) {
        missing_import_files <- data.frame(country = names(no_path), colony = no_path)
        missing_import_files$country <- gsub("\\d+$", "", missing_import_files$country)
        row.names(missing_import_files) <- NULL
        log_warn("Import files for the following locations could not be found: \n", paste(capture.output(print(missing_import_files)), collapse = "\n"))
    }


    null_path_idx <- which(!sapply(all_paths, is.null))

    all_paths <- all_paths[null_path_idx]
    all_colony <- all_colony[null_path_idx]

    distinct_idx <- which(!duplicated(all_paths))
    all_colony <- all_colony[distinct_idx]
    all_paths <- all_paths[distinct_idx]

    all_sheets <- lapply(all_colony, load_master_import)

    names(all_sheets) <- all_colony

    if (!combine) {
        # If not combining, return the lists
        return(all_sheets)
    }

    all_data <- lapply(all_sheets, function(x) x$data)
    # combine metadata
    all_metadata <- lapply(all_data, function(x) x$METADATA)

    # If the master_metadata sheet is is lacking the nest_latitude and nest_longitude columns, add them
    all_metadata <- lapply(all_metadata, function(x) {
        if (!"nest_latitude" %in% colnames(x)) {
            x$nest_latitude <- NA
        }
        if (!"nest_longitude" %in% colnames(x)) {
            x$nest_longitude <- NA
        }
        return(x)
    })

    all_metadata_names <- unique(unlist(sapply(all_metadata, names)))

    # which columns are in all
    names_present_bools <- lapply(all_metadata, function(x) all_metadata_names %in% names(x))
    names_present_bools <- do.call(rbind, names_present_bools)
    in_all_bool <- colSums(names_present_bools) == nrow(names_present_bools)
    in_all_names <- all_metadata_names[in_all_bool]
    not_in_all_names <- all_metadata_names[!in_all_bool]

    for (i in seq_along(all_metadata)) {
        metadata <- all_metadata[[i]]
        bad_names_bool <- not_in_all_names %in% names(metadata)
        if (any(bad_names_bool)) {
            bad_names <- not_in_all_names[bad_names_bool]
            path <- all_paths[[i]]
            log_warn(path, " had the following non-standard columns: ", paste0(bad_names, collapse = ", "))
        }
    }

    all_metadata_clean <- lapply(all_metadata, function(x) x[, in_all_names])
    all_metadata_clean <- lapply(seq_along(all_metadata_clean), function(i) {
        metadata <- all_metadata_clean[[i]]
        metadata$path <- all_paths[[i]]
        return(metadata)
    })

    all_metadata_clean <- do.call(rbind, all_metadata_clean)
    # combine startups
    all_startup <- lapply(all_data, function(x) x$`STARTUP_SHUTDOWN`)

    all_startup <- lapply(seq_along(all_startup), function(i) {
        startup <- all_startup[[i]]
        startup$path <- all_paths[[i]]
        return(startup)
    })


    all_startup <- do.call(rbind, all_startup)

    return(list(METADATA = all_metadata_clean, STARTUP_SHUTDOWN = all_startup))
}

#' Load master import file for a given colony
#'
#' This function loads the master import file for a specified colony or directly from a file path.
#' Either a colony name of a file path must be provided.
#' It iterates through the appropriate sheets and combines the data into a list of data frames.
#' @param colony A character string specifying the name of the colony.
#' @param file_path File path of master import file
#' @param use_stored If TRUE, use a pre-existing path, rather than searching for a new one. Defaults to TRUE.
#' @return A LoadedWB object.
#' @examples
#' \dontrun{
#' load_master_import("ColonyA")
#' }
#' @export
#' @concept metadata
load_master_import <- function(colony = NULL, file_path = NULL, use_stored = TRUE) {
    if (!is.null(colony) && is.null(file_path)) {
        file_path <- get_master_import_path(colony, use_stored)
    }

    if (is.null(file_path)) {
        stop("Unable to find master import sheet for this colony.")
    }

    if (!file.exists(file_path)) {
        stop("The specified master import file does not exist.")
    }

    # Desired sheets
    sheets <- c("METADATA", "STARTUP_SHUTDOWN")
    startup_col_types <- c(
        logger_serial_no = 0,
        logger_model = 0,
        producer = 0,
        production_year = 1,
        project = 0,
        starttime_gmt = 3,
        logging_mode = 1,
        started_by = 0,
        started_where = 0,
        days_delayed = 1,
        programmed_gmt_time = 3,
        intended_species = 0,
        intended_location = 0,
        intended_deployer = 0,
        shutdown_session = 4,
        field_status = 0,
        downloaded_by = 0,
        download_type = 0,
        download_date = 2,
        decomissioned = 2,
        shutdown_date = 2,
        comment = 0
    )

    import_list <- load_sheets_as_list(file_path, sheets, col_types = list(NULL, startup_col_types))

    return(import_list)
}


#' Load partner provided metadata from an Excel file
#'
#' This function reads metadata provided by partners from an Excel file.
#' It iterates through the appropriate sheets and combines the data into a list of data frames.
#' @param file_path A character string specifying the absolute path to the Excel file.
#' @return A list of data frames, each corresponding to a sheet in the Excel file.
#' @examples
#' \dontrun{
#' load_partner_metadata("path/to/partner_metadata.xlsx")
#' }
#' @export
#' @concept metadata
load_partner_metadata <- function(file_path) {
    if (!file.exists(file_path)) {
        stop("The specified file does not exist.")
    }

    # Desired sheets
    sheets <- c("ENCOUNTER DATA", "LOGGER RETURNS", "RESTART TIMES")

    # Skip the first row as it contains extra headers.
    metadata_list <- load_sheets_as_list(file_path, sheets, 1, col_types = list(
        NULL,
        NULL,
        c(
            logger_id = 0,
            logger_model = 0,
            startdate_GMT = 2,
            starttime_GMT = 3,
            `Logging mode` = 0,
            intended_species = 0,
            comment = 0
        )),
        col_upper = list(
            c("logger_id_retrieved", "logger_id_deployed"),
            c("logger_id"),
            c("logger_id")
        )
        )

    return(metadata_list)
}

# Append encounter data
#'
#' Append encounter data to the master import metadata
#'
#' @param master_metadata A data frame representing the master import metadata.
#' @param encounter_data A data frame representing the encounter data to be appended.
#' @return A data frame with the encounter data appended to the master metadata.
#' @examples
#' \dontrun{
#' updated_master_metadata <- append_encounter_data(master_metadata, encounter_data)
#' }
#' @export
#' @concept encounters
append_encounter_data <- function(master_metadata, encounter_data) {
    if (nrow(encounter_data) == 0) {
        log_info("No encounter data to append.")
        return(master_metadata)
    }

    # Change longer column names in encounter_data to match those in master_metadata
    names(encounter_data)[grep("other relevant variables", names(encounter_data), fixed = TRUE)] <- "other"

    # remove invalid rows from encounter_data
    encounter_data <- encounter_data[!is.na(encounter_data$date), ]

    # If the master_metadata sheet is is lacking the nest_latitude and nest_longitude columns, add them
    if (!"nest_latitude" %in% colnames(master_metadata)) {
        master_metadata$nest_latitude <- NA
    }
    if (!"nest_longitude" %in% colnames(master_metadata)) {
        master_metadata$nest_longitude <- NA
    }

    # Throw an error if there are any columns in encounter_data that are not in master_metadata
    if (any(!colnames(encounter_data) %in% colnames(master_metadata))) {
        stop(paste("The following columns are in encounter_data but not in master_metadata:", paste(colnames(encounter_data)[!colnames(encounter_data) %in% colnames(master_metadata)], collapse = ", ")))
    }

    # Make sure the column order matches
    master_metadata <- master_metadata[, colnames(encounter_data)]

    # Check if duplicate rows exist based on logger_id and date

    encounter_id_date <- paste(encounter_data$ring_number, encounter_data$logger_id_retrieved, encounter_data$logger_id_deployed, encounter_data$date)
    master_metadata_id_date <- paste(master_metadata$ring_number, master_metadata$logger_id_retrieved, master_metadata$logger_id_deployed, master_metadata$date)

    if (sum(encounter_id_date %in% master_metadata_id_date) > 0) {
        log_trace("Duplicate rows found based on logger_id and date. These rows will not be appended to the master metadata.")
        # Remove duplicate rows from encounter_data
        encounter_data <- encounter_data[!encounter_id_date %in% master_metadata_id_date, ]
    }

    updated_metadata <- rbind(master_metadata, encounter_data)

    log_success("Appended ", nrow(encounter_data), " rows to master metadata. New total is ", nrow(updated_metadata), " rows.")

    return(updated_metadata)
}

#' Attempt to add logger from startup sheets
#'
#' This function attempts to add a logger to the master startup data frame from the startup sheets.
#' Because the data quality of older startup sheets is variable, the function checks for column mismatches and skips these files.
#' Incorrectly formatted datetime columns can also lead to issues.
#'
#' @param master_startup A data frame containing the master startup and shutdown information.
#' @param partner_metadata Dataframe of loggers handled by partners
#'
#' @return A new version of the master startup data frame, with the logger added if succesful.
#' @examples
#' \dontrun{
#' updated_master_startup <- add_loggers_from_startup_sheets(master_startup)
#' }
#' @export
#' @concept startups
add_loggers_from_startup <- function(master_startup, partner_metadata) {
    partner_logger_ids <- unique(c(partner_metadata$logger_id_retrieved, partner_metadata$logger_id_deployed))
    partner_logger_ids <- partner_logger_ids[!is.na(partner_logger_ids)]
    # Force imported classes
    master_classes <- sapply(master_startup, function(variable) paste(class(variable), collapse = "/"))
    excel_classes <- master_classes
    excel_classes[master_classes == "POSIXct/POSIXt"] <- 3
    excel_classes[master_classes == "Date"] <- 2
    excel_classes[master_classes == "character"] <- 0
    excel_classes[master_classes == "numeric"] <- 1
    excel_classes[master_classes == "logical"] <- 4
    excel_classes_numeric <- as.numeric(excel_classes)
    names(excel_classes_numeric) <- names(excel_classes)

    log_trace("Checking for new loggers in startup files")
    master_logger_id_date <- paste(master_startup$logger_serial_no, as.character(master_startup$starttime_gmt))
    startup_paths <- get_startup_paths()
    for (startup_path in startup_paths) {
        log_trace("Processing startup file: ", startup_path)

        # Peek at excel file to determine column number
        startup_file <- tryCatch(
            suppressWarnings(openxlsx2::read_xlsx(startup_path, rows = c(1))),
            error = function(e) {
                log_trace(paste("Unable to import:", startup_path, e))
                return(NULL)
            }
        )
        if (is.null(startup_file)) {
            next
        }

        if (ncol(startup_file) != ncol(master_startup)) {
            log_trace(paste("Skipping startup file due to column number mismatch:", startup_path))
            next
        }

        # Peeking at the excel files can miss empty columns created further down.
        # Import is wrapped in a try catch to handle import failures.
        startup_file <- tryCatch(
            suppressWarnings(openxlsx2::read_xlsx(startup_path, types = excel_classes_numeric)),
            error = function(e) {
                log_trace(paste("Unable to import:", startup_path, e))
                return(NULL)
            }
        )
        if (is.null(startup_file)) {
            next
        }
        # log_warn(paste(names(warnings()), collapse = "\n"))

        # Skip files whose columns do not match master_startup
        if (!all(colnames(master_startup) %in% colnames(startup_file))) {
            log_trace(paste("Skipping startup file due to column mismatch:", startup_path))
            next
        }

        # As I now force the column types based on the master file, this snippet is not needed.
        # class_match = sapply(1:ncol(master_startup), function(i) {
        #     # Ignore cases where this column is all NA in the file - this means the excel import will not have guessed the class correctly
        #     if (all(is.na(startup_file[[i]])) | all(is.na(master_startup[[i]]))) {
        #         return(TRUE)
        #     }
        #     return(all(class(startup_file[[i]]) == class(master_startup[[i]])))
        # })

        # if (any(!class_match)) {
        #     mismatched_cols <- colnames(startup_file)[!class_match]
        #     master_class <- sapply(which(!class_match), function(i) paste(class(master_startup[[i]]), collapse = "/"))
        #     startup_class <- sapply(which(!class_match), function(i) paste(class(startup_file[[i]]), collapse = "/"))
        #     mismatch_summary <- tibble::tibble(Column = mismatched_cols, Master_Class = master_class, Startup_Class = startup_class)
        #     log_warn(paste("Skipping startup file:",startup_path," due to class mismatch in columns:\n", paste(capture.output(print(mismatch_summary, n = nrow(mismatch_summary)))[c(-1, -3)], collapse = "\n")))
        #     next
        # }

        startup_file <- tibble(startup_file)

        # Filter to only include rows where the logger has been handled
        startup_file <- startup_file[
            !is.na(startup_file$starttime_gmt) &
            !is.na(startup_file$logger_serial_no) &
            startup_file$logger_serial_no %in% partner_logger_ids, ]

        if (nrow(startup_file) == 0) {
            next
        }
        startup_logger_id_date <- paste(startup_file$logger_serial_no, as.character(startup_file$starttime_gmt))
        # get logger ID/date combinations that do not appear in master_startup
        new_logger_indices <- which(!startup_logger_id_date %in% master_logger_id_date)
        if (length(new_logger_indices) > 0) {
            new_loggers <- startup_file[new_logger_indices, ]
            for (date_index in which(master_classes == "Date")) {
                new_loggers[, date_index] <- sapply(new_loggers[, date_index], as.Date)
            }

            n_loggers <- nrow(new_loggers)
            log_success("Adding ", n_loggers, " new loggers from startup file: ", startup_path)

            logger_summary <- new_loggers[, c("logger_serial_no", "logger_model", "production_year", "starttime_gmt", "intended_location")]

            log_success("New loggers:\n", paste(capture.output(print(logger_summary, n = n_loggers))[c(-1, -3)], collapse = "\n"))

            master_startup <- rbind(master_startup, new_loggers)
        }
    }
    return(master_startup)
}


#' Find a logger's unfinished session in the master startup data frame
#'
#' This function finds the unfinished session for a given logger in the master startup data frame.
#'
#' @param master_startup A data frame containing the master startup and shutdown information.
#' @param logger_id A character string specifying the logger ID.
#' @param logger_download_stop_date A Date object specifying the reported download/stop date of the logger.
#'
#' @return A list containing the index of the unfinished session and the session data frame, or NULL if no unfinished session is found.
#' @examples
#' \dontrun{
#' unfinished_session <- get_unfinished_session(master_startup, "Logger123", as.Date("2023-01-15"))
#' }
#' @export
#' @concept loggers
get_unfinished_session <- function(master_startup, logger_id, logger_download_stop_date) {
    # Find session in master_startup
    # Get logger ID unfinished sessions
    unfinished_bool <- is.na(master_startup$shutdown_date) & is.na(master_startup$download_date) & master_startup$logger_serial_no == logger_id
    unfinished_indices <- which(unfinished_bool)
    master_startup_unfinished <- master_startup[unfinished_indices, ]
    if (nrow(master_startup_unfinished) == 0) {
        log_warn(paste0("No unfinished session found for logger ID: ", logger_id, "."))
        return(NULL)
    } else if (nrow(master_startup_unfinished) >= 1) {
        if (nrow(master_startup_unfinished) > 1) {
            log_trace(paste0("Multiple unfinished sessions without end dates found for logger ID: ", logger_id, ". Trying to use closest startup date."))
        } else {
            log_trace(paste0("Unfinished session found for logger ID: ", logger_id, ". Checking dates."))
        }

        if (!is.na(logger_download_stop_date)) {
            # Check the closest startup date before the download date where there is not a finished session in between

            # calculate difference between reported download date and startup date

            logger_session_indices <- which(master_startup$logger_serial_no == logger_id &
                !is.na(master_startup$starttime_gmt))

            logger_sessions <- master_startup[logger_session_indices, ]

            time_diffs <- as.numeric(difftime(logger_download_stop_date,
                logger_sessions$starttime_gmt,
                units = "days"
            ))

            logger_sessions_finished <- which(!(is.na(logger_sessions$shutdown_date) | is.na(logger_sessions$download_date)))

            time_diffs[time_diffs < 0] <- NA # Ignore future dates
            if (length(logger_sessions_finished) > 0) {
                time_diffs[1:max(logger_sessions_finished)] <- NA # ignore finished sessions and unfinished sessions falling before a finished session
            }

            closest_index <- which(time_diffs == min(time_diffs, na.rm = TRUE) & !is.na(time_diffs))
            if (length(closest_index) == 0) {
                log_warn(paste("No suitable unfinished session found for:", logger_id))
                return(NULL)
            } else if (length(closest_index) > 1) {
                log_warn(paste("Cannot resolve multiple unfinished sessions for:", logger_id))
                return(NULL)
            }
            unfinished_indices <- logger_session_indices[closest_index]
            master_startup_unfinished <- master_startup[unfinished_indices, ]
        } else {
            log_warn(paste0("No download date available for logger ID:", logger_id, ". Cannot resolve multiple unfinished sessions."))
            return(NULL)
        }
    }
    log_success(paste("Found unfinished session for logger ID:", logger_id, logger_download_stop_date))
    unfinished_summary <- master_startup_unfinished[, c("logger_serial_no", "starttime_gmt", "intended_species", "intended_location")]
    log_success("Unfinished session:\n", paste(capture.output(print(unfinished_summary, n = nrow(unfinished_summary)))[c(-1, -3)], collapse = "\n"))
    return(list(index = unfinished_indices, session = master_startup_unfinished))
}

#' Modify logger status to end a session
#'
#' Function to modify a logger status, setting download_date, shutdown_date and logger status
#'
#' @param logger_id logger serial number
#' @param logger_status Character string giving the new logger `download_type`
#' @param downloaded_by Character string containing the name of the user who is downloading the data. If NULL this will not be modified
#' @param download_date Date to set the download_date/shutdown_date to. If NULL, this will be today's date.
#' @param comment Optional comment to append to the existing comment string.
#' @param master_sheet master sheet in which the logger will be found. If not provided, all sheets will be checked.
#' @param all_master_sheet If master_sheet is not provided, all sheets will be checked. To save these being loaded every time, they can be provided using `load_all_master_import(combine = FALSE)`
#' @param nonresponsive_list A named list of tibbles, each containing nonresponsive logger data for different manufacturers.
#'
#' @return list containing modified version of master_sheet, modified version of nonresponsive_list
#' @export
#' @concept loggers
end_logger_session <- function(logger_id, logger_status, downloaded_by = "", download_date = NULL, comment = "", master_sheet = NULL, all_master_sheet = NULL, nonresponsive_list = list()) {
    if (is.null(download_date)) {
        download_date <- as.Date(Sys.time())
    }
    new_data <- list(download_type = logger_status, downloaded_by = downloaded_by, download_date = download_date, shutdown_date = shutdown_date, comment = comment)
    result_list <- modify_logger_status(logger_id, new_data, master_sheet, all_master_sheet, nonresponsive_list)
    return(result_list)
}

#' Modify logger status
#'
#' Function to modify a logger status, such as download_date and shutdown_date if the logger session is open.
#'
#' @param logger_id logger serial number
#' @param new_data named list of new data to be inserted, where the name of each element corresponds to a column in the sheet.
#' @param master_sheet master sheet in which the logger will be found. If not provided, all sheets will be checked.
#' @param all_master_sheet If master_sheet is not provided, all sheets will be checked. To save these being loaded every time, they can be provided using `load_all_master_import(combine = FALSE)`
#' @param nonresponsive_list A named list of tibbles, each containing nonresponsive logger data for different manufacturers.
#'
#' @return list containing modified version of master_sheet, modified version of nonresponsive_list
#' @export
#' @concept loggers
modify_logger_status <- function(logger_id, new_data = list(), master_sheet = NULL, all_master_sheet = NULL, nonresponsive_list = list()) {
    if (is.null(master_sheet)) {
        if (is.null(all_master_sheet)) {
            all_master_sheet <- load_all_master_import(combine = FALSE)
        }
        logger_search <- get_logger_from_metadata(logger_id, all_master_sheet)
        if (length(logger_search) == 0) {
            stop(paste("No master sheet for logger", logger_id))
        }
        master_sheet <- all_master_sheet[[logger_search[[1]]$list_index]]
    }
    if (!"download_date" %in% names(new_data)) {
        download_date <- as.Date(Sys.time())
    }
    # Check for an open session
    unfinished_session <- get_unfinished_session(master_sheet$data$STARTUP_SHUTDOWN, logger_id, download_date)
    if (is.null(unfinished_session)) {
        stop(paste("No unfinished session for logger", logger_id))
    }
    if ("comment" %in% names(new_data)) {
        master_sheet$data$STARTUP_SHUTDOWN <- set_comments(master_sheet$data$STARTUP_SHUTDOWN, unfinished_session$index, new_data$comment)
    }
    for (col_name in names(new_data)[names(new_data) != "comment"]) {
        master_sheet$data$STARTUP_SHUTDOWN <- set_master_startup_value(
            master_sheet$data$STARTUP_SHUTDOWN,
            unfinished_session$index,
            col_name,
            new_data[[col_name]]
        )
    }

    if ("download_type" %in% names(new_data) && new_data$download_type == "Nonresponsive") {
        nonresponsive_for_manufacturer <- master_sheet$data$STARTUP_SHUTDOWN[unfinished_session$index, ]
        manufacturer <- lower(nonresponsive_for_manufacturer$manufacturer)

        new_nonresponsive <- tibble(
            logger_serial_no = nonresponsive_for_manufacturer$logger_serial_no,
            logger_model = nonresponsive_for_manufacturer$logger_model,
            producer = nonresponsive_for_manufacturer$producer,
            production_year = nonresponsive_for_manufacturer$production_year,
            project = nonresponsive_for_manufacturer$project,
            starttime_gmt = nonresponsive_for_manufacturer$starttime_gmt,
            download_type = "Nonresponsive",
            download_date = nonresponsive_for_manufacturer$download_date,
            comment = nonresponsive_for_manufacturer$comment,
            intended_species = nonresponsive_for_manufacturer$intended_species,
            intended_location = nonresponsive_for_manufacturer$intended_location,
            logging_mode = nonresponsive_for_manufacturer$logging_mode,
            days_delayed = nonresponsive_for_manufacturer$days_delayed,
            programmed_gmt_time = nonresponsive_for_manufacturer$programmed_gmt_time,
            priority = NA,
            sent = NA)

        nonresponsive_list <- append_to_nonresponsive(nonresponsive_list, new_nonresponsive, manufacturer)

    }

    return(list(master_sheet = master_sheet, nonresponsive_list = nonresponsive_list))
}

#' Get all nonresponsive loggers and export
#'
#' Checks the master startup sheets for loggers marked as nonresponsive handled between certain dates
#' and returns per-manufacturer nonresponsive sheets.
#'
#' @param all_metadata_combined Tibble containing combined startup information from master startup sheets, as generated by `load_all_master_import(TRUE)`
#' @param nonresponsive_list A list containing tibbles of unresponsive loggers for different manufacturers.
#' The name of the list element should match the producer name in master_startup (e.g., "Lotek", "MigrateTech").
#' This can be generated with the `load_nonresponsive` function
#' @param start_date Date or character string specifying the start date for considering nonresponsive loggers. If NULL, defaults to January 1st of the current year.
#' @param end_date Date or character string specifying the end date for considering nonresponsive loggers. If NULL, defaults to Inf (no end date).
#'
#' @return Updated list of nonresponsive logger sheets
#' @concept nonresponsive
#' @export
nonresponsive_from_master <- function(all_metadata_combined, nonresponsive_list, start_date = NULL, end_date = Inf) {
    if (is.null(start_date)) {
        start_date <- format(Sys.Date(), "%Y-01-01")
    }

    nonresponsive_rows <- all_metadata_combined$STARTUP_SHUTDOWN[!is.na(all_metadata_combined$STARTUP_SHUTDOWN$download_date) &
        all_metadata_combined$STARTUP_SHUTDOWN$download_date >= as.Date(start_date) &
        all_metadata_combined$STARTUP_SHUTDOWN$download_date <= as.Date(end_date), ]

    nonresponsive_rows$producer_2 <- tolower(nonresponsive_rows$producer)
    # biotrack loggers should go in the lotek sheet
    nonresponsive_rows$producer_2[nonresponsive_rows$producer_2 == "biotrack"] <- "lotek"

    for (manufacturer in tolower(nonresponsive_list$names())) {
        nonresponsive_for_manufacturer <- nonresponsive_rows[nonresponsive_rows$producer_2 == manufacturer, ]

        if (nrow(nonresponsive_for_manufacturer) == 0) {
            next
        }

        new_nonresponsive <- tibble(
            logger_serial_no = nonresponsive_for_manufacturer$logger_serial_no,
            logger_model = nonresponsive_for_manufacturer$logger_model,
            producer = nonresponsive_for_manufacturer$producer,
            production_year = nonresponsive_for_manufacturer$production_year,
            project = nonresponsive_for_manufacturer$project,
            starttime_gmt = nonresponsive_for_manufacturer$starttime_gmt,
            download_type = "Nonresponsive",
            download_date = nonresponsive_for_manufacturer$download_date,
            comment = nonresponsive_for_manufacturer$comment, 
            intended_species = nonresponsive_for_manufacturer$intended_species,
            intended_location = nonresponsive_for_manufacturer$intended_location,
            logging_mode = nonresponsive_for_manufacturer$logging_mode,
            days_delayed = nonresponsive_for_manufacturer$days_delayed,
            programmed_gmt_time = nonresponsive_for_manufacturer$programmed_gmt_time,
            priority = NA,
            sent = NA
        )
        nonresponsive_list <- append_to_nonresponsive(nonresponsive_list, new_nonresponsive, manufacturer)
    }
    return(nonresponsive_list)
}

#' Append to nonresponsive list
#'
#' This function appends to the approrpiate sheet in a list of nonresponsive sheets. 
#' It will check for duplicate logger IDs and ensure column ordering matches.
#'
#' @param nonresponsive_list A list containing tibbles of unresponsive loggers for different manufacturers.
#' The name of the list element should match the producer name in master_startup (e.g., "Lotek", "MigrateTech").
#' This can be generated with the `load_nonresponsive` function
#' @param new_nonresponsive Tibble containing new rows to be appended.
#' @param manufacturer Character string of name of manufacturer whose nonresponsive sheet is to be appended to.
#' @return Modified nonresponsive_list
#' @concept nonresponsive
#' @export
append_to_nonresponsive <- function(nonresponsive_list, new_nonresponsive, manufacturer) {
    # reorder columns

    current_rows <- nonresponsive_list$sheets_list[[manufacturer]]$data[[1]]
    missing_cols <- setdiff(names(current_rows), names(new_nonresponsive))
    if (length(missing_cols) > 0) {
        for (col in missing_cols) {
            new_nonresponsive[[col]] <- NA
        }
    }
    new_nonresponsive <- new_nonresponsive[, names(current_rows)]


    all_rows <- rbind(current_rows, new_nonresponsive)
    all_rows_non_dup <- all_rows[!duplicated(all_rows$logger_serial_no), ]
    added_rows <- new_nonresponsive[!new_nonresponsive$logger_serial_no %in% current_rows$logger_serial_no, ]
    nonresponsive_list$sheets_list[[manufacturer]]$data[[1]] <- all_rows_non_dup
    log_success("Added ", nrow(added_rows), " nonresponsive loggers to ", manufacturer, " sheet.")
    return(nonresponsive_list)
}

#' Get unimported metadata
#'
#' Checks a location to see if there is unimported metadata in the "not_processed" folder.
#' If found, returns a list of file paths to these unimported metadata files.
#' @param location Character string specifying the location (colony) to check for unprocessed metadata.
#'
#' @concept metadata
#' @export
get_location_unprocessed <- function(location) {
    if (is.null(the$sea_track_folder)) {
        stop("Sea track folder is not set. Please use set_sea_track_folder() to set it.")
    }
    locations_path <- file.path(the$sea_track_folder, "Locations")
    if (!dir.exists(locations_path)) {
        stop("Locations folder not found in the sea track folder.")
    }
    all_location_dirs <- list.dirs(locations_path, full.names = FALSE, recursive = TRUE)
    location_unprocessed_dir <- all_location_dirs[grepl("not_processed", tolower(all_location_dirs), fixed = TRUE) &
        grepl(location, all_location_dirs, fixed = TRUE)]
    if (length(location_unprocessed_dir) == 0) {
        log_info(paste("No 'not_processed' folder found for location:", location))
        return(NULL)
    }
    unprocessed_files_list <- lapply(location_unprocessed_dir, function(unprocessed_dir) {
        list.files(file.path(locations_path, unprocessed_dir), pattern = "^[^~].*\\.xlsx$", full.names = TRUE)
    })
    unprocessed_files <- unlist(unprocessed_files_list)
    if (length(unprocessed_files) == 0) {
        log_info(paste("No unprocessed files found for location:", location))
        return(NULL)
    }
    return(unprocessed_files)
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

#' Set a value in a specific cell of master startup
#'
#' This function updates the value of a specified cell in the `master_startup` data frame.
#'
#' @param master_startup Master starup tibble.
#' @param index Integer. The row index of the cell to update.
#' @param column Character or integer. The column name or index of the cell to update.
#' @param value The new value to assign to the specified cell.
#'
#' @return The updated `master_startup` data frame.
#' @examples
#' \dontrun{
#' set_master_startup_value(master_startup, 2, "download_type", "Succesfully downloaded")
#' }
#' @export
#' @concept startups
set_master_startup_value <- function(master_startup, index, column, value) {
    master_startup[index, column] <- value
    log_trace(paste0("Set value in master_startup: row ", index, ", column '", column, "' to '", value, "'"))
    return(master_startup)
}


#' Set or append comments in the master_startup data frame
#'
#' This function updates the 'comment' field of the specified row in the master_startup data frame.
#' If a non-empty logger comment is provided, it will be set as the comment if no existing comment is present.
#' If an existing comment is present, the logger comment will be appended to it, separated by " | ".
#'
#' @param master_startup A data frame containing a 'comment' column to be updated.
#' @param index Integer index specifying the row in master_startup to update.
#' @param logger_comments A character string containing the comment to add or append.
#'
#' @return The updated master_startup data frame with the modified comment.
#' @examples
#' \dontrun{
#' master_startup <- data.frame(comment = c("", "Existing comment"))
#' set_comments(master_startup, 1, "New logger comment")
#' set_comments(master_startup, 2, "Another logger comment")
#' }
#' @export
#' @concept startups
set_comments <- function(master_startup, index, logger_comments) {
    if (!is.na(logger_comments) && logger_comments != "") {
        # If there is a comment:
        if (is.na(master_startup$comment[index]) || master_startup$comment[index] == "") {
            # If there is no existing comment:
            master_startup$comment[index] <- logger_comments
        } else {
            # If there is an existing comment, append to it:
            master_startup$comment[index] <- paste(master_startup$comment[index], logger_comments, sep = " | ")
        }
    }
    return(master_startup)
}


#' Handle restarted loggers
#'
#' This function processes logger return information and updates the master import data frame accordingly.
#'
#' @param colony A character string specifying the name of the colony.
#' @param master_startup A data frame containing the master startup and shutdown information.
#' @param logger_returns A data frame containing logger return information.
#' @param restart_times A data frame containing logger restart information.
#' @param nonresponsive_list A list containing tibbles of unresponsive loggers for different manufacturers.
#' The name of the list element should match the producer name in master_startup (e.g., "Lotek", "MigrateTech").
#' @return A list consisting of two elements:
#'  - `master_startup``: An updated dataframe containing the modified master import data frame.
#'  - `nonresponsive_list`: An updated list containing the modified nonresponsive logger data frames.
#' @examples
#' \dontrun{
#' updated_master_startup <- handle_returned_loggers(master_startup, logger_returns, restart_times)
#' }
#' @export
#' @concept loggers
handle_returned_loggers <- function(colony, master_startup, logger_returns, restart_times, nonresponsive_list = list()) {
    if (nrow(logger_returns) == 0) {
        log_info("No logger returns to process.")
        return(list(master_startup = master_startup, nonresponsive_list = nonresponsive_list))
    }

    log_trace("Check returned loggers")
    valid_status <- logger_returns$status != "No download attemted"
    unhandled_loggers <- tibble()
    if (any(valid_status)) {
        all_updated_session_summary <- tibble()
        logger_indexes <- which(valid_status)
        for (i in logger_indexes) {
            logger_id <- logger_returns$logger_id[i]
            logger_status <- logger_returns$status[i]
            logger_download_stop_date <- logger_returns$`download / stop_date`[i]

            unfinished_session_result <- get_unfinished_session(master_startup, logger_id, logger_download_stop_date)
            if (is.null(unfinished_session_result)) {
                log_trace(paste("Skipping logger ID:", logger_id, "due to unresolved unfinished session. This may indicate an error or that this session has already been ended."))
                unhandled_loggers <- rbind(unhandled_loggers, logger_returns[i, ])
                next
            }
            unfinished_index <- unfinished_session_result$index
            unfinished_session <- unfinished_session_result$session

            master_startup <- set_master_startup_value(master_startup, unfinished_index, "download_type", logger_status)
            master_startup <- set_master_startup_value(master_startup, unfinished_index, "download_date", logger_download_stop_date)
            master_startup <- set_master_startup_value(master_startup, unfinished_index, "shutdown_date", logger_download_stop_date)
            master_startup <- set_master_startup_value(master_startup, unfinished_index, "downloaded_by", logger_returns$`downloaded by`[i])
            master_startup <- set_comments(master_startup, unfinished_index, logger_returns$comment[i])

            updated_session_summary <- master_startup[unfinished_index, c("logger_serial_no", "starttime_gmt", "download_type", "download_date")]
            all_updated_session_summary <- rbind(all_updated_session_summary, updated_session_summary)
        }
        log_success("Updated ", nrow(all_updated_session_summary), " sessions.")
        log_success("Updated sessions:\n", paste(capture.output(print(all_updated_session_summary, n = nrow(all_updated_session_summary)))[c(-1, -3)], collapse = "\n"))

        if (nrow(unhandled_loggers) > 0) {
            unhandled_loggers_summary <- unhandled_loggers[, c("logger_id", "status", "download / stop_date")]
            log_warn(nrow(unhandled_loggers_summary), " returns were not processed.")
            log_warn("Unhandled returns:\n", paste(capture.output(print(unhandled_loggers_summary, n = nrow(unhandled_loggers_summary)))[c(-1, -3)], collapse = "\n"))
        }
    }

    # Handle restarts
    log_trace("Handle restarts")
    restart_indexes <- which(logger_returns$`stored or sent to?` == "redeployed")
    if (length(restart_indexes) > 0) {
        added_sessions <- tibble()
        for (i in restart_indexes) {
            return_restart <- logger_returns[i, ]
            logger_id <- return_restart$logger_id
            downloader <- return_restart$`downloaded by`
            restart_info <- restart_times[restart_times$logger_id == logger_id, ]
            if (nrow(restart_info) == 0) {
                stop(paste("Logger ID:", logger_id, "not present in restart times sheet"))
            }
            logger_restart_datetime <- paste(restart_info$startdate_GMT, format(restart_info$starttime_GMT, "%H:%M:%S"))

            # Get full logger info from existing sheet
            previous_sessions <- master_startup[master_startup$logger_serial_no == logger_id, ]
            if (nrow(previous_sessions) == 0) {
                stop(paste("Logger ID:", logger_id, "not present in master startup sheet"))
            }
            # generate new row
            new_session <- tibble(
                logger_serial_no = logger_id,
                logger_model = previous_sessions$logger_model[1],
                producer = previous_sessions$producer[1],
                production_year = previous_sessions$production_year[1],
                project = previous_sessions$project[1],
                starttime_gmt = logger_restart_datetime,
                logging_mode = restart_info$`Logging mode`[1],
                started_by = downloader,
                started_where = colony,
                days_delayed = NA,
                programmed_gmt_time = NA,
                intended_species = restart_info$intended_species[1],
                intended_location = colony,
                intended_deployer = NA,
                shutdown_session = NA,
                field_status = NA,
                downloaded_by = NA,
                download_type = NA,
                download_date = NA,
                decomissioned = NA,
                shutdown_date = NA,
                comment = restart_info$comment[1],
            )
            added_sessions <- rbind(added_sessions, new_session)
        }
        log_success("Adding ", nrow(added_sessions), " new sessions from restarts.")
        added_sessions_summary <- added_sessions[, c("logger_serial_no", "logger_model", "production_year", "starttime_gmt", "intended_location")]
        log_success("New sessions:\n", paste(capture.output(print(added_sessions_summary, n = nrow(added_sessions_summary)))[c(-1, -3)], collapse = "\n"))

        master_startup <- rbind(master_startup, added_sessions)
    }

    # HANDLE UNRESPONSIVES
    log_trace("Handle nonresponsive loggers")

    nonresponsive_index <- which(logger_returns$`stored or sent to?` == "Nonresponsive")
    if (length(nonresponsive_index) > 0) {
        nonresponsive_returns <- logger_returns[nonresponsive_index, ]
        # Get manufacturers
        nonresponsive_returns$manufacturer <- master_startup$producer[match(nonresponsive_returns$logger_id, master_startup$logger_serial_no)]
        nonresponsive_returns$manufacturer_2 <- tolower(nonresponsive_returns$manufacturer)

        # biotrack loggers should go in the lotek sheet
        nonresponsive_returns$manufacturer_2[nonresponsive_returns$manufacturer_2 == "biotrack"] <- "lotek"

        for (manufacturer in tolower(nonresponsive_list$names())) {
            nonresponsive_for_manufacturer <- nonresponsive_returns[nonresponsive_returns$manufacturer_2 == manufacturer, ]

            if (nrow(nonresponsive_for_manufacturer) == 0) {
                next
            }

            current_startup <- master_startup[match(nonresponsive_for_manufacturer$logger_id, master_startup$logger_serial_no), ]

            new_nonresponsive <- tibble(
                logger_serial_no = nonresponsive_for_manufacturer$logger_id,
                logger_model = current_startup$logger_model,
                producer = current_startup$producer,
                production_year = current_startup$production_year,
                project = current_startup$project,
                starttime_gmt = current_startup$starttime_gmt,
                download_type = "Nonresponsive",
                download_date = nonresponsive_for_manufacturer$`download / stop_date`,
                comment = nonresponsive_for_manufacturer$comment,
                intended_species = current_startup$intended_species,
                intended_location = current_startup$intended_location,
                logging_mode = current_startup$logging_mode,
                days_delayed = current_startup$days_delayed,
                programmed_gmt_time = current_startup$programmed_gmt_time,
                sent = NA,
                priority = NA)


            nonresponsive_list <- append_to_nonresponsive(nonresponsive_list, new_nonresponsive, manufacturer)
        }
    }


    return(list(master_startup = master_startup, nonresponsive_list = nonresponsive_list))
}

#' Add partner provided metadata to the master import file
#'
#' This function adds metadata provided by partners to a master import file of the appropriate colony.
#' It firstly adds missing sessions by checking start up files.
#' It then appends the reported encounter data, avoiding duplicate rows.
#' Finally it updates sessions based on reported logger returns. This includes generating new sessions for loggers restarted in the field.
#'
#' @param colony A character string specifying the name of the colony.
#' @param new_metadata List of tibbles, each corresponding to a sheet in the partner provided information file.
#' @param master_import List of tibbles, each corresponding to a sheet in the master import file.
#' @param nonresponsive_list A named list of tibbles, each containing nonresponsive logger data for different manufacturers.
#'
#' @return An updated version of the master import file, as a list where each element is a sheet from the excel file.
#' @export
#' @concept metadata
handle_partner_metadata <- function(colony, new_metadata, master_import, nonresponsive_list = LoadedWBCollection$new()) {
    if (!all(c("ENCOUNTER DATA", "LOGGER RETURNS", "RESTART TIMES") %in% names(new_metadata$data))) {
        stop("new_metadata must contain the sheets: ENCOUNTER DATA, LOGGER RETURNS, RESTART TIMES")
    }
    if (!all(c("METADATA", "STARTUP_SHUTDOWN") %in% names(master_import$data))) {
        stop("master_import must contain the sheets: METADATA, STARTUP_SHUTDOWN")
    }

    log_info("Add missing sessions from start up files")
    updated_loggers <- add_loggers_from_startup(master_import$data$STARTUP_SHUTDOWN, new_metadata$data$`ENCOUNTER DATA`)

    master_import$data$`STARTUP_SHUTDOWN` <- updated_loggers

    log_info("Append encounter data")
    updated_metadata <- append_encounter_data(master_import$data$METADATA, new_metadata$data$`ENCOUNTER DATA`)

    master_import$data$METADATA <- updated_metadata

    log_info("Update sessions from logger returns")
    updated_sessions <- handle_returned_loggers(
        colony,
        master_import$data$`STARTUP_SHUTDOWN`,
        new_metadata$data$`LOGGER RETURNS`,
        new_metadata$data$`RESTART TIMES`,
        nonresponsive_list
    )

    master_import$data$`STARTUP_SHUTDOWN` <- updated_sessions$master_startup
    nonresponsive_list <- updated_sessions$nonresponsive_list

    return(list(master_import = master_import, nonresponsive_list = nonresponsive_list))
}


#' Save a master sheet to an Excel file.
#'
#' This function writes the provided data frame (`new_master_sheets`) to an Excel file
#' specified by `filename`.
#'
#' @param new_master_sheets A LoadedWB object.
#' @param filepath A string specifying the path and name of the Excel file to be created.
#'
#' @return No return value.
#'
#' @examples
#' \dontrun{
#' save_master_sheet(new_master_sheets, "output.xlsx")
#' }
#'
#' @export
#' @concept metadata
save_master_sheet <- function(new_master_sheets, filepath = NULL) {
    if (is.null(new_master_sheets)) {
        stop("new_master_sheets is null!")
    }
    if (is.null(filepath)){
        filepath = new_master_sheets$wb$path
    }

    for (sheet in names(new_master_sheets$data)){
        sheet_index <- which(names(new_master_sheets$data) == sheet)
        hidden_rows <- new_master_sheets$wb$worksheets[[sheet_index]]$sheet_data$row_attr$hidden
        new_master_sheets$wb$worksheets[[sheet_index]]$sheet_data$row_attr$hidden <- rep("", length(hidden_rows))

        new_master_sheets$wb$add_data(
            sheet = sheet,
            x = new_master_sheets$data[[sheet]],
            with_filter = TRUE,
            remove_cell_style = TRUE,
            na.strings = "")
        new_master_sheets$wb$freeze_pane(
            sheet = sheet,
            first_active_col = 5,
            first_active_row  = 2
            )
        new_master_sheets$wb$set_col_widths(sheet = sheet, cols = seq_len(ncol(new_master_sheets$data[[sheet]])), widths = "auto")
        new_master_sheets$wb$set_row_heights(sheet = sheet, rows = seq_len(nrow(new_master_sheets$data[[sheet]]) + 1), hidden = FALSE)
        wb_set_row_heights(new_master_sheets$wb, sheet = sheet, rows = seq_len(nrow(new_master_sheets$data[[sheet]]) + 1), hidden = FALSE)
        new_master_sheets$wb$remove_conditional_formatting(sheet = sheet)
        col_dims <- seq_len(ncol(new_master_sheets$data[[sheet]]))
        dims_mat_header <- openxlsx2::wb_dims(rows = 1, cols = col_dims)
        new_master_sheets$wb$add_fill(sheet, dims_mat_header, openxlsx2::wb_color(hex = "#ACB9CA"))


        if (sheet == "STARTUP_SHUTDOWN"){
            col_dims <- openxlsx2::wb_dims(
            x = new_master_sheets$data[[sheet]],
            cols = c("download_date", "shutdown_date"), select = "col_names")

            col_letters <- sapply(strsplit(col_dims, ","), function(x) gsub("[[:digit:]]+", "",x ))

            cf_formula <- sprintf('=OR($%s2<>"", $%s2<>"")', col_letters[1], col_letters[2])

            tryCatch({
                suppressWarnings(
                    new_master_sheets$wb$add_dxfs_style(
                        "seatrack_pos",
                        fontColour = openxlsx2::wb_color(hex = "#006100"),
                        bgFill = openxlsx2::wb_color("#C6EFCE")
                    )
                    )
            }, error = function(e) {
                invisible(NULL)
            })

            new_master_sheets$wb$add_conditional_formatting(
                sheet,
                dims = openxlsx2::wb_dims(x = new_master_sheets$data[[sheet]], select = "data"),
                rule = cf_formula,
                style = "seatrack_pos"
            )
        }
        # full_dims <- openxlsx2::wb_dims(x = new_master_sheets$data[[sheet]])
        # all_col_filters <- lapply(1:ncol(new_master_sheets$data[[sheet]]), function(i) {
        #     glue::glue('<filterColumn colId="{i}">
        #     </filterColumn>')
        # })
        # all_col_filters <- paste(all_col_filters, collapse = "\n")
        # autoFilter_value <- glue::glue('
        # <autoFilter ref="{full_dims}">
        # {all_col_filters}
        # </autoFilter>
        # ')
        # new_master_sheets$wb$worksheets[[which(names(new_master_sheets$data) == sheet)]]$autoFilter <- autoFilter_value
    }


    new_master_sheets$wb$save(filepath)
    # SET FORMATTING
}

#' Save multiple nonresponsive logger sheets to Excel files
#'
#' This function iterates through a list of nonresponsive logger sheets and a vector of file paths,
#' saving each sheet to its corresponding file path.
#' @param file_paths A character vector of file paths to save each sheet.
#' @param nonresponsive_list A named list of tibbles, each containing nonresponsive logger data.
#'
#' @return No return value.
#' @examples
#' \dontrun{
#' save_multiple_nonresponsive(nonresponsive_list, file_paths)
#' }
#' @export
#' @concept nonresponsive
save_nonresponsive <- function(file_paths, nonresponsive_list) {
    if (length(nonresponsive_list$sheets_list) != length(file_paths)) {
        stop("nonresponsive_list and file_paths must be the same length.")
    }
    for (i in seq_along(nonresponsive_list$sheets_list)) {
        openxlsx2::write_xlsx(nonresponsive_list$sheets_list[[i]]$data[[1]], file_paths[i], first_row = TRUE, first_active_col = 5, widths = "auto", na.strings = "")
        log_success("Saved nonresponsive sheet to: ", file_paths[i])
    }
}
