#' Get raw data from previous sessions
#'
#' This function gets any sessions occuring before the latest session of a seatrack GLS logger and
#' collects the associated raw data files into a zip file. The zip file also contains a CSV
#' with metadata about the previous sessions.
#'
#' This history is useful when a manufacturer is attempting to recover data from a nonresponsive logger.
#'
#' @param logger_serial_nos A character vector of logger serial numbers to get previous session data for.
#' @param zip_filename The name of the output zip file. Defaults to "raw_data_<current_date>.zip".
#' @param search_path The path to search for raw data files. If NULL, defaults to the "ALL" folder in the SeaTrack database imports directory.
#'
#' @return None, but exports a zip file containing the raw data files and a metadata CSV.
#' @export
#' @concept nonresponsive
get_raw_data_prev_session <- function(logger_serial_nos = c(), zip_filename = paste0("raw_data_", Sys.Date(), ".zip"), search_path = NULL) {
    logger_ids <- tibble(logger_serial_no = logger_serial_nos)

    db_logger_ids <- get_db_metadata_import(logger_ids, "loggers.logger_info", additional_db_col_names = c("logger_id"))

    # find the sessions associated with these logger_ids
    logger_info <- get_db_metadata_import(data.frame(logger_id = db_logger_ids$logger_id), "loggers.logger_info", additional_db_col_names = c("*"))

    sessions <- get_db_metadata_import(data.frame(logger_id = db_logger_ids$logger_id), "loggers.logging_session", additional_db_col_names = c("*"))

    previous_sessions <- sessions[!is.na(sessions$retrieval_id), ]
    previous_sessions$logger_serial_no <- db_logger_ids$logger_serial_no[match(previous_sessions$logger_id, db_logger_ids$logger_id)]

    retrievals <- get_db_metadata_import(data.frame(session_id = previous_sessions$session_id), "loggers.retrieval", additional_db_col_names = c("*"))
    deployments <- get_db_metadata_import(data.frame(session_id = previous_sessions$session_id), "loggers.deployment", additional_db_col_names = c("*"))

    startup <- get_db_metadata_import(data.frame(session_id = previous_sessions$session_id), "loggers.startup", additional_db_col_names = c("*"))


    previous_sessions$deployment_date <- deployments$deployment_date[match(previous_sessions$session_id, retrievals$session_id)]
    previous_sessions$retrieval_date <- retrievals$retrieval_date[match(previous_sessions$session_id, retrievals$session_id)]
    previous_sessions$starttime_gmt <- startup$starttime_gmt[match(previous_sessions$session_id, startup$session_id)]
    previous_sessions <- data.frame(previous_sessions, logger_info[match(previous_sessions$logger_id, retrievals$logger_id), ])


    filenames <- paste(previous_sessions$logger_serial_no, format(previous_sessions$retrieval_date, "%Y"), sep = "_")
    # filenames <- previous_sessions$session_id

    previous_sessions <- previous_sessions[, c("logger_serial_no", "logger_model", "production_year", "session_id", "starttime_gmt", "deployment_date", "retrieval_date")]
    if (is.null(search_path)) {
        search_path <- file.path(the$sea_track_folder, "Database\\Imports_Logger data\\Raw logger data\\ALL")
    }

    all_files <- list.files(search_path)
    matched_files <- lapply(filenames, function(x) {
        all_files[grepl(x, all_files)]
    })
    names(matched_files) <- filenames

    # Should try and find unmatched files on the FTP server.

    temp_dir <- tempfile()
    dir.create(temp_dir)

    for (i in seq_along(matched_files)) {
        subfolder <- file.path(temp_dir, names(matched_files)[i])
        dir.create(subfolder)
        files_to_copy <- matched_files[[i]]
        for (file in files_to_copy) {
            file.copy(file.path(search_path, file), subfolder)
        }
    }
    csv_path <- file.path(temp_dir, "metadata.csv")
    utils::write.csv(previous_sessions, csv_path, row.names = FALSE)

    zip::zip(zipfile = zip_filename, files = c(csv_path, list.dirs(temp_dir, recursive = FALSE, full.names = TRUE)), mode = "cherry-pick")
    unlink(temp_dir, recursive = TRUE)
}
