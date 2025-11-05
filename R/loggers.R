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
            sent = NA
        )

        nonresponsive_list <- append_to_nonresponsive(nonresponsive_list, new_nonresponsive, manufacturer)
    }
    master_sheet$modified <- TRUE
    return(list(master_sheet = master_sheet, nonresponsive_list = nonresponsive_list))
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
                priority = NA
            )


            nonresponsive_list <- append_to_nonresponsive(nonresponsive_list, new_nonresponsive, manufacturer)
        }
    }


    return(list(master_startup = master_startup, nonresponsive_list = nonresponsive_list))
}
