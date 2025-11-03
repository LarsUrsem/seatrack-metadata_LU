
#' Prepare master sheet for database import
#'
#' This function prepares the master import sheet for database import by processing the metadata and startup/shutdown information.
#' It checks the database for existing sessions and categorizes them into sessions to be closed, opened, or both opened and closed.
#' It then prepares session batches for database import. Metadata is linked to sessions within these batches.
#' @param master_sheets A LoadedWBCollection object containing the master import sheets.
#' @return A vector of DBImportCollection objects representing the prepared session batches for database import.
#' @export
#' @concept db_import_prep
prepare_master_sheet_for_db <- function(master_sheets) {
    
    metadata <- master_sheets$data$METADATA
    metadata <- metadata[order(metadata$date), ]

    startup_shutdown <- master_sheets$data$STARTUP_SHUTDOWN
    startup_shutdown <- startup_shutdown[order(startup_shutdown$starttime_gmt), ]
    # combine download/shutdown
    startup_shutdown$download_date[is.na(startup_shutdown$download_date)] <- startup_shutdown$shutdown_date[is.na(startup_shutdown$download_date)]

    # check db for which sessions are already in the correct state
    shutdown_new <- check_shutdown_db(startup_shutdown)

    startup_new <- check_startups_db(startup_shutdown)

    # which of our closed sessions are open sessions in the database vs. not existing at all in the database?
    db_shutdown_starts_df <- 
    data.frame(
        session_id = paste(shutdown_new$logger_serial_no,as.Date(shutdown_new$starttime_gmt), sep ="_"),
        starttime_gmt = shutdown_new$starttime_gmt)
    closed_sessions_bool <- check_db_metadata_import(db_shutdown_starts_df, "loggers.startup")
    
    # rows of sessions that are currently open in the database and will only be closed
    db_to_close_only <- shutdown_new[!closed_sessions_bool,]
    log_info(nrow(db_to_close_only), " sessions to close")

    # rows of sessions that currently don't exist in the database and therefore need to be started first before being closed.
    db_to_open_and_close <- shutdown_new[closed_sessions_bool,]
    log_info(nrow(db_to_open_and_close), " sessions to open and close")

    # row of sessions that will be opened in the database and not closed
    db_to_open_only <- startup_new[is.na(startup_new$download_date), ]
    log_info(nrow(db_to_open_only), " sessions to open")

    #neither db_to_close or db_to_open_only should have any duplicates. Remove these and warn the user

    # THIS SHOULDN'T REALLY HAPPEN
    duplicate_to_close_loggers<-unique(db_to_close_only$logger_serial_no[duplicated(db_to_close_only$logger_serial_no)])
    if(length(duplicate_to_close_loggers) > 0){
        log_warn("The following logger serial numbers have multiple close entries in the import sheet that cannot be added to the database: ", 
        paste(duplicate_to_close_loggers, collapse = ", "))

        for(duplicate_logger in duplicate_to_close_loggers){
            duplicate_summary <- db_to_open_only[duplicate_to_close_loggers$logger_serial_no == duplicate_logger, c("logger_serial_no","starttime_gmt","download_date")]
            log_warn(duplicate_logger, ":\n", paste(capture.output(print(duplicate_summary, n = nrow(duplicate_summary)))[c(-1, -3)], collapse = "\n"))
        }

        db_to_close_only <- db_to_close_only[!db_to_close_only$logger_serial_no %in% duplicate_to_close_loggers, ]
        log_warn("These sessions will not be added to the database. Check if these sessions can be closed in the master import sheet.")
    }
    # WOULD SUGGEST MULTIPLE OPEN SESSIONS IN THE DB

    duplicate_to_open_loggers<-unique(db_to_open_only$logger_serial_no[duplicated(db_to_open_only$logger_serial_no)])
    if(length(duplicate_to_open_loggers) > 0){
        log_warn("The following logger serial numbers have multiple open startup entries in the import sheet that cannot be added to the database: ", 
        paste(duplicate_to_open_loggers, collapse = ", "))

        for(duplicate_logger in duplicate_to_open_loggers){
            duplicate_summary <- db_to_open_only[db_to_open_only$logger_serial_no == duplicate_logger, c("logger_serial_no","starttime_gmt","download_date")]
            log_warn(duplicate_logger, ":\n", paste(capture.output(print(duplicate_summary, n = nrow(duplicate_summary)))[c(-1, -3)], collapse = "\n"))
        }

        db_to_open_only <- db_to_open_only[!db_to_open_only$logger_serial_no %in% duplicate_to_open_loggers, ]

        log_warn("These sessions will not be added to the database. Check if these sessions can be closed in the master import sheet.")
    }

    # batch open closed sessions
    db_to_open_and_close_list <- list()
    db_to_open_and_close_list <- c(db_to_open_and_close_list, list(db_to_open_and_close))

    while(any(duplicated(db_to_open_and_close_list[[length(db_to_open_and_close_list)]]$logger_serial_no))){
        current_index <- length(db_to_open_and_close_list)
        current_df <- db_to_open_and_close_list[[length(db_to_open_and_close_list)]]
        first_df <- current_df[!duplicated(current_df$logger_serial_no), ]
        new_df <- current_df[duplicated(current_df$logger_serial_no), ]
        db_to_open_and_close_list[[current_index]] <- first_df
        db_to_open_and_close_list <- c(db_to_open_and_close_list, list(new_df))
    }

    db_to_open_and_close_sessions <- sapply(db_to_open_and_close_list, SessionBatch$new, type = "open_and_close")
    log_trace("Split sessions to open and close into ",length(db_to_open_and_close_sessions)," sessions.")

    session_batches <- c(SessionBatch$new(db_to_close_only, "close_only"), db_to_open_and_close_sessions, SessionBatch$new(db_to_open_only, "open_only"))
    session_retrieval_deployments <- prepare_session_batches(session_batches, metadata)
    return(session_retrieval_deployments)
}

#' Prepare session batches for database import
#'
#' This function prepares a list of session batches for database import by processing each batch and extracting relevant metadata.
#' It filters out any batches that do not contain sessions and returns a list of prepared session batches.
#' It adds deployments and retrievals associated with the sessions in each batch.
#' In cases where sessions have multiple deployments or retrievals, those sessions are removed from the batch and a warning is logged.
#' @param session_batches A vector of SessionBatch objects to be prepared for database import.
#' @param metadata A dataframe containing metadata information for the sessions.
#' @return A vector of DBImportCollection objects representing the prepared session batches for database import.
#' @export
#' @concept db_import_prep
prepare_session_batches <- function(session_batches, metadata){
    session_metadata_batches <- sapply(session_batches, prepare_session_batch, metadata = metadata)
    session_metadata_batches <- session_metadata_batches[which(!sapply(session_metadata_batches , is.null))]
    return(session_metadata_batches)    
}

#' Prepare session batch for database import
#'
#' This function prepares a session batch for database import by processing each batch and extracting relevant metadata.
#' It adds deployments and retrievals associated with the sessions in each batch.
#' In cases where sessions have multiple deployments or retrievals, those sessions are removed from the batch and a warning is logged.
#' @param session_batch A SessionBatch object to be prepared for database import.
#' @param metadata A dataframe containing metadata information for the sessions.
#' @return DBImportCollection object representing the prepared session batches for database import.
#' @export
#' @concept db_import_prep
prepare_session_batch <- function(session_batch, metadata){
    if(nrow(session_batch$sessions) == 0){
        log_info(glue::glue("No sessions available. Sessions would be {gsub('_',' ', session_batch$type)}"))
        return(NULL)
    }
    original_count <- nrow(session_batch$sessions)
    log_info(glue::glue("Prepare {original_count}. Sessions will be {gsub('_',' ', session_batch$type)}"))

    deployment_results <- prepare_session_deployments(session_batch, metadata)
    deployments <- deployment_results$deployments
    session_batch$sessions <- deployment_results$sessions

    retrieval_results <- prepare_session_retrievals(session_batch, metadata)
    retrievals <- retrieval_results$retrievals
    session_batch$sessions <- retrieval_results$sessions    

    # as we may have lost sessions, double check.
    deployment_results <- prepare_session_deployments(session_batch, metadata)
    deployments <- deployment_results$deployments
    session_batch$sessions <- deployment_results$sessions


    new_count <-nrow(session_batch$sessions)
    log_info(glue::glue("Removed {original_count - new_count} sessions. {new_count} sessions prepared.."))
    

    return(DBImportCollection$new(
        session_batch,
        retrievals,
        deployments
    ))

}

#' Get open session dates
#' Convenience function to get the open date for a set of logger sessions.
#' If the download date is available, use that. If not, use the shutdown date. If both exist, use the later date.
#' If neither are available, use a date far in the future to indicate the session is still open.
#' @param sessions A tibble containing session information from master import startup_shutdown.
#' @return A vector of POSIXct dates representing the open dates for the sessions.
#' @concept utility
get_open_session_dates <- function(sessions){
    session_batch_open_date <- sessions$download_date
    later_shutdown_bool <- (!is.na(sessions$shutdown_date)) & (!is.na(sessions$download_date)) & (sessions$shutdown_date > sessions$download_date)
    session_batch_open_date[later_shutdown_bool] <- sessions$shutdown_date[later_shutdown_bool]
    session_batch_open_date[is.na(session_batch_open_date)] <- sessions$shutdown_date[is.na(session_batch_open_date)]
    session_batch_open_date[is.na(session_batch_open_date)] <- sessions$starttime_gmt[is.na(session_batch_open_date)] + (100*60*60*24*365)
    return(session_batch_open_date)
}

#' Get session deployments
#' 
#' Function to get deployments associated with a set of logger sessions. The function will look for deployments that occurred between the session start time and the session end time (download/shutdown).
#' If the session is open (no download/shutdown date), the function will use a date far in the future to indicate the session is still open.
#' If there are ambiguities (multiple deployments for a single session), those sessions will be removed and a warning will be logged.
#' @param session_batch A SessionBatch object containing session information from master import startup_shutdown.
#' @param metadata A dataframe containing metadata information for the sessions.
#' @return A list containing a dataframe of deployments and a tibble of sessions.
#' @concept db_import_prep
#' @export 
prepare_session_deployments <- function(session_batch, metadata){

    sessions <- session_batch$sessions  

    session_batch_open_date <- get_open_session_dates(sessions)
    #foreach session batch
    # find the deployments and retrievals for those loggers that are associated with these sessions
    #deployments
    deployed_match_idx <- lapply(seq_len(nrow(sessions)), function(session_row_idx) {
        session_row <- sessions[session_row_idx, ]
        session_open_date <- session_batch_open_date[session_row_idx]
        match_idx <- which(metadata$date >= as.Date(session_row$starttime_gmt) &
        metadata$date < session_open_date&
        metadata$logger_id_deployed == session_row$logger_serial_no & !is.na(metadata$logger_id_deployed))
        return(match_idx)
    })

    n_matches <- sapply(deployed_match_idx, length)
    duplicate_deployments <- n_matches > 1
    if (any(duplicate_deployments)){
        log_warn(sum(duplicate_deployments), " sessions have multiple deployments within them")
        log_warn("These sessions will not be handled.")
        duplicate_idx <- which(duplicate_deployments)
        for(i in duplicate_idx){

            session_summary <- session_batch$sessions[i, c("logger_serial_no","starttime_gmt","download_date")]
            duplicate_logger <- session_summary$logger_serial_no
            log_warn(duplicate_logger, ":\n", paste(capture.output(print(session_summary, n = 1))[c(-1, -3)], collapse = "\n"))

            deployment_summary <- metadata[deployed_match_idx[[i]], c("date", "logger_id_retrieved", "logger_id_deployed", "comment")]
            log_warn("Deployments:\n", paste(capture.output(print(deployment_summary, n = nrow(deployment_summary)))[c(-1, -3)], collapse = "\n"))
            
            
        }
        
        deployed_match_idx <- deployed_match_idx[-c(duplicate_idx)]
        sessions <- sessions[-c(duplicate_idx), ]
    }

    deployments <- metadata[unlist(deployed_match_idx),]
    log_trace(glue::glue("Found {nrow(deployments)} associated with these sessions"))

    new_deployments <- check_deployment_db(deployments)
    # As we are handling retrievals and deployments seperately, remove deployment information
    new_deployments$logger_model_retrieved <- NA
    new_deployments$logger_id_retrieved <- NA
    log_trace(glue::glue("Found {nrow(new_deployments)} associated with these sessions and not present in database."))
    

    return(list(deployments = new_deployments, sessions = sessions))
}


#' Get session retrievals
#' 
#' Function to get retrievals associated with a set of logger sessions. The function will look for retrievals that occurred between the session start time and the session end time (download/shutdown).
#' If the session is open (no download/shutdown date), the function will use a date far in the future to indicate the session is still open.
#' If there are ambiguities (multiple retrievals for a single session), those sessions will be removed and a warning will be logged.
#' @param session_batch A SessionBatch object containing session information from master import startup_shutdown.
#' @param metadata A dataframe containing metadata information for the sessions.
#' @return A list containing a dataframe of retrievals and a tibble of sessions.
#' @concept db_import_prep
#' @export 
prepare_session_retrievals <- function(session_batch, metadata){

    sessions <- session_batch$sessions    

    session_batch_open_date <- get_open_session_dates(sessions)

    retrieved_match_idx <- lapply(seq_len(nrow(sessions)), function(session_row_idx) {
        session_row <- sessions[session_row_idx, ]
        session_open_date <- session_batch_open_date[session_row_idx]
        match_idx <- which(metadata$date > as.Date(session_row$starttime_gmt) &
        metadata$date <= session_open_date&
        metadata$logger_id_retrieved == session_row$logger_serial_no)
        return(match_idx)
    })

    n_matches <- sapply(retrieved_match_idx, length)
    duplicate_retrievals <- n_matches > 1
    if (any(duplicate_retrievals)){
        log_warn(sum(duplicate_retrievals), " sessions have multiple retrievals within them")
        duplicate_idx <- which(duplicate_retrievals)
        for(i in duplicate_idx){

            session_summary <- session_batch$sessions[i, c("logger_serial_no","starttime_gmt","download_date")]
            duplicate_logger <- session_summary$logger_serial_no
            log_warn(duplicate_logger, ":\n", paste(capture.output(print(session_summary, n = 1))[c(-1, -3)], collapse = "\n"))

            retrieval_summary <- metadata[retrieved_match_idx[[i]], c("date", "logger_id_retrieved", "logger_id_deployed", "comment")]
            log_warn("Retrievals:\n", paste(capture.output(print(retrieval_summary, n = nrow(retrieval_summary)))[c(-1, -3)], collapse = "\n"))
            log_warn("This session will not be handled.")
            
        }
        retrieved_match_idx <- retrieved_match_idx[-c(duplicate_idx)]
        sessions <- session_batch$sessions[-c(duplicate_idx), ]
    }

    retrievals <- metadata[unlist(retrieved_match_idx),]
    log_trace(glue::glue("Found {nrow(retrievals)} associated with these sessions"))

    new_retrievals <- check_retrieval_db(retrievals)
    # As we are handling retrievals and deployments seperately, remove deployment information
    new_retrievals$logger_model_deployed <- NA
    new_retrievals$logger_id_deployed <- NA
    log_trace(glue::glue("Found {nrow(new_retrievals)} associated with these sessions and not present in database."))

    return(list(retrievals = new_retrievals, sessions = sessions))

}