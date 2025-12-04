#' Check existing rows in database given certain columns from a metadata dataframe
#'
#' This function checks for existing rows in a database table based on specified columns from a metadata dataframe.
#' It constructs a SQL query to compare the values in the dataframe with those in the database table. IF a row from the dataframe
#' does not exist in the database table based on the specified columns, it is marked as missing.
#'
#' @param metadata_df A dataframe containing metadata to be checked against the database.
#' @param table The name of the database table to check against.
#' @param col_names A vector of column names from the metadata dataframe to be used for comparison. If NULL, all columns are used.
#' @param db_col_names A vector of column names in the database table corresponding to `col_names`. If NULL, `col_names` are used.
#' @return A logical vector indicating which rows in the metadata dataframe do not exist in the database table.
#' @export
#' @concept db_import_utility
check_db_metadata_import <- function(metadata_df, table, col_names = NULL, db_col_names = NULL) {
    existing_rows <- get_db_metadata_import(metadata_df, table, col_names, db_col_names)
    log_trace("Check existing rows in database using columns: ", paste(col_names, collapse = ", "), "... ", nrow(existing_rows), " existing rows found")

    existing_rows_combined <- sapply(seq_len(nrow(existing_rows)), function(row_idx) {
        paste(existing_rows[row_idx, ], collapse = " ")
    })
    metadata_df_combined <- sapply(seq_len(nrow(metadata_df)), function(row_idx) {
        paste(metadata_df[row_idx, ], collapse = " ")
    })
    missing_row_bool <- !metadata_df_combined %in% existing_rows_combined
    log_trace(sum(missing_row_bool), " rows are not in the database")

    return(missing_row_bool)
}

get_db_metadata_import <- function(metadata_df, table, col_names = NULL, db_col_names = NULL, additional_db_col_names = c()) {
    metadata_df <- tibble(metadata_df)
    if (is.null(col_names)) {
        col_names <- names(metadata_df)
    }
    if (is.null(db_col_names)) {
        db_col_names <- col_names
    }
    if (length(db_col_names) != length(col_names)) {
        stop("`db_col_names`` must be equal in length to `col_names`")
    }
    # given a table
    res <- DBI::dbSendQuery(con, glue::glue("SELECT {paste(db_col_names, collapse = ',')} FROM {table} LIMIT 1"))
    column_info <- DBI::dbColumnInfo(res)
    DBI::dbClearResult(res)

    # check certain columns
    db_cols <- metadata_df[, col_names]

    temp_values <- seatrackR::R_df_to_db_values(db_cols, column_info$type)

    t_db_names <- paste0("t.", db_col_names)
    if (length(additional_db_col_names) > 0) {
        t_additional_db_names <- paste0("t.", additional_db_col_names)
    } else {
        t_additional_db_names <- c()
    }

    t_db_names_combined <- paste(c(t_db_names, t_additional_db_names), collapse = ",\n")
    mv_db_names <- paste0("mv.", db_col_names)
    mv_db_names_combined <- paste(mv_db_names, collapse = ",\n")

    match_names <- lapply(seq_along(t_db_names), function(name_idx) {
        glue::glue("({t_db_names[name_idx]} IS NOT DISTINCT FROM {mv_db_names[name_idx]})")
    })
    combined_match_names <- paste(match_names, collapse = " AND \n")

    combined_db_names <- paste(db_col_names, collapse = ", ")


    query <- glue::glue("
    WITH match_values ({combined_db_names}) AS (
        VALUES
            { temp_values }
    )
    SELECT
        {t_db_names_combined}
    FROM
        {table} t
        JOIN match_values mv ON
        {combined_match_names}
        ;
    ")

    existing_rows <- DBI::dbGetQuery(con, query)
    return(existing_rows)
}

#' Check existing startups in database
#'
#' Function to check which startups from the master import sheet are not already present in the database.
#' It constructs a dataframe of session IDs and start times, and uses the `check_db_metadata_import` function
#' to determine which startups are new and need to be added to the database.
#' @param startup_shutdown A tibble containing session information from master import startup_shutdown.
#' @return A tibble of startups that are not already present in the database.
#' @concept db_import_utility
#' @export
check_startups_db <- function(startup_shutdown) {
    if (nrow(startup_shutdown) == 0) {
        return(startup_shutdown)
    }
    db_startup_df <- data.frame(
        session_id = paste(startup_shutdown$logger_serial_no, as.Date(startup_shutdown$starttime_gmt), sep = "_"),
        starttime_gmt = startup_shutdown$starttime_gmt
    )

    sessions_to_startup_bool <- check_db_metadata_import(db_startup_df, "loggers.startup")
    startup_new <- startup_shutdown[sessions_to_startup_bool, ]
    return(startup_new)
}

#' Check existing shutdowns in database
#'
#' Function to check which shutdowns from the master import sheet are not already present in the database.
#' It constructs a dataframe of session IDs and download dates, and uses the `check_db_metadata_import` function
#' to determine which shutdowns are new and need to be added to the database.
#' @param startup_shutdown A tibble containing session information from master import startup_shutdown.
#' @return A tibble of shutdowns that are not already present in the database.
#' @concept db_import_utility
#' @export
check_shutdown_db <- function(startup_shutdown) {
    if (nrow(startup_shutdown) == 0) {
        return(startup_shutdown)
    }
    shutdown <- startup_shutdown[!is.na(startup_shutdown$download_date) | !is.na(startup_shutdown$shutdown_date), ]
    db_shutdown_df <-
        data.frame(
            session_id = paste(shutdown$logger_serial_no, as.Date(shutdown$starttime_gmt), sep = "_"),
            download_date = shutdown$download_date
        )

    db_shutdown_df$download_date[is.na(db_shutdown_df$download_date)] <- shutdown$shutdown_date[is.na(db_shutdown_df$download_date)]

    sessions_to_shutdown_bool <- check_db_metadata_import(db_shutdown_df, "loggers.shutdown")
    shutdown_new <- shutdown[sessions_to_shutdown_bool, ]
    return(shutdown_new)
}

#' Check existing retrievals in database
#'
#' Function to check which retrievals from the master import sheet are not already present in the database.
#' It constructs a dataframe of retrieval dates and individ IDs, and uses the `check_db_metadata_import` function
#' to determine which retrievals are new and need to be added to the database.
#' @param retrievals A tibble containing retrieval information from master import metadata.
#' @return A tibble of retrievals that are not already present in the database.
#' @concept db_import_utility
#' @export
check_retrieval_db <- function(retrievals) {
    if (nrow(retrievals) == 0) {
        return(retrievals)
    }
    db_retrievals_df <- tibble(
        retrieval_date = retrievals$date,
        individ_id = paste(retrievals$euring_code, retrievals$ring_number, sep = "_")
    )
    new_rows_bool <- check_db_metadata_import(db_retrievals_df, "loggers.retrieval")
    new_retrievals <- retrievals[new_rows_bool, ]
    return(new_retrievals)
}

#' Check existing deployments in database
#'
#' Function to check which deployments from the master import sheet are not already present in the database.
#' It constructs a dataframe of deployment dates and individ IDs, and uses the `check_db_metadata_import` function
#' to determine which deployments are new and need to be added to the database.
#' @param deployments A tibble containing deployment information from master import metadata.
#' @return A tibble of deployments that are not already present in the database.
#' @concept db_import_utility
#' @export
check_deployment_db <- function(deployments) {
    if (nrow(deployments) == 0) {
        return(deployments)
    }
    db_deployments_df <- tibble(
        deployment_date = deployments$date,
        individ_id = paste(deployments$euring_code, deployments$ring_number, sep = "_")
    )
    new_rows_bool <- check_db_metadata_import(db_deployments_df, "loggers.deployment")
    new_deployments <- deployments[new_rows_bool, ]
    return(new_deployments)
}
