push_master_import_file_to_db <- function(location) {
    master_sheets <- load_master_import(location)
    push_master_sheet_to_db(master_sheets)
}

push_master_sheet_to_db <- function(master_sheets) {
    import_fields <- DBI::dbListFields(con, DBI::Id(schema = "imports", table = "metadata_import"))

    metadata <- master_sheets$data$METADATA

    # retrieval - METADATA
    retrievals <- metadata[!is.na(metadata$logger_id_retrieved), ]
    # check if any retrievals occurred after a deployment from the same year (e.g animal found dead or retrieved by mistake)


    db_retrievals_df <- tibble(
        retrieval_date = retrievals$date,
        individ_id = paste(retrievals$euring_code, retrievals$ring_number, sep = "_")
    )
    new_rows_bool <- check_db_metadata_import(db_retrievals_df, "loggers.retrieval")
    new_retrievals <- retrievals[new_rows_bool, ]
    # As we are handling retrievals and deployments seperately, remove deployment information
    new_retrievals$logger_model_deployed <- NA
    new_retrievals$logger_id_deployed <- NA



    # shutdown - STARTUP_SHUTDOWN
    # starup - STARTUP_SHUTDOWN
    # deployment - METADATA
    deployments <- metadata[!is.na(metadata$logger_id_deployed), ]
    db_deployments_df <- tibble(
        deployment_date = deployments$date,
        individ_id = paste(deployments$euring_code, deployments$ring_number, sep = "_")
    )
    new_rows_bool <- check_db_metadata_import(db_deployments_df, "loggers.deployment")
    new_deployments <- deployments[new_rows_bool, ]
    # As we are handling retrievals and deployments seperately, remove deployment information
    new_retrievals$logger_model_deployed <- NA
    new_retrievals$logger_id_deployed <- NA
    # retrieval in same year


    # shutdown downloaded/not used
}

R_value_to_db_value <- function(r_value) {
    if (class(r_value) == "Date") {
        db_value <- glue::glue("DATE '{format(r_value)}'")
    } else if (is.na(r_value)) {
        db_value <- glue::glue("NULL")
    } else if (class(r_value) == "character") {
        db_value <- glue::glue("'{r_value}'")
    } else {
        db_value <- r_value
    }
    return(db_value)
}

R_df_to_db_values <- function(db_cols) {
    db_values <- lapply(seq_len(nrow(db_cols)), function(row_idx) {
        curr_row <- db_cols[row_idx, ]
        db_strings <- sapply(names(db_cols), function(col_name) R_to_db(curr_row[[col_name]]))
        db_row <- paste(db_strings, collapse = ", ")
        db_row <- paste0("(", db_row, ")")
    })
    combined_db_values <- paste(db_values, collapse = ", \n")
    return(combined_db_values)
}

check_db_metadata_import <- function(metadata_df, table, col_names = NULL, db_col_names = NULL) {
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
    # check certain columns
    db_cols <- metadata_df[, col_names]

    temp_values <- R_df_to_db_values(db_cols)

    t_db_names <- paste0("t.", db_col_names)
    t_db_names_combined <- paste(t_db_names, collapse = ",\n")
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
    log_trace("Check existing rows in database using columns: ", paste(col_names, collapse = ", "), "... ")

    existing_rows <- DBI::dbGetQuery(con, query)
    log_trace("Check existing rows in database using columns: ", paste(col_names, collapse = ", "), "... ", nrow(existing_rows), " existing rows found")

    existing_rows_combined <- sapply(seq_len(nrow(existing_rows)), function(row_idx) {
        paste(existing_rows[row_idx, ], collapse = " ")
    })
    metadata_df_combined <- sapply(seq_len(nrow(metadata_df)), function(row_idx) {
        paste(metadata_df[row_idx, ], collapse = " ")
    })
    missing_row_bool <- !metadata_df_combined %in% existing_rows_combined
    log_trace(nrow(existing_rows), " rows are not in the database")

    return(missing_row_bool)
}

push_master_import_file_to_db <- function(location) {
    master_sheets <- load_master_import(location)
    push_master_sheet_to_db(master_sheets)
}

push_master_sheet_to_db <- function(master_sheets) {
    import_fields <- DBI::dbListFields(con, DBI::Id(schema = "imports", table = "metadata_import"))

    metadata <- master_sheets$data$METADATA

    # retrieval - METADATA
    retrievals <- metadata[!is.na(metadata$logger_id_retrieved), ]
    # check if any retrievals occurred after a deployment from the same year (e.g animal found dead or retrieved by mistake)


    db_retrievals_df <- tibble(
        retrieval_date = retrievals$date,
        individ_id = paste(retrievals$euring_code, retrievals$ring_number, sep = "_")
    )
    new_rows_bool <- check_db_metadata_import(db_retrievals_df, "loggers.retrieval")
    new_retrievals <- retrievals[new_rows_bool, ]
    # As we are handling retrievals and deployments seperately, remove deployment information
    new_retrievals$logger_model_deployed <- NA
    new_retrievals$logger_id_deployed <- NA




    # shutdown - STARTUP_SHUTDOWN
    # starup - STARTUP_SHUTDOWN
    # deployment - METADATA
    deployments <- metadata[!is.na(metadata$logger_id_deployed), ]
    db_deployments_df <- tibble(
        deployment_date = deployments$date,
        individ_id = paste(deployments$euring_code, deployments$ring_number, sep = "_")
    )
    new_rows_bool <- check_db_metadata_import(db_deployments_df, "loggers.deployment")
    new_deployments <- deployments[new_rows_bool, ]
    # As we are handling retrievals and deployments seperately, remove deployment information
    new_retrievals$logger_model_deployed <- NA
    new_retrievals$logger_id_deployed <- NA
    # retrieval in same year


    # shutdown downloaded/not used
}

R_value_to_db_value <- function(r_value) {
    if (class(r_value) == "Date") {
        db_value <- glue::glue("DATE '{format(r_value)}'")
    } else if (is.na(r_value)) {
        db_value <- glue::glue("NULL")
    } else if (class(r_value) == "character") {
        db_value <- glue::glue("'{r_value}'")
    } else {
        db_value <- r_value
    }
    return(db_value)
}

R_df_to_db_values <- function(db_cols) {
    db_values <- lapply(seq_len(nrow(db_cols)), function(row_idx) {
        curr_row <- db_cols[row_idx, ]
        db_strings <- sapply(names(db_cols), function(col_name) R_to_db(curr_row[[col_name]]))
        db_row <- paste(db_strings, collapse = ", ")
        db_row <- paste0("(", db_row, ")")
    })
    combined_db_values <- paste(db_values, collapse = ", \n")
    return(combined_db_values)
}

check_db_metadata_import <- function(metadata_df, table, col_names = NULL, db_col_names = NULL) {
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
    # check certain columns
    db_cols <- metadata_df[, col_names]

    temp_values <- R_df_to_db_values(db_cols)

    t_db_names <- paste0("t.", db_col_names)
    t_db_names_combined <- paste(t_db_names, collapse = ",\n")
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
    log_trace("Check existing rows in database using columns: ", paste(col_names, collapse = ", "), "... ")

    existing_rows <- DBI::dbGetQuery(con, query)
    log_trace("Check existing rows in database using columns: ", paste(col_names, collapse = ", "), "... ", nrow(existing_rows), " existing rows found")

    existing_rows_combined <- sapply(seq_len(nrow(existing_rows)), function(row_idx) {
        paste(existing_rows[row_idx, ], collapse = " ")
    })
    metadata_df_combined <- sapply(seq_len(nrow(metadata_df)), function(row_idx) {
        paste(metadata_df[row_idx, ], collapse = " ")
    })
    missing_row_bool <- !metadata_df_combined %in% existing_rows_combined
    log_trace(nrow(existing_rows), " rows are not in the database")

    return(missing_row_bool)
}
