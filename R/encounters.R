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
