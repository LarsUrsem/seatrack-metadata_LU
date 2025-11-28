#' Get `all_colony_info` for GLS processing
#'
#' Simple function to get seatrack colonies from database and shape the dataframe for immediate use in seatrackRgls
#'
#' @export
#' @concept gls_helper
gls_seatrack_colony_info <- function() {
    all_colony_info <- seatrackR::getColonies()
    all_colony_info <- data.frame(colony = all_colony_info$colony_int_name, col_lat = all_colony_info$lat, col_lon = all_colony_info$lon)
    return(all_colony_info)
}

#' Handle existing GLS calibration data
#'
#' Function to reshape existing seatrack GLS calibration settings for immediate use in seatrackRgls
#'
#' @param metadata_path Path to metadata Excel file
#' @return A list with two dataframes: calibration_data and extra_metadata
#' @export
#' @concept gls_helper
gls_seatrack_calibration <- function(metadata_path) {
    metadata <- openxlsx2::read_xlsx(metadata_path)

    cols_to_get <- c("logger_id", "logger_model", "species", "date_deployed", "date_retrieved", "colony", "sun_angle_start", "sun_angle_end", "light_threshold", "analyzer")

    calibration_data <- metadata[, cols_to_get]
    extra_metadata <- metadata[!is.na(metadata$logger_id) & !duplicated(metadata$logger_id), c("logger_id", "date_retrieved", "date_deployed", "logger_producer", "ring_number", "country_code", "data_responsible", "age_deployed")]
    return(list(calibration_data = new_calibration_data, extra_metadata = extra_metadata))
}

#' Get metadata from database based on GLS files in import directory
#'
#' Function to scan a directory for GLS files and retrieve corresponding metadata from the Sea Track database.
#' @param import_directory Path to the directory containing GLS files.
#' @param no_pos_only Logical indicating whether to include only loggers without position data in the database. Default is TRUE.
#' @return A dataframe containing metadata for the GLS loggers found in the import directory.
#' @export
#' @concept gls_helper
gls_metadata <- function(import_directory, no_pos_only = TRUE) {
    print("Scan import directory for files...")
    all_files <- list.files(import_directory, pattern = "*.lux|*.lig")
    all_files_split <- strsplit(all_files, "_")
    file_info_list <- lapply(all_files_split, function(x) {
        data.frame(logger_id = x[1], year_downloaded = x[2], id_year = paste(x[1], x[2], sep = "_"))
    })
    file_info <- do.call(rbind, file_info_list)
    file_info <- data.frame(filename = all_files, file_info)

    db_info <- seatrackR::getSessionInfo(posdata_filename = file_info$id_year, has_pos_data = !no_pos_only, logger_download_type = "Successfully downloaded")

    # logger_id, logger_model, species, date_deployed, date_retrieved, colony
    metadata <- dplyr::select(
        db_info,
        logger_id = logger_serial_no,
        logger_model,
        species,
        deployment_date,
        retrieval_date,
        colony
    )
    return(metadata)
}

#' Prepare GLS calibration data using seatrack database
#'
#' Function to prepare GLS calibration data for use with seatrackRgls, based on GLS files in the import directory and metadata from the Sea Track database.
#' @param import_directory Path to the directory containing GLS files.
#' @param output_directory Path to the directory where the prepared calibration data will be saved.
#' @param no_pos_only Logical indicating whether to include only loggers without position data in the database. Default is TRUE.
#' @return None. The function saves the prepared calibration data to the specified output directory.
#' @export
#' @concept gls_helper
gls_prepare_calibration <- function(import_directory, output_directory, no_pos_only = TRUE) {
    metadata <- gls_metadata(import_directory, no_pos_only)
    all_colony_info <- gls_seatrack_colony_info()
    calibration_template <- seatrackRgls::prepare_calibration(
        import_directory,
        metadata,
        all_colony_info,
        output_directory
    )
}

#' Prepare GLS position data for database upload
#'
#' Function to prepare GLS position data from a specified directory for upload to the Sea Track database.
#' @param gls_directory_path Path to the directory containing GLS position data files.
#' @return A dataframe containing the prepared GLS position data ready for database upload.
#' @export
#' @concept gls_helper
gls_prepare_folder_upload <- function(gls_directory_path) {
    all_files <- list.files(gls_directory_path, pattern = "*.csv")
    all_file_paths <- file.path(gls_directory_path, all_files)
    pos_list <- all_pos_data <- lapply(all_file_paths, gls_prepare_file_upload)
    all_pos <- do.call(rbind, pos_list)
}

#' Prepare a single GLS position data file for database upload
#'
#' Function to prepare a single GLS position data file for upload to the Sea Track database.
#' @param gls_file_path Path to the GLS position data file.
#' @return A dataframe containing the prepared GLS position data ready for database upload.
#' @export
#' @concept gls_helper
gls_prepare_file_upload <- function(gls_file_path) {
    filename <- basename(gls_file_path)
    filename_only <- tools::file_path_sans_ext(filename)
    id_year <- strsplit(filename_only, "_-_")[[1]][2]
    gls_pos_data <- read.csv(gls_file_path)
    db_info <- seatrackR::getSessionInfo(posdata_filename = id_year)
    gls_pos_data$session_id <- db_info$session_id
    gls_pos_data$data_version <- 3
    db_template <- dplyr::select(
        gls_pos_data,
        date_time,
        year_tracked,
        session_id,
        lon_raw,
        lat_raw,
        lon,
        lat,
        eqfilter,
        tfirst = tFirst,
        tsecond = tSecond,
        twl_type = type,
        sun = sun_angle,
        light_threshold,
        analyzer,
        data_version
    )
    return(db_template)
}
