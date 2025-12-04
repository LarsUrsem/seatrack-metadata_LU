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
gls_seatrack_calibration <- function(metadata_path, split_years = "06-01") {
    metadata <- openxlsx2::read_xlsx(metadata_path)
    metadata <- metadata[!is.na(metadata$logger_id), ]

    cols_to_get <- c("logger_id", "logger_model", "species", "date_deployed", "date_retrieved", "colony", "sun_angle_start", "sun_angle_end", "light_threshold", "analyzer", "year_tracked")
    if (!any(cols_to_get %in% names(metadata))) {
        log_error("Calibration data missing required columns.")
        return(NULL)
    }

    calibration_data <- metadata[, cols_to_get]

    extra_metadata <- metadata[!is.na(metadata$logger_id) & !duplicated(metadata$logger_id), c("logger_id", "date_retrieved", "date_deployed", "logger_producer", "ring_number", "country_code", "data_responsible", "age_deployed")]

    calibration_data$logger_id <- as.character(calibration_data$logger_id)
    calibration_data$logger_model <- as.character(calibration_data$logger_model)
    calibration_data$sun_angle_start <- as.numeric(calibration_data$sun_angle_start)
    calibration_data$sun_angle_end <- as.numeric(calibration_data$sun_angle_end)
    calibration_data$light_threshold <- as.numeric(calibration_data$light_threshold)
    calibration_data$date_deployed <- as.Date(calibration_data$date_deployed)
    calibration_data$date_retrieved <- as.Date(calibration_data$date_retrieved)

    extra_metadata$logger_id <- as.character(extra_metadata$logger_id)
    extra_metadata$age_deployed <- as.character(extra_metadata$age_deployed)
    extra_metadata$date_deployed <- as.Date(extra_metadata$date_deployed)
    extra_metadata$date_retrieved <- as.Date(extra_metadata$date_retrieved)
    extra_metadata$ring_number <- as.character(extra_metadata$ring_number)

    deployment_year <- as.numeric(format(calibration_data$date_deployed, "%Y"))
    retrieval_year <- as.numeric(format(calibration_data$date_retrieved, "%Y"))
    current_year_tracked <- metadata$year_tracked


    calibration_data$total_years_tracked <- paste(deployment_year, retrieval_year, sep = "_")

    # split by deployment/retrieval dates
    id_year <- paste(calibration_data$logger_id, calibration_data$date_deployed, calibration_data$date_retrieved)
    all_time_windows <- lapply(unique(id_year), function(idy) {
        logger_calibration <- calibration_data[id_year == idy, ]
        logger_deployment_year <- as.numeric(format(logger_calibration$date_deployed, "%Y"))[1]
        logger_retrieval_year <- as.numeric(format(logger_calibration$date_retrieved, "%Y"))[1]

        current_years <- as.numeric(sapply(logger_calibration$year_tracked, function(x) {
            strsplit(as.character(x), "_")[[1]][1]
        }))
        all_years <- seq(logger_deployment_year, logger_retrieval_year)
        if (length(all_years) > 1) {
            all_years <- all_years[1:(length(all_years) - 1)]
        }
        missing_years <- all_years[!all_years %in% current_years]
        if (length(missing_years) > 0) {
            template_row <- logger_calibration[1, , drop = FALSE]
            new_rows <- do.call(rbind, lapply(missing_years, function(y) {
                r <- template_row

                if ("sun_angle_start" %in% names(r)) r$sun_angle_start <- NA
                if ("sun_angle_end" %in% names(r)) r$sun_angle_end <- NA
                if ("light_threshold" %in% names(r)) r$light_threshold <- NA
                if ("year_tracked" %in% names(r)) r$year_tracked <- as.character(y)
                if ("total_years_tracked" %in% names(r)) r$total_years_tracked <- paste(logger_deployment_year, logger_retrieval_year, sep = "_")
                if ("year_tracked" %in% names(r)) r$year_tracked <- paste(y, (y + 1) - 200, sep = "_")
                return(r)
            }))
            logger_calibration <- rbind(logger_calibration, new_rows)
            current_years <- as.numeric(sapply(logger_calibration$year_tracked, function(x) {
                strsplit(as.character(x), "_")[[1]][1]
            }))
            logger_calibration <- logger_calibration[order(current_years), ]
        }

        time_windows <- seatrackRgls:::get_calibration_splits(logger_calibration, split_years)
        if (nrow(time_windows) < nrow(logger_calibration)) {
            logger_calibration <- logger_calibration[1:nrow(time_windows), ]
        }

        new_logger_calibration <- data.frame(
            logger_calibration[, c("logger_id", "logger_model")],
            time_windows,
            logger_calibration[, !names(logger_calibration) %in% c("logger_id", "logger_model", "date_deployed", "date_retrieved")]
        )

        return(new_logger_calibration)
    })
    calibration_data <- do.call(rbind, all_time_windows)
    calibration_data$year_tracked <- paste(format(calibration_data$start_datetime, "%Y"), format(calibration_data$end_datetime, "%Y"), sep = "_")
    calibration_data$noon_filter <- TRUE
    calibration_data$daylength_filter <- TRUE
    calibration_data$midnightsun_removal <- TRUE

    return(list(calibration_data = calibration_data, extra_metadata = extra_metadata))
}

gls_get_existing_calibration <- function(existing_calibration_dir = file.path(the$sea_track_folder, "Locations")) {
    if (!dir.exists(existing_calibration_dir)) {
        stop("Could not find existing calibration directory: ", existing_calibration_dir)
    }

    metadata_files <- list.files(existing_calibration_dir, pattern = "^metadata_new\\.xlsx$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)

    if (length(metadata_files) == 0) {
        return(list(calibration_data = data.frame(), extra_metadata = data.frame()))
    }

    results <- lapply(metadata_files, function(f) {
        tryCatch(
            gls_seatrack_calibration(f),
            error = function(e) {
                warning(sprintf("Failed to process '%s': %s", f, e$message))
                NULL
            }
        )
    })
    results <- Filter(Negate(is.null), results)

    if (length(results) == 0) {
        return(list(calibration_data = data.frame(), extra_metadata = data.frame()))
    }


    calibration_data_all <- do.call(dplyr::bind_rows, lapply(results, function(x) x$calibration_data))
    extra_metadata_all <- do.call(dplyr::bind_rows, lapply(results, function(x) x$extra_metadata))

    return(list(calibration_data = calibration_data_all, extra_metadata = extra_metadata_all))
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
    all_files <- list.files(import_directory, pattern = "*.lux|*.lig", recursive = TRUE, full.names = TRUE)
    all_files_split <- strsplit(basename(all_files), "_")
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
        date_deployed = deployment_date,
        date_retrieved = retrieval_date,
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
#' @param existing_calibration_dir Directory containing existing calibration data. Default is NULL. If provided, loggers already present in this directory will be excluded from the new calibration data.
#' @param include_existing Logical indicating whether to include existing calibration data in the final output. Default is TRUE.
#' @return None. The function saves the prepared calibration data to the specified output directory.
#' @export
#' @concept gls_helper
gls_prepare_calibration <- function(import_directory, output_directory, no_pos_only = TRUE, existing_calibration_dir = NULL, include_existing = TRUE) {
    metadata <- gls_metadata(import_directory, no_pos_only)
    if (!is.null(existing_calibration_dir)) {
        # Load existing calibration data if available and merge
        existing_calibration_file <- gls_get_existing_calibration(existing_calibration_dir)
        existing_calibration_data <- existing_calibration_file$calibration_data

        metadata_id_year <- paste(metadata$logger_id, format(metadata$date_deployed, "%Y"))
        existing_id_year <- paste(existing_calibration_data$logger_id, sapply(strsplit(existing_calibration_data$total_years_tracked, "_"), function(x) x[1]))
        # Remove existing calibration rows from metadata
        metadata <- metadata[!metadata_id_year %in% existing_id_year, ]
    }

    all_colony_info <- gls_seatrack_colony_info()
    calibration_template <- seatrackRgls::prepare_calibration(
        import_directory,
        data.frame(metadata[1,]),
        all_colony_info,
        output_directory,
        FALSE
    )
    if (include_existing) {
        calibration_template <- dplyr::bind_rows(existing_calibration_data, calibration_template)
    }

    calibration_output_dir <- file.path(output_directory, "calibration_data")
    seatrackRgls::calibration_to_wb(
        calibration_template,
        calibration_output_dir
    )
}

#' Process GLS position data using seatrackRgls
#'
#' Function to process GLS position data from a specified directory using seatrackRgls. 
#' The only additional functionality is fetching colony information from the Sea Track database.
#' @param import_directory Path to the directory containing GLS files.
#' @param calibration_data Dataframe containing GLS calibration data.
#' @param output_directory Path to the directory where the processed position data will be saved.
#' @return None. The function saves the processed position data to the specified output directory.
#' @export
#' @concept gls_helper
gls_process_positions <- function(import_directory, calibration_data, output_directory){
    all_colony_info <- gls_seatrack_colony_info()
    process_positions(
        import_directory,
        calibration_data,
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
    pos_list <- lapply(all_file_paths, gls_prepare_file_upload)
    all_pos <- do.call(rbind, pos_list)
    return(all_pos)
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
