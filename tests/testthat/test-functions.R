# Test script for functions in R/functions.R

library(testthat)
library(openxlsx2)
library(tibble)

# Create a temporary directory for file-based tests
tmp_dir <- tempdir()

# Helper functions for test data
create_test_startup_sheet <- function() {
  tibble(
    logger_serial_no = "L1",
    logger_model = "birdTracker5000",
    producer = "Lotek",
    production_year = 2024,
    project = "seatrack",
    starttime_gmt = as.Date("2025-01-01"),
    logging_mode = NA,
    started_by = NA,
    started_where = "TestColony",
    days_delayed = NA,
    programmed_gmt_time = NA,
    intended_species = "bird",
    intended_location = "TestColony",
    intended_deployer = NA,
    shutdown_session = NA,
    field_status = NA,
    downloaded_by = NA,
    download_type = NA,
    download_date = NA,
    decomissioned = NA,
    shutdown_date = NA,
    comment = ""
  )
}

create_test_logger_returns <- function() {
  tibble(
    logger_id = "L1",
    status = "Downloaded",
    `download / stop_date` = as.Date("2025-01-10"),
    `downloaded by` = "User",
    comment = "Logger returned",
    `stored or sent to?` = ""
  )
}

create_test_restart_times <- function() {
  tibble(
    logger_id = character(),
    startdate_GMT = as.Date(character()),
    starttime_GMT = as.POSIXct(character()),
    `Logging mode` = character(),
    intended_species = character(),
    comment = character()
  )
}

create_test_master_import <- function() {
  list(
    METADATA = tibble(
      ring_number = NA,
      logger_id_deployed = "A",
      logger_id_retrieved = NA,
      date = as.Date("2025-01-01"),
      nest_latitude = NA,
      nest_longitude = NA
    ),
    `STARTUP_SHUTDOWN` = create_test_startup_sheet()
  )
}

describe("File System Operations", {
  test_that("start_logging creates log file", {
    log_file <- paste0("seatrack_functions_log_", Sys.Date(), ".txt")
    start_logging(tmp_dir, log_file)
    expect_true(file.exists(file.path(tmp_dir, log_file)))
  })

  test_that("set_sea_track_folder sets global variable and logs", {
    expect_error(set_sea_track_folder("nonexistent_dir"))
    set_sea_track_folder(tmp_dir)
    expect_true(exists("sea_track_folder", envir = the))
  })

  test_that("get_master_import_path errors if folder not set", {
    expect_error(get_master_import_path("ColonyA"))
  })

  test_that("get_startup_paths returns xlsx files", {
    the$sea_track_folder <<- tmp_dir
    startup_path <- file.path(tmp_dir, "Starttime files and stored loggers", 2025)
    dir.create(startup_path, recursive = TRUE, showWarnings = FALSE)
    test_file <- file.path(startup_path, "test.xlsx")
    file.create(test_file)
    expect_type(get_startup_paths(), "character")
    file.remove(test_file)
  })

  test_that("load_sheets_as_list loads sheets", {
    test_file <- file.path(tmp_dir, "test.xlsx")
    wb <- openxlsx2::wb_workbook()
    wb$add_worksheet("Sheet1")
    wb$add_worksheet("Sheet2")
    wb$add_data("Sheet1", data.frame(a = 1:3, b = 4:6))
    wb$add_data("Sheet2", data.frame(x = 7:9, y = 10:12))
    openxlsx2::wb_save(wb, test_file)
    sheets <- load_sheets_as_list(test_file, c("Sheet1", "Sheet2"))
    expect_equal(length(sheets$data), 2)
    expect_true(all(sapply(sheets$data, is.data.frame)))
    file.remove(test_file)
  })

  test_that("save_master_sheet writes xlsx file with LoadedWB", {
    # Create test data list
    data_list <- list(
      "Sheet1" = tibble(a = 1:3, b = 4:6),
      "Sheet2" = tibble(x = 7:9, y = 10:12)
    )
    # Create LoadedWB instance
    wb <- openxlsx2::wb_workbook()
    wb$add_worksheet("Sheet1")
    wb$add_worksheet("Sheet2")

    loaded_wb <- LoadedWB$new(data = data_list, wb = wb)
    
    out_file <- file.path(tmp_dir, "out.xlsx")
    save_master_sheet(loaded_wb, out_file)
    expect_true(file.exists(out_file))
    
    # Verify the saved file contains the correct data
    result <- load_sheets_as_list(out_file, c("Sheet1", "Sheet2"))
    expect_equal(result$data$Sheet1, data_list$Sheet1)
    expect_equal(result$data$Sheet2, data_list$Sheet2)
    
    file.remove(out_file)
  })
})


describe("Data Manipulation Operations", {
  test_that("append_encounter_data appends non-duplicate rows", {
    master <- data.frame(
      ring_number = NA,
      logger_id_deployed = c("A", "B"),
      logger_id_retrieved = NA,
      date = as.Date(c("2025-01-01", "2025-01-02")),
      nest_latitude = NA,
      nest_longitude = NA
    )
    encounter <- data.frame(
      ring_number = NA,
      logger_id_deployed = c("C", "B"),
      logger_id_retrieved = NA,
      date = as.Date(c("2025-01-03", "2025-01-02")),
      nest_latitude = NA,
      nest_longitude = NA
    )
    result <- append_encounter_data(master, encounter)
    expect_equal(nrow(result), 3) # Only non-duplicate appended
  })

  test_that("append_encounter_data errors on column mismatch", {
    master <- data.frame(a = 1)
    encounter <- data.frame(b = 2)
    expect_error(append_encounter_data(master, encounter))
  })


  test_that("load_nonresponsive loads existing file for Lotek", {
    file_path <- file.path(tmp_dir, "lotek_unresponsive.xlsx")
    df <- tibble(
      logger_serial_no = "L1",
      logger_model = "birdTracker5000",
      producer = "Lotek",
      production_year = 2024,
      project = "seatrack",
      starttime_gmt = as.POSIXct("2025-01-01"),
      download_type = "Nonresponsive",
      download_date = as.Date("2025-01-10"),
      comment = "No response"
    )
    openxlsx2::write_xlsx(df, file_path)
    result <- load_nonresponsive(c(file_path), c("Lotek"))
    expect_equal(nrow(result$sheets_list$lotek$data[[1]]), 1)
    expect_equal(result$sheets_list$lotek$data[[1]]$producer[1], "Lotek")
    file.remove(file_path)
  })


  test_that("load_nonresponsive initializes empty sheet if file missing", {
    file_path <- file.path(tmp_dir, "missing_lotek_unresponsive.xlsx")
    result <- load_nonresponsive(file_path, "Lotek")
    expect_equal(nrow(result$sheets_list$lotek$data[[1]]), 0)
    expect_true(all(c(
      "logger_serial_no", "logger_model", "producer", "production_year", "project",
      "starttime_gmt", "logging_mode", "days_delayed", "programmed_gmt_time",
      "download_type", "download_date", "comment", "priority"
    ) %in% names(result$sheets_list$lotek$data[[1]])))
  })


  test_that("set_master_startup_value updates cell value", {
    df <- data.frame(a = 1:3, b = 4:6)
    updated <- set_master_startup_value(df, 2, "b", 99)
    expect_equal(updated$b[2], 99)
  })

  test_that("set_comments sets and appends comments", {
    df <- data.frame(comment = c("", "Existing"))
    df1 <- set_comments(df, 1, "New")
    expect_equal(df1$comment[1], "New")
    df2 <- set_comments(df1, 2, "Another")
    expect_equal(df2$comment[2], "Existing | Another")
  })
})

describe("Logger Session Management", {
  colony <- "TestColony"
  master_startup <- create_test_startup_sheet()
  logger_returns <- create_test_logger_returns()
  restart_times <- create_test_restart_times()

  test_that("handle_returned_loggers updates master_startup with logger_returns", {
    updated <- handle_returned_loggers(colony, master_startup, logger_returns, restart_times)
    expect_true(any(!is.na(updated$master_startup$download_date)))
    expect_true(any(updated$master_startup$comment == "Logger returned"))
  })

  test_that("handle_returned_loggers returns unhandled loggers if no unfinished session", {
    ms <- tibble(
      logger_serial_no = "X",
      starttime_gmt = as.Date("2025-01-01"),
      shutdown_date = as.Date("2025-01-02"),
      download_date = as.Date("2025-01-02")
    )
    lr <- tibble(
      logger_id = "Y",
      status = "Downloaded",
      `download / stop_date` = as.Date("2025-01-10"),
      `downloaded by` = "User",
      comment = "Logger returned",
      `stored or sent to?` = ""
    )
    result <- handle_returned_loggers(colony, ms, lr, restart_times)
    expect_true(is.list(result))
  })

  test_that("handle_returned_loggers handles nonresponsive loggers", {
    lr <- tibble(
      logger_id = "L1",
      status = "Nonresponsive",
      `download / stop_date` = as.Date("2025-01-10"),
      `downloaded by` = "User",
      comment = "No response",
      `stored or sent to?` = "Nonresponsive"
    )
    nonresponsive_list <- load_nonresponsive("foo", "Lotek")
    result <- handle_returned_loggers(colony, master_startup, lr, restart_times, nonresponsive_list)
    expect_true(is.list(result))
  })

  test_that("handle_returned_loggers processes restarts", {
    lr <- tibble(
      logger_id = "L1",
      status = "Downloaded",
      `download / stop_date` = as.Date("2025-01-10"),
      `downloaded by` = "User",
      comment = "Logger returned",
      `stored or sent to?` = "redeployed"
    )
    rt <- tibble(
      logger_id = "L1",
      startdate_GMT = as.Date("2025-01-11"),
      starttime_GMT = as.POSIXct("2025-01-11 12:00:00"),
      `Logging mode` = "modeA",
      intended_species = "bird",
      comment = "Restarted"
    )
    result <- handle_returned_loggers(colony, master_startup, lr, rt)
    expect_true(is.list(result))
    expect_true(nrow(result$master_startup) == 2) # Original + restart
  })


  test_that("get_unfinished_session returns NULL if no matching logger", {
    df <- tibble(
      logger_serial_no = c("A", "B"),
      starttime_gmt = as.Date(c("2025-01-01", "2025-01-02")),
      shutdown_date = c(NA, NA),
      download_date = c(NA, NA)
    )
    result <- get_unfinished_session(df, "C", as.Date("2025-01-10"))
    expect_null(result)
  })

  test_that("get_unfinished_session returns NULL if all sessions finished", {
    df <- tibble(
      logger_serial_no = c("A", "A"),
      starttime_gmt = as.Date(c("2025-01-01", "2025-01-02")),
      shutdown_date = as.Date(c("2025-01-03", "2025-01-04")),
      download_date = as.Date(c("2025-01-03", "2025-01-04"))
    )
    result <- get_unfinished_session(df, "A", as.Date("2025-01-10"))
    expect_null(result)
  })

  test_that("get_unfinished_session handles multiple unfinished sessions and missing download date", {
    df <- tibble(
      logger_serial_no = c("A", "A"),
      starttime_gmt = as.Date(c("2025-01-01", "2025-01-02")),
      shutdown_date = c(NA, NA),
      download_date = c(NA, NA)
    )
    result <- get_unfinished_session(df, "A", NA)
    expect_null(result)
  })


  test_that("add_loggers_from_startup skips unreadable Excel file", {
    df <- tibble(
      logger_serial_no = c("A"),
      starttime_gmt = as.Date("2025-01-01"),
      intended_location = "Loc1"
    )
    startup_dir <- file.path(tmp_dir, "Starttime files and stored loggers", "2025")
    dir.create(startup_dir, recursive = TRUE, showWarnings = FALSE)
    writeLines("not an excel file", file.path(startup_dir, "corrupt.xlsx"))
    the$sea_track_folder <<- tmp_dir
    result <- add_loggers_from_startup(df)
    expect_equal(nrow(result), 1)
    file.remove(file.path(startup_dir, "corrupt.xlsx"))
  })

  test_that("add_loggers_from_startup skips file with wrong data types", {
    df <- tibble(
      logger_serial_no = c("A"),
      starttime_gmt = as.Date("2025-01-01"),
      intended_location = "Loc1"
    )
    startup_dir <- file.path(tmp_dir, "Starttime files and stored loggers", "2025")
    dir.create(startup_dir, recursive = TRUE, showWarnings = FALSE)
    wrong_type_logger <- tibble(
      logger_serial_no = 123,
      starttime_gmt = "not a date",
      intended_location = "Loc1"
    )
    file_path <- file.path(startup_dir, "wrong_type.xlsx")
    openxlsx2::write_xlsx(wrong_type_logger, file_path)
    the$sea_track_folder <<- tmp_dir
    result <- add_loggers_from_startup(df)
    expect_equal(nrow(result), 1)
    file.remove(file_path)
  })

  test_that("add_loggers_from_startup skips file with column mismatch", {
    df <- tibble(
      logger_serial_no = c("A"),
      starttime_gmt = as.Date("2025-01-01"),
      intended_location = "Loc1"
    )
    startup_dir <- file.path(tmp_dir, "Starttime files and stored loggers", "2025")
    dir.create(startup_dir, recursive = TRUE, showWarnings = FALSE)
    wrong_logger <- tibble(wrong_col = "B")
    file_path <- file.path(startup_dir, "wrong_startup.xlsx")
    openxlsx2::write_xlsx(wrong_logger, file_path)
    the$sea_track_folder <<- tmp_dir
    result <- add_loggers_from_startup(df)
    expect_equal(nrow(result), 1)
    file.remove(file_path)
  })
})

describe("Colony Location Operations", {
  test_that("get_all_locations fails if sea track folder is not set", {
    the$sea_track_folder <<- NULL
    expect_error(get_all_locations(), "Sea track folder is not set")
  })

  test_that("get_all_locations fails if Locations folder doesn't exist", {
    the$sea_track_folder <<- tmp_dir
    expect_error(get_all_locations(), "Locations folder not found")
  })

  test_that("get_all_locations returns correct structure", {
    the$sea_track_folder <<- tmp_dir
    locations_path <- file.path(tmp_dir, "Locations")
    # Create test directory structure
    dir.create(file.path(locations_path, "Norway", "Jan Mayen"), recursive = TRUE)
    dir.create(file.path(locations_path, "Norway", "Sklinna"), recursive = TRUE)
    dir.create(file.path(locations_path, "Finland", "Tvärminne"), recursive = TRUE)

    colonies <- get_all_locations()
    expect_type(colonies, "list")
    expect_named(colonies, c("Finland", "Norway"))
    expect_equal(colonies$Norway, c("Jan Mayen", "Sklinna"))
    expect_equal(colonies$Finland, "Tvärminne")

    # Clean up test directories
    unlink(locations_path, recursive = TRUE)
  })
})

describe("Partner Metadata Processing", {
  test_that("load_partner_metadata fails on nonexistent file", {
    expect_error(load_partner_metadata("nonexistent.xlsx"), "does not exist")
  })

  test_that("load_partner_metadata loads metadata sheets", {
    test_file <- file.path(tmp_dir, "partner_metadata.xlsx")
    wb <- openxlsx2::wb_workbook()
    # Create ENCOUNTER DATA sheet
    encounter_data <- tibble(
      ring_number = NA,
      logger_id_deployed = "A",
      logger_id_retrieved = NA,
      date = as.Date("2025-01-01"),
      nest_latitude = NA,
      nest_longitude = NA
    )

    logger_returns <- tibble(
      logger_id = "A",
      status = "Downloaded",
      `download / stop_date` = as.Date("2025-01-05"),
      `downloaded by` = "User",
      comment = "Logger returned",
      `stored or sent to?` = ""
    )

    restart_times <- tibble(
      logger_id = "A",
      startdate_GMT = as.Date("2025-01-06"),
      starttime_GMT = as.POSIXct("2025-01-06 12:00:00"),
      `Logging mode` = "Mode1",
      intended_species = "bird",
      comment = "Restarted"
    )

    wb$add_worksheet("ENCOUNTER DATA")
    wb$add_worksheet("LOGGER RETURNS")
    wb$add_worksheet("RESTART TIMES")
    # Add a header row plus data to simulate real file structure
    wb$add_data("ENCOUNTER DATA", "SeaTrack Partner: ENCOUNTER DATA", startRow = 1)
    wb$add_data("ENCOUNTER DATA", encounter_data, startRow = 2)
    wb$add_data("LOGGER RETURNS", "SeaTrack Partner: LOGGER RETURNS", startRow = 1)
    wb$add_data("LOGGER RETURNS", logger_returns, startRow = 2)
    wb$add_data("RESTART TIMES", "SeaTrack Partner: RESTART TIMES", startRow = 1)
    wb$add_data("RESTART TIMES", restart_times, startRow = 2)
    openxlsx2::wb_save(wb, test_file)

    metadata_list <- load_partner_metadata(test_file)
    expect_equal("LoadedWB", class(metadata_list)[1])
    expect_named(metadata_list$data, c("ENCOUNTER DATA", "LOGGER RETURNS", "RESTART TIMES"))
    expect_equal(nrow(metadata_list$data$`ENCOUNTER DATA`), 1)

    file.remove(test_file)
  })



  test_that("handle_partner_metadata errors if master_import missing required sheets", {
    master_import_data <- list(
      METADATA = tibble(
        ring_number = NA,
        logger_id_deployed = "A",
        logger_id_retrieved = NA,
        date = as.Date("2025-01-01"),
        nest_latitude = NA,
        nest_longitude = NA
      )
      # Missing STARTUP_SHUTDOWN
    )
    new_metadata_data <- list(
      `ENCOUNTER DATA` = tibble(),
      `LOGGER RETURNS` = tibble(),
      `RESTART TIMES` = tibble()
    )
      new_metadata <- LoadedWB$new(data = new_metadata_data, wb = openxlsx2::wb_workbook())
    master_import <- LoadedWB$new(data = master_import_data, wb = openxlsx2::wb_workbook())
    expect_error(handle_partner_metadata("TestColony", new_metadata, master_import))
  })

  test_that("handle_partner_metadata handles empty new_metadata", {
    master_import_data <- list(
      METADATA = tibble(
        ring_number = NA,
        logger_id_deployed = "A",
        logger_id_retrieved = NA,
        date = as.Date("2025-01-01"),
        nest_latitude = NA,
        nest_longitude = NA
      ),
      `STARTUP_SHUTDOWN` = tibble(
        logger_serial_no = "A",
        starttime_gmt = as.Date("2025-01-01"),
        intended_location = "Loc1"
      )
    )
    new_metadata_data <- list(
      `ENCOUNTER DATA` = tibble(),
      `LOGGER RETURNS` = tibble(),
      `RESTART TIMES` = tibble()
    )
    new_metadata <- LoadedWB$new(data = new_metadata_data, wb = openxlsx2::wb_workbook())
    master_import <- LoadedWB$new(data = master_import_data, wb = openxlsx2::wb_workbook())
    result <- handle_partner_metadata("TestColony", new_metadata, master_import)
    expect_true(is.list(result$master_import$data))
    expect_true("METADATA" %in% names(result$master_import$data))
    expect_true("STARTUP_SHUTDOWN" %in% names(result$master_import$data))
  })

  test_that("handle_partner_metadata processes new encounters and returns", {
    # Create master import with existing data
    master_import_data <- list(
      METADATA = tibble(
        ring_number = c(NA, "R2"),
        logger_id_deployed = c("A", "B"),
        logger_id_retrieved = c(NA, NA),
        date = as.Date(c("2025-01-01", "2025-01-02")),
        nest_latitude = c(NA, 60.5),
        nest_longitude = c(NA, 22.5)
      ),
      `STARTUP_SHUTDOWN` = tibble(
        logger_serial_no = c("A", "B"),
        logger_model = c("Model1", "Model2"),
        producer = c("Lotek", "MigrateTech"),
        production_year = c(2024, 2024),
        project = c("seatrack", "seatrack"),
        starttime_gmt = as.Date(c("2025-01-01", "2025-01-02")),
        logging_mode = c(NA, 1),
        started_by = c(NA, "User"),
        started_where = c("TestColony", "TestColony"),
        days_delayed = c(NA, 0),
        programmed_gmt_time = as.Date(c(NA, "2025-01-02")),
        intended_species = c("bird", "bird"),
        intended_location = c("TestColony", "TestColony"),
        intended_deployer = c(NA, NA),
        shutdown_session = c(NA, NA),
        field_status = c(NA, NA),
        downloaded_by = c(NA, NA),
        download_type = c(NA, NA),
        download_date = c(NA, NA),
        decomissioned = c(NA, NA),
        shutdown_date = c(NA, NA),
        comment = c("", "")
      )
    )

    # Create new metadata with both encounters and returns
    new_metadata_data <- list(
      `ENCOUNTER DATA` = tibble(
        ring_number = c("R3", NA),
        logger_id_deployed = c("C", "A"),
        logger_id_retrieved = c(NA, NA),
        date = as.Date(c("2025-01-03", "2025-01-04")),
        nest_latitude = c(60.6, NA),
        nest_longitude = c(22.6, NA)
      ),
      `LOGGER RETURNS` = tibble(
        logger_id = "A",
        status = "Downloaded",
        `download / stop_date` = as.Date("2025-01-05"),
        `downloaded by` = "User2",
        comment = "Successfully retrieved",
        `stored or sent to?` = "redeployed"
      ),
      `RESTART TIMES` = tibble(
        logger_id = "A",
        startdate_GMT = as.Date("2025-01-06"),
        starttime_GMT = as.POSIXct("2025-01-06 12:00:00"),
        `Logging mode` = "Mode3",
        intended_species = "bird",
        comment = "Restarted successfully"
      )
    )

    new_metadata <- LoadedWB$new(data = new_metadata_data, wb = openxlsx2::wb_workbook())
    master_import <- LoadedWB$new(data = master_import_data, wb = openxlsx2::wb_workbook())

    result <- handle_partner_metadata("TestColony", new_metadata, master_import)

    # Check metadata updates
    expect_equal(nrow(result$master_import$data$METADATA), 4)
    expect_true(any(result$master_import$data$METADATA$date == as.Date("2025-01-03")))

    # Check startup/shutdown updates
    startup_sheet <- result$master_import$data$`STARTUP_SHUTDOWN`
    expect_true(any(!is.na(startup_sheet$download_date)))
    expect_equal(sum(startup_sheet$logger_serial_no == "A"), 2) # Original + restarted
    expect_true(any(grepl("Successfully retrieved", startup_sheet$comment)))
  })

  test_that("handle_partner_metadata handles nonresponsive loggers", {
    master_import_data <- list(
      METADATA = tibble(
        ring_number = NA,
        logger_id_deployed = "A",
        logger_id_retrieved = NA,
        date = as.Date("2025-01-01"),
        nest_latitude = NA,
        nest_longitude = NA
      ),
      `STARTUP_SHUTDOWN` = tibble(
        logger_serial_no = "A",
        logger_model = "Model1",
        producer = "Lotek",
        production_year = 2024,
        project = "seatrack",
        starttime_gmt = as.Date("2025-01-01"),
        logging_mode = NA,
        started_by = NA,
        started_where = "TestColony",
        days_delayed = NA,
        programmed_gmt_time = NA,
        intended_species = "bird",
        intended_location = "TestColony",
        intended_deployer = NA,
        shutdown_session = NA,
        field_status = NA,
        downloaded_by = NA,
        download_type = NA,
        download_date = NA,
        decomissioned = NA,
        shutdown_date = NA,
        comment = ""
      )
    )

    master_import <- LoadedWB$new(data = master_import_data, wb = wb_workbook())

    new_metadata_data <- list(
      `ENCOUNTER DATA` = tibble(),
      `LOGGER RETURNS` = tibble(
        logger_id = "A",
        status = "Nonresponsive",
        `download / stop_date` = as.Date("2025-01-10"),
        `downloaded by` = "User",
        comment = "No response",
        `stored or sent to?` = "Nonresponsive",
      ),
      `RESTART TIMES` = tibble()
    )

    new_metadata <- LoadedWB$new(data = new_metadata_data, wb = wb_workbook())

    nonresponsive_sheet <- tibble(
      logger_serial_no = character(),
      logger_model = character(),
      producer = character(),
      production_year = numeric(),
      project = character(),
      starttime_gmt = as.POSIXct(character()),
      download_type = character(),
      download_date = as.Date(character()),
      comment = character(),
      intended_species = character(),
      intended_location = character()
    )
    nonresponsive_list <- LoadedWBCollection$new(sheets_list = list(lotek = LoadedWB$new(data = list(sheet1 = nonresponsive_sheet))))

    result <- handle_partner_metadata("TestColony", new_metadata, master_import, nonresponsive_list)

    # Check nonresponsive list updates
    expect_equal(nrow(result$nonresponsive_list$sheets_list$lotek$data[[1]]), 1)
    expect_equal(result$nonresponsive_list$sheets_list$lotek$data[[1]]$logger_serial_no[1], "A")
    expect_equal(result$nonresponsive_list$sheets_list$lotek$data[[1]]$download_type[1], "Nonresponsive")

    # Check startup sheet updates
    startup_sheet <- result$master_import$data$`STARTUP_SHUTDOWN`
    expect_equal(startup_sheet$download_type[1], "Nonresponsive")
    expect_equal(startup_sheet$download_date[1], as.Date("2025-01-10"))
  })
})
