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
#' @param col_upper A list the same length as sheets, containing either NULL or a character vector of column names to be forced into uppercase.
#' @return A `LoadedMetadata` object, with a list of tibbles corresponding to each sheet in `data` and the original workbook in `wb``
#' @examples
#' \dontrun{
#' sheets_data <- load_sheets_as_list("path/to/file.xlsx", c("Sheet1", "Sheet2"), skip = 1)
#' }
#' @export
#' @concept utility
load_sheets_as_list <- function(
    file_path, sheets, skip_rows = 0, force_date = TRUE, drop_unnamed = TRUE,
    col_types = rep(list(NULL), length(sheets)), col_upper = rep(list(NULL), length(sheets))) {
    if (!file.exists(file_path)) {
        stop("The specified file does not exist.")
    }
    log_trace("Loading file: ", file_path)
    wb <- openxlsx2::wb_load(file_path)
    # Iterate through sheets and read data
print(col_upper)

    data_list <- lapply(1:length(sheets), function(sheet_index) {
        print(sheet_index)
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
