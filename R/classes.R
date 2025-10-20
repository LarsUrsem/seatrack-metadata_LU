#' @title LoadedMetadata Class
#'
#' @description A convenience class for handling metadata loaded from workbooks.
#' @export
LoadedWB <- R6::R6Class(
    "LoadedWB",
    public = list(
        #' @description
        #' Create a new LoadedWB object
        #' @param data A list of tibbles
        #' @param wb A workbook object
        #' @return A new LoadedWB object
        initialize = function(data = list(), wb = openxlsx2::wb_workbook()) {
            self$data <- data
            self$wb <- wb
        },

        #' @field data A list of tibbles
        data = list(),

        #' @field wb A workbook object
        wb = openxlsx2::wb_workbook(),

        #' @description
        #' Print method for LoadedWB
        #' @return The LoadedWB object invisibly
        print = function() {
            cat(paste0("data originally from ", self$wb$path, ":\n"))
            print(self$data)
            invisible(self)
        }
    ),
    active = list(
        #' @field path The path of the workbook
        path = function() {
            return(self$wb$path)
        }
    )
)

#' @title LoadedWBCollection Class
#' @description A convenience class for handling collections of LoadedWB objects.
#' @export
LoadedWBCollection <- R6::R6Class(
    "LoadedWBCollection",
    public = list(
        #' @description
        #' Create a new LoadedWBCollection object
        #' @param sheets_list A list of LoadedWB objects
        #' @return A new LoadedWBCollection object
        initialize = function(sheets_list = list()) {
            self$sheets_list <- sheets_list
        },
        #' @field sheets_list A list of LoadedWB objects
        sheets_list = list(),

        #' @description
        #' Get the names of the sheets in the collection
        #' @return A character vector of sheet names
        names = function() {
            names(self$sheets_list)
        },

        #' @description
        #' Print method for LoadedWBCollection
        #' @return The LoadedWBCollection object invisibly
        print = function() {
            for (sheet in self$sheets_list) {
                print(sheet)
            }
            invisible(self)
        }
    ),
    active = list(
        #' @field all_paths The paths of all workbooks in the collection
        all_paths = function() {
            sapply(sheets_list, function(x) {
                x$path
            })
        },
        #' @field all_names The names of all workbooks in the collection
        all_names = function() {
            names(sheets_list)
        }
    )
)
