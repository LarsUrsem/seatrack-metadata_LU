#' @title LoadedMetadata Class
#'
#' @description A convenience class for handling metadata loaded from workbooks.
#' @export
#' @family classes
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

        #' @field modified Has the workbook been modified since import?
        modified = FALSE,

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
#' @family classes
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
            sapply(self$sheets_list, function(x) {
                x$path
            })
        },
        #' @field all_names The names of all workbooks in the collection
        all_names = function() {
            names(self$sheets_list)
        },
        #' @field modified sheets_list
        modified = function(){
            modified_sheets <- sapply(self$sheets_list, function(x) {
                if(x$modified){
                    return(x)
                }
            })
            modified_sheets <- modified_sheets[!sapply(is.null, modified_sheets)]
            return(modified_sheets)
        }
    )
)

#' @title SessionBatch Class
#' @description A convenience class for storing a set of logger sessions along with the type of database import they require.
#' @export
#' @family classes
SessionBatch <- R6::R6Class(
    "SessionBatch",
    public = list(
        #' @description Create a new SessionBatch object
        #' @param sessions A tibble containing session information from master import startup_shutdown.
        #' @param type The type of import that needs to occur. Must be either "close_only", "open_only" or "open_and_close".
        #' @return A new SessionBatch object
        initialize = function(sessions = tibble(), type = c("close_only", "open_only", "open_and_close")) {
            self$type <- match.arg(type)
            self$sessions <- sessions
        },
        #' @field sessions Tibble containing session information from master import startup_shutdown.
        sessions = tibble(),
        #' @field type The type of import that needs to occur. Must be either "close_only", "open_only" or "open_and_close". 
        type = character(),
        #' @description
        #' Print method for SessionBatch
        #' @return The SessionBatch object invisibly
        print = function() {
            cat("Sessions:\n")
            print(self$sessions)
            cat(paste0("Sessions will be ", gsub("_"," ", self$type),"\n"))
            invisible(self)
        }       
    )
)

#' @title DBImportCollection Class
#' @description A convenience class for storing logger sessions alongside the deployments and retrievals that are associated with them.
#' @export
#' @family classes
DBImportCollection <- R6::R6Class(
    "DBImportCollection",
    public = list(
        #' @description
        #' Create a new DBImportCollection object
        #' @param sessions A SessionBatch containing session information from master import startup_shutdown.
        #' @param retrievals A tibble containing retrievals events information from master import metadata
        #' @param deployments A tibble containing deployments events information from master import metadata
        #' @return A new DBImportCollection object
        initialize = function(sessions = SessionBatch$new(), retrievals = tibble(), deployments = tibble()){
            self$sessions = sessions
            self$retrievals = retrievals
            self$deployments = deployments
        },
        #' @field sessions SessionBatch containing session information from master import startup_shutdown.
        sessions = SessionBatch$new(),
        #' @field retrievals Tibble containing retrievals events information from master import metadata
        retrievals = tibble(),
        #' @field deployments Tibble containing retrievals events information from master import metadata
        deployments = tibble(),
        #' @description
        #' Print method for DBImportCollection
        #' @return The DBImportCollection object invisibly        
        print = function() {
            cat("$sessions:\n")
            print(self$sessions)
            cat("$retrievals:\n")
            print(self$retrievals)
            cat("$deployments:\n")
            print(self$deployments)            
            invisible(self)
        }       
    ),
    active = list(
        #' @field type The type of import that needs to occur.
        type = function() {
            self$sessions$type
        }
    )
)
