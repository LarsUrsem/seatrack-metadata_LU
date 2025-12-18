appender_file_safe <- function(file) {
    force(file)
    function(line) {
        # make sure folder exists
        dir <- dirname(file)
        if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

        tryCatch(
            {
                print(line)
                con <- file(file, open = "a")
                writeLines(line, con)
                close(con)
            },
            error = function(e) {
                warning("Failed to write to file: ", conditionMessage(e))
            }
        )
    }
}
