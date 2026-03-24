# Setup Julia environment before running tests
# This file is automatically run by testthat before tests
tryCatch(
    {
        setup_daf(pkg_check = FALSE)
        JULIA_AVAILABLE <- TRUE
    },
    error = function(e) {
        JULIA_AVAILABLE <<- FALSE
    }
)
