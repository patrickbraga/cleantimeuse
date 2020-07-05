#' (Internal) Check that data has all core variables
#'
#' Examines whether the user-loaded data includes `AGE`, `TIME`, `ELOC`, `ACT_WORK`, and `MAIN.` These are the minimum variables needed for other data manipulation functions in the `gsdmtus` package.
#'
#' @param mtus_data User-loaded IPUMS MTUS data which has been loaded using the `mtus_init` function.
#'
#' @return `TRUE` if all those variables are present. `FALSE` otherwise.

all_core_variables_are_in <- function(mtus_data) {
    minimum_vars <- c("AGE", "TIME", "ELOC", "ACT_WORK", "MAIN")
    user_downloaded_vars <- colnames(mtus_data)
    missing_vars <- NULL

    # For each of the minimum required variables,
    # check that it is in the user-inputted data
    # set.
    for (i in 1:length(minimum_vars)) {
        var_to_check = minimum_vars[i]

        if (var_to_check %in% user_downloaded_vars) {
            print(c(var_to_check, "is present."))
        } else {
            print(c(var_to_check, "is missing."))
            missing_vars <- c(missing_vars, minimum_vars[i])
        }
    }

    # If the list of missing variables is not
    # empty, then tell the user there are
    # variables missing.
    if (!is.null(missing_vars)) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}
