people <- NULL

mtus_init_raw_data <- function(xml_file_path, check_vars = TRUE) {
    #' Load IPUMS MTUS data
    #'
    #' This function makes it easy for the user to import MTUS data downloaded directly from the IPUMS website. Ideally, it's the first function in the `gsdmtus` package that the user runs. Unlike the code suggested on the IPUMS website, this function resolves a "labels must be unique" error that may appear when loading the data. It also offers an option to check whether the user has downloaded the basic variables needed for data cleaning in other `gsdmtus` functions.
    #'
    #' @details Use this function early on in your code to load by-activity and by-person data, the two primary data formats in which the MTUS data are available. You can select whether to download data by activity or by individual on the IPUMS download page. Both formats are required for full use of the `gsdmtus` package.
    #'
    #' @param xml_file_path String with file path to an xml file downloaded from the IPUMS MTUS website. This xml file contains the information that the `ipumsr` package uses to load the data into R.
    #' @param check_vars `TRUE` to check whether `AGE`, `TIME`, `ELOC`, `ACT_WORK`, and `MAIN` are present. `FALSE` to skip that step.
    #'
    #' @usage by_activity_data <- mtus_init_raw_data("mtus_data/mtus_00009.xml")
    #' @usage by_person_data <- mtus_init_raw_data("mtus_data/mtus_00014.xml", FALSE)
    #'
    #'
    #' @return Returns `mtus_data`, the IPUMS MTUS data loaded and ready for manipulation.

    # xml_file_path is the path to the IPUMS xml
    # to be read The zap_labels function resolves
    # a 'labels must be unique' error that
    # otherwise appears.  The code below is
    # adapted from IPUMS instructions.

    mtus_data <- read_ipums_ddi(xml_file_path) %>%
        read_ipums_micro() %>%
        zap_labels()

    if (check_vars == TRUE) {
        if (all_core_variables_are_in(mtus_data)) {
            print("All minimum required variables are present.")
            return(mtus_data)
        } else {
            print("You did not download all the basic variables. Please make sure your data extract includes AGE, TIME, ELOC, ACT_WORK, and MAIN at the very least.")
            return(FALSE)
        }
    } else {
        return(mtus_data)
    }
}
