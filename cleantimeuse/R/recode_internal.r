recode_internal <- function(input_data, activities = FALSE,
    locations = FALSE) {

    #' (Internal) Recode activites and/or locations
    #'
    #' This is an internal function that uses preset codes and labels for activites (both MAIN and SEC) and location (ELOC) variables. It is called from within `mtus_init_countries`. This function uses pre-loaded CSV files to run calls of the `mtus_recode` function.
    #'
    #' @param input_data The by-activity data to recode, which may be entered either as a string inq uotes or as a data object without quotes.
    #' @param activites Boolean; toggle whether to recode the activities in the `MAIN` and `SEC` columns.
    #' @param locations Boolean; toggle whether to recode the locations in the `ELOC` column.

    # If user enters input_data as a character
    # string instead of a data object, then
    # convert it into a data object. Otherwise,
    # work directly with the data object.  But
    # save the name of the sample as varname so
    # that we can update it directly with all the
    # recoded data at the end of the function.

    # print(paste('input_data =', input_data))
    # print(eval(typeof(input_data)))

    if (typeof(input_data) == "character") {
        print("type is character")
        varname = input_data
        eval(call("assign", "input_data", as.name(input_data)))
    } else {
        print("type is list")
        # the command below transforms the input_data
        # argument into a string
        varname = deparse(substitute(input_data))
    }

    if (activities == TRUE) {
        # load a csv file with equivalences
        print("recoding main activities")
        input_data <- mtus_recode(input_data,
            "MAIN", "csv/codes_activities.csv")

        if ("SEC" %in% names(input_data)) {
            print("recoding secondary activities")
            input_data <- mtus_recode(input_data,
                "SEC", "csv/codes_sec.csv")
        } else {
            print("SEC column not found in data")
        }
    }

    if (locations == TRUE) {
        print("recoding locations")
        input_data <- mtus_recode(input_data,
            "ELOC", "csv/codes_locations.csv")
    }

    View(input_data)
    # first check if user-inputted data has a SEC
    # column for secondary activities.  names(...)
    # creates a vector with all column names
    # Finally, replace the original data object
    # with this labeled one eval(call('assign',
    # c(as.name(varname)), input_data))
    # View(input_data)
    return(input_data)
}
