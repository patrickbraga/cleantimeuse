mtus_init_countries <- function(by_activity_data,
                                by_person_data,
                                countries,
                                recode_activities = FALSE,
                                recode_locations = FALSE) {
    #' Load country-specific data from IPUMS MTUS data set
    #'
    #' This procedure makes it easy for the user to split imported MTUS data into separate countries. The user must first use the `mtus_init_raw_data` function to load by-activity data and by-person data before using `mtus_init_countries`.
    #'
    #' @param by_activity_data By-activity data downloaded from the IPUMS website and then loaded into R with `mtus_init_raw_data`. The user must at minimum provide by-activity data; by-person data
    #' @param by_person_data One of two options: either by-person data downloaded from the IPUMS website and loaded into R with `mtus_init_raw_data`, or `NULL` if the user's intent is to create standalone by-activity data objects.
    #' @param countries The user may enter a specific country-year sample (e.g. "CA2010"), a character list of country-year samples (e.g. "FR2009" or "UK2014"), or `"all"`.
    #' @param recode_activites Boolean. `TRUE` to recode numeric activity codes in the `MAIN` and `SEC` columns with human-readable labels. `FALSE` to skip.
    #' @param recode_locations Boolean. `TRUE` to recode numeric activity codes in the `ELOC` column with human-readable labels. `FALSE` to skip.
    #'
    #' @return This procedure creates individual data objects for any given country sample (or all country samples). For instance, if a user has a large data set and only wishes to load France and UK, they can provide the character list `c("FR2009", "UK2014")` as the `countries` argument.
    #' @return If the user wants to pick out every country in the data and create a separate data object for each one, the user should enter `"all"` as the `countries` argument.
    #' @return To create a data object of solely by-activity data without counting numbers of trips that an individual took, set the `by_person_data` argument to `NULL`.
    #'
    #' @details Please note that this procedure does not return a value; the user should call it without the assignment operator (`<-`).

    # variant of mtus_load_country without a return object

    # if user calls the function as
    # mtus_init_countries(…, 'all') then then
    # assign to the 'countries' variable an array
    # with all unique values in the 'SAMPLE'
    # column.

    if (countries == "all") {
        # because by_person_data will be smaller than
        # by_activity data (because data are grouped
        # by person rather than each separate activity
        # a person does), extract country-year
        # combinations from by_person_data. But if
        # it's NULL (i.e., if a user is extracting
        # solely by-activity data), then do so from
        # by_activity_data
        if (!is.null(by_person_data)) {
            # If the user entered by-person data, the data frame is smaller
            # than the by-activity data, so it's faster to pick out distinct
            # country options from here.
            print("Loading variable names from by-person data.")
            countries = by_person_data %>% distinct(SAMPLE)
        } else if (is.null(by_person_data)) {
            print("Loading variable names from by-activity data.")
            countries = by_activity_data %>% distinct(SAMPLE)
        }

        # The above line outputs a list, but we need a
        # character vector instead
        countries <- unlist(countries)
    }

    # use each country+year combination to fetch
    # subset data from the downloaded IPUMS MTUS
    # data
    for (i in 1:length(countries)) {
        # initialize 'countries' as a vector
        assign(countries[i], NULL)

        # initialize varname so it's in the correct
        # scope
        varname <- NULL
        varname = countries[i]

        if (!is.null(by_person_data)) {
            # call mtus_load_country for each country in
            # the character list and use the name of the
            # country-year sample as the new variable

            eval(call("<<-", varname, mtus_load_country(by_activity_data,
                by_person_data, countries[i])))
        } else if (is.null(by_person_data)) {
            # do the same as above, but only for
            # by-activity data, and have the variable name
            # start with a_
            a_varname = paste("a_", as.name(countries[i]), sep = "")
            print(paste("a_variable name will be", a_varname))

            eval(call("<<-", a_varname,
                      init_by_activity(by_activity_data, countries[i])))
        }

        # Recode numeric codes into categorical labels
        # if user said so, but only if the user is
        # creating a by-activity variable (in which
        # case by_person_data = NULL.

        if (recode_activities == TRUE ||
            recode_locations  == TRUE) {

            if (!is.null(by_person_data)) {
                print("Recoding only works on by-activity data. Please set the by_person_data argument to NULL to use the recoding feature.")
            } else if (is.null(by_person_data)) {
                print(c("varname is", varname))

                if (recode_activities == TRUE) {
                  eval(call("<<-", as.name(a_varname),
                    recode_internal(a_varname,
                      activities = TRUE)))
                }
                if (recode_locations == TRUE) {
                  eval(call("<<-", as.name(a_varname),
                    recode_internal(a_varname,
                      locations = TRUE)))
                }
                print("Recoded columns will be accessible in the variable preceded by a_")
            }
        }
    }
}


mtus_load_country <- function(by_activity_data,
                              by_person_data, country_code) {

    #' (Internal) Load a specific country for internal use.
    #'
    #' This function is not meant to be user-facing. It is called from within `mtus_init_countries` to count the number of trips each individual has performed. However, the user may call it (using the assignment operator `<-`) as a shortcut to filter the loaded MTUS data to just a single country-year sample, join the data, and count the number of trips. The user is encouraged to use the `mtus_init_countries(…)` procedure instead, though, because it is cleaner and less prone to user entry error.

    # country_options is a list of country-year samples available in the data.
    # The code uses <<- because country_options is meant to be global and accessible
    # by other functions (namely, check_valid_country)
    country_options <<- unique(by_person_data$SAMPLE)

    if (check_valid_country(country_code) == TRUE) {
        print(paste("Loading data from", country_code))
        country_by_activity <- filter_to_country(by_activity_data,
                                                      country_code)
        country_by_person <- filter_to_country(by_person_data,
                                                    country_code)
        country_data <- count_trips(country_by_activity,
                                         country_by_person)
        return(country_data)
    }
}
