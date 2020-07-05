
init_by_activity <- function(by_activity_data,
                                  country_code) {
  #' (Internal) Load by-activity data
  #'
  #' This function is not meant to be user-facing. It is called within the user-facing `mtus_init_countries` function to create by-activity data objects; in `mtus_init_countries`, by-activity objects automatically start with the prefix `a_`, such as `a_FR2009`. This is distinct from `FR2009`, which is the convention used for cleaned data joining by-activity and by-person data.
  #'
  #' The function first creates a character list of all available country-year samples by fetching all unique values from the `SAMPLE` column of the data set. It then runs `check_valid_country(…)` to ensure that the `country_code` argument is valid. If so, then it calls filters `by_activity_data` to just
  #'
  #' @param by_activity_data The by-activity data that the user loaded earlier using `mtus_init_raw_data`.
  #' @param country_code A country-year sample available within the data. Because this involves manually typing a code (e.g. "UK2014"), the user is instead encouraged to use `mtus_init_countries` to avoid entry errors.
  #' @return `country_by_activity`: the by-activity data filtered to a specific country-year sample

  country_options <<- unique(by_activity_data$SAMPLE)

  if (check_valid_country(country_code) == TRUE) {
    print (paste("Loading by-activity data from",
                 country_code))

    country_by_activity <- filter_to_country(by_activity_data,
                                                  country_code)
    return(country_by_activity)
  }
}

filter_to_country <- function(mtus_data, country_code) {
  #' Filter a data set to a specific country-year sample
  #'
  #' This function is a shortcut for the `dplyr::filter` method, with the added layer of double-checking if the country code (e.g. FR2009 or UK2014) that the user entered is available in the original MTUS data. This function works with both by-activity and by-person data.
  #'
  #' @usage just_France <- filter_to_country(my_original_data, "FR2009")
  #'
  #' @return `country_data`: the original MTUS data filtered to a single country-year sample
  if (check_valid_country(country_code) == TRUE) {
    country_data <- dplyr::filter(mtus_data, SAMPLE == country_code)
    return(country_data)
  }
}

