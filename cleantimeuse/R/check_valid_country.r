#' (Internal) Check that country-year mix is an available sample
#'
#' The `SAMPLE` column of IPUMS MTUS data lists all country-year samples that are available. This function confirms that the user has entered an available country-year sample in their loaded IPUMS MTUS data.
#'
#' @param country_code The desired country-year code under the `SAMPLE` column in the MTUS data, such as "FR2009" or "UK2014".
#' @return `TRUE` if the country-year sample is available.
#' @return `FALSE` if the sample isn't in the selected data; if so, the package displays a list of available countries.

check_valid_country <- function(country_code) {
    # Confirm that user has entered an available
    # country and year mix

    if (country_code %in% country_options) {
        return(TRUE)
    } else {
        print(c("Country/year mix not available. Please select from the following:",
            country_options))
        return(FALSE)
    }
}
