% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/init_by_activity.r
\name{filter_to_country}
\alias{filter_to_country}
\title{Filter a data set to a specific country-year sample}
\usage{
just_France <- filter_to_country(my_original_data, "FR2009")
}
\value{
\code{country_data}: the original MTUS data filtered to a single country-year sample
}
\description{
This function is a shortcut for the \code{dplyr::filter} method, with the added layer of double-checking if the country code (e.g. FR2009 or UK2014) that the user entered is available in the original MTUS data. This function works with both by-activity and by-person data.
}
