% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mtus_init_raw_data.r
\name{mtus_init_raw_data}
\alias{mtus_init_raw_data}
\title{Load IPUMS MTUS data}
\usage{
by_activity_data <- mtus_init_raw_data("mtus_data/mtus_00009.xml")

by_person_data <- mtus_init_raw_data("mtus_data/mtus_00014.xml", FALSE)
}
\arguments{
\item{xml_file_path}{String with file path to an xml file downloaded from the IPUMS MTUS website. This xml file contains the information that the \code{ipumsr} package uses to load the data into R.}

\item{check_vars}{\code{TRUE} to check whether \code{AGE}, \code{TIME}, \code{ELOC}, \code{ACT_WORK}, and \code{MAIN} are present. \code{FALSE} to skip that step.}
}
\value{
Returns \code{mtus_data}, the IPUMS MTUS data loaded and ready for manipulation.
}
\description{
This function makes it easy for the user to import MTUS data downloaded directly from the IPUMS website. Ideally, it's the first function in the \code{gsdmtus} package that the user runs. Unlike the code suggested on the IPUMS website, this function resolves a "labels must be unique" error that may appear when loading the data. It also offers an option to check whether the user has downloaded the basic variables needed for data cleaning in other \code{gsdmtus} functions.
}
\details{
Use this function early on in your code to load by-activity and by-person data, the two primary data formats in which the MTUS data are available. You can select whether to download data by activity or by individual on the IPUMS download page. Both formats are required for full use of the \code{gsdmtus} package.
}
