# Declare survey objects to fill in definition function below
svy_workers <- NULL
svy_all_home <- NULL
svy_all_work <- NULL
svy_part_home <- NULL

def_survey_objects <- function(workers) {
  #' Define survey objects for analysis and graphing
  #'
  #' This procedure prepares four preset survey objects that are useful for exploratory data analysis and incorporate survey weights. Rather than returning values, this procedure makes four global survey-design objects available and ready for actions such as plotting graphs.
  #'
  #' @param workers By-person data that the user has already loaded.
  #'
  #' @return `svy_workers`: All individuals who work.
  #' @return `svy_all_home`: Workers who work entirely from home.
  #' @return `svy_all_work`: Workers who work entirely away from home.
  #' @return `svy_part_home`: Workers who work partly from home.

  # Create survey design objects to incorporate survey weights
  # Below I used <<- because the svy_* variables
  # are global rather than local. When I tried using
  # just <- to assign them values, I got an error tha
  # I was trying to apply svymean to a variable of NULL class.
  svy_workers <<- workers %>%
    as_survey_design(ids = IDENT, weight = PROPWT)
  svy_all_home <<- workers[workers$all_home,] %>%
    as_survey_design(ids = IDENT, weight = PROPWT)
  svy_all_work <<- workers[workers$all_work,] %>%
    as_survey_design(ids = IDENT, weight = PROPWT)
  svy_part_home <<- workers[workers$part_home,] %>%
    as_survey_design(ids = IDENT, weight = PROPWT)
}
