#' (Internal) Count the number of trips by category
#'
#' Returns a `clean_data` object with columns counting different types of trips.
#'
#' @param by_activity_data By-activity data object that user has already loaded.
#' @param by_person_data By-person data object that user has already loaded.
#'
#' @return Newly created columns include the following: `work_time`, `home_work_time`, `travel_time`, [count of] `trips`, `commutes`, `nonwork_trips`, `pct_home_work`, `all_home` [i.e., work entirely from home], `all_work`, `part_home`, `work_cat`, and `full_day.`

count_trips <- function(by_activity_data,
                             by_person_data
                             ) {

    work_codes <- c("7", "8", "9", "10", "11")
    commute_code <- c("63")

    # '7' | Paid work - main job (not at home)
    # '8' | Paid work at home
    # '9' | Second or other job not at home
    # '10' | Unpaid work to generate household income
    # '11' | Travel as part of work
    # '63' | Travel to/from work

    # ELOC code 8 = travel
    clean_data <- by_activity_data %>%
        filter (MAIN %in% work_codes | ELOC == "8")

    # The clean_data and summary_data chunks of code are directly
    # derived from code originally written by Carole Voulgaris
    clean_data <- clean_data %>%
        mutate(work_time = TIME * (MAIN %in% work_codes)) %>%
        mutate(home_work_time = TIME * (MAIN == "8" | MAIN == "9")) %>%
        mutate(travel_time = TIME * (ELOC == "8")) %>%
        mutate(trips = (ELOC == "8")) %>%
        mutate(commutes = (MAIN == "63")) %>%
        mutate(nonwork_trips = (trips & !commutes))

    summary_data <- clean_data %>%
        dplyr::select(IDENT, work_time, home_work_time, travel_time,
                      trips, commutes, nonwork_trips) %>%
        group_by(IDENT) %>%
        summarise_all(sum) %>%
        filter(work_time > 0) %>%
        mutate(pct_home_work = (home_work_time)/(work_time)) %>%
        mutate(all_home = pct_home_work == 1) %>%
        mutate(all_work = pct_home_work == 0) %>%
        mutate(part_home = !all_home & !all_work) %>%
        mutate(work_cat = "1work") %>%
        mutate(full_day = work_time > 419)

    clean_data <- summary_data %>%
        left_join(by_person_data, by = "IDENT") %>%
        filter(AGE > 19 & AGE < 71)

    return(clean_data)
}
