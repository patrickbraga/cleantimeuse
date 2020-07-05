#' Recode MTUS data into human-readable labels
#'
#' Most IPUMS data uses numeric levels to indicate different values for variables (e.g. the number `8` in the `ELOC` column means an activity was performed at home). This function enables a user to recode numeric levels into human-readable labels. Please note that the user-inputted CSV file must have a specific format described below.
#'
#' @param input_data The by-activity data object that the user has already loaded. This argument may be entered either as a string inq uotes or as a data object without quotes.
#' @param which_column String denoting the column that the user wishes to recode.
#' @param codes_csv String with path to a csv file. The file must contain two columns: the first column header must match the name of the MTUS variable to be renamed (e.g. MAIN, SEC, ELOC), and the second column header must be named LABEL, and the column must contain the list of corresponding human-readable labels.
#'
#' @return Returns a version of input_data with user-designated labels replacing original IPUMS MTUS codes.

mtus_recode <- function(input_data, which_column, codes_csv) {
  # Readers of the code, beware!
  # As elsewhere in the package, this function does a lot of jumping through hoops
  # to take a user-inputted value (specifically `which_column`) and make R treat
  # it as an object name. Functions in the dplyr package don't appear to be very
  # amenable to eval(call(…)) and as.name(…), which is why I went the route of
  # renaming columns and

  # Initialize a blank tibble.
  # Fill in the tibble with the vector of ELOCs from the by-activity data.
  # Rename that column to ELOC so we can join it with the table of labels
  # Replace those with labels from the code table.
  # Use the mutate function to replace the numeric codes with human-readable categories

  # input_data is a data object; enter either with or without quotes
  # which_column is the column to recode; must enter with quotes
  # codes_csv is a path to CSV file where the first column is numeric codes
  # (the column name MUST equal that of the variable to be replaced),
  # and the second column is a verbal label (header MUST be LABEL);
  # codes_csv must be entered as a path and/or a file name, and with quotes.

  # this if-else statement allows user to enter the data object name either
  # in quotes or without quotes

  if (typeof(input_data) == "character"){
    # if type is character
    varname = input_data
    eval(call("assign", "input_data", as.name(input_data)))
  } else {
    # if type is list, transform the input_data argument into a string
    varname = deparse(substitute(input_data))
  }

  # load a csv file with equivalences
  codes_list <- read.csv(codes_csv)

  if (which_column %in% names(input_data)) {
    # replace numeric codes with categorical labels for MAIN
    temp <- NULL
    temp <- as_tibble(input_data[which_column])
    temp <- inner_join(temp, codes_list, by=which_column) %>%
      rename(RECODED = colnames(temp)[1]) %>%
      mutate(RECODED = LABEL)

    # save column location; this is so we can restore the original name
    # of the column after copying in the recoded values.
    # We use a regular expression because otherwise R will consider
    # any two variables with similar starting letters. eg SEC and SECTOR
    # will be both picked out, but we want an exact match. ^ means beginning
    # and $ means end.
    regex_which_column = paste0("^", which_column, "$")
    column_index = grep(regex_which_column, colnames(input_data))
    print(c("column index", column_index))
    # Rename the column of interest to RECODEE to make it easier to run the mutate
    input_data <- input_data %>%
      rename(RECODEE = which_column) %>%
      mutate(RECODEE = temp$RECODED)

    # Reset the name of the column
    colnames(input_data)[column_index] <- which_column
  }
  else {
    print(paste("Column", which_column, "not found in", input_data,"."))
  }

  return(input_data)
}
