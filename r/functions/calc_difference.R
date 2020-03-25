
#' Calculate Difference
#' 
#' This function calculates the difference in the number of cases in the latest
#' period and a period n days prior to this
#' 
#' @param x a tibble containing an area and total cases in columns with column
#' names in the format total_cases_yyyy_mm_dd
#' 
#' @param num_days_lag number of days lag, if the number specified lags the data
#' greater than the number of available days of data then the difference is 
#' calculated between the latest data and the earliest data

calc_difference <- function(x, num_days_lag){
  
  x_data <- select(x, contains("total_cases"))
  num_cols <- ncol(x_data)
  
  if(ncol(x_data) > num_days_lag){
    x_data <- select(x_data, (num_cols - num_days_lag + 1):num_cols)
  }
  
  difference_in_cases <- (x_data[,num_cols] - x_data[,1]) %>%
    set_colnames("difference_in_cases")
  
  x <- x %>%
    select(-contains("total_cases")) %>%
    bind_cols(difference_in_cases)
  
  return(x)
}
