
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
