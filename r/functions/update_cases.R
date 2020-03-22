
update_cases <- function(x, force_update, country){
  
  # If there is no difference between the latest cases found and yesterdays
  # cases then it discards the latest stuff unless the user forces it to do
  # otherwise. Alerts the user.
  
  if(new_cases(x) == FALSE){
    warning(paste0("No new cases in ", country))
    
    if(force_update == FALSE){
      warning("force_update = FALSE, discarding todays data")
      
      x <- x %>%
        select(-ncol(x))
      
    } else {
      warning("force_update = TRUE, keeping todays data")
    }
    
  } else {
    message(paste0("New cases found in ", country))
  }
  
  return(x)
}