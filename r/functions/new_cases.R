
new_cases <- function(x){
  
  # Calculate the difference between the latest cases found and yesterdays
  # cases found
  
  num_new <- x %>%
    select((ncol(x) -1):ncol(x)) %>%
    set_colnames(c("yesterday", "today")) %>%
    mutate(difference = today - yesterday) %>%
    filter(difference != 0) %>%
    nrow()
  
  if(num_new > 0){
    return(TRUE)
  } else {
    return(FALSE)
  }
}