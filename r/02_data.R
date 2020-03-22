
# COVID19 ANALYSIS
# 02_data.R

# Read Data --------------------------------------------------------------------

# England data provided as a csv online each data linked from gov.uk to arcgis

england_latest <- "https://www.arcgis.com/sharing/rest/content/items/b684319181f94875a6879bbc833ca3a6/data" %>%
  read_csv() %>%
  clean_names()

# Scrape Scotland data from gov.scot using rvest package

scotland_latest <- "https://www.gov.scot/coronavirus-covid-19" %>%
  read_html() %>%
  html_node(xpath = '//*[@id="overview"]/table') %>%
  html_table() %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(total_cases = as.numeric(positive_cases))%>%
  select(-positive_cases)

# Scrape Wales data from 

wales_latest <- "https://phw.nhs.wales/news/public-health-wales-statement-on-novel-coronavirus-outbreak/" %>%
  read_html() %>%
  html_node(xpath = '//*[@id="news"]/div[8]/div[2]/div/section/div[1]/div/div/table') %>%
  html_table(header = TRUE) %>%
  as_tibble() %>%
  clean_names() 

# Shape files for each geographic breakdown. England data is reported at the 
# County and Unitary Authority level. However, you can't see the small London
# boroughs in this breakdown so doing London separately too. Scotland reports
# data at the Health Board level.

shape_file_england<- wd$data %>%
  paste0("Counties_and_Unitary_Authorities_April_2019_Boundaries_EW_BFE.shp") %>%
  readOGR()

shape_file_london <- wd$data %>%
  paste0("London_Borough_Excluding_MHW.shp") %>%
  readOGR()

shape_file_scotland <- wd$data %>%
  paste0("SG_NHS_HealthBoards_2019.shp") %>%
  readOGR()

shape_file_wales <- wd$data %>%
  paste0("Local_Health_Boards_December_2016_Full_Clipped_Boundaries_in_Wales.shp") %>%
  readOGR()

# Update latest data -----------------------------------------------------------

# Get previous data

cumulative_cases_england <- wd$output %>%
  paste0("cumulative-cases-england.csv") %>%
  read_csv()

cumulative_cases_scotland <- wd$output %>%
  paste0("cumulative-cases-scotland.csv") %>%
  read_csv()

cumulative_cases_wales <- wd$output %>%
  paste0("cumulative-cases-wales.csv") %>%
  read_csv()

# Append

total_cases_col <- "total_cases_" %>%
   paste0(Sys.Date()) %>%
   str_replace_all("-", "_")

if(!(total_cases_col %in% colnames(cumulative_cases_england))){
  
  # Hackney and City of London cases reported together but separate areas and
  # Cornwall and Isles of Scilly reported together. Just duplicate the London
  # ones and we will add a note to the maps and change the other to just 
  # Cornwall and add a note
  
  england_latest_without_hackney_and_city_of_london <- england_latest %>%
    filter(!str_detect(gss_nm, "Hackney"))
  
  cumulative_cases_england <- england_latest %>%
    filter(str_detect(gss_nm, "Hackney")) %>%
    slice(c(1, 1)) %>%
    mutate(gss_nm = c("Hackney", "City of London")) %>%
    bind_rows(england_latest_without_hackney_and_city_of_london) %>%
    mutate(gss_nm = if_else(gss_nm == "Cornwall and Isles of Scilly", 
                            "Cornwall", gss_nm)) %>%
    select(gss_nm, total_cases) %>%
    set_colnames(c("gss_nm", total_cases_col)) %>%
    left_join(cumulative_cases_england, ., by = c("gss_nm")) %>%
    update_cases(force_update_england, "England")
  
  wd$output %>%
    paste0("cumulative-cases-england.csv") %>%
    write_csv(cumulative_cases_england, .)
  
} else {
  message("England already updated")
}

if(!(total_cases_col %in% colnames(cumulative_cases_scotland))){
  
  # Formatting on Ayrshire and Arran funny so force it to be consistent with
  # shape file.
  
  cumulative_cases_scotland <- scotland_latest %>%
    mutate(health_board = if_else(str_detect(health_board, "Ayr"), 
                                  "Ayrshire and Arran", health_board)) %>%
    set_colnames(c("health_board", total_cases_col)) %>%
    left_join(cumulative_cases_scotland, ., by = "health_board") %>%
    update_cases(force_update_scotland, "Scotland")
  
  wd$output %>%
    paste0("cumulative-cases-scotland.csv") %>%
    write_csv(cumulative_cases_scotland, .)
  
} else {
  message("Scotland already updated")
  
}


if(!(total_cases_col %in% colnames(cumulative_cases_wales))){
  
  # Wales data broken down by hospital now so a little cleaning required to get
  # it by health board
  
  health_boards <- wales_latest %>%
    filter(health_board != "") %>%
    select(health_board) %>%
    unlist()

  health_board_positions <- which(wales_latest$health_board %in% health_boards)
  
  for(i in seq_along(1:length(health_boards))){
    
    wales_latest <- wales_latest %>%
      mutate(health_board = if_else(row_number() >= health_board_positions[i],
                                    health_boards[i], health_board))
    
  }
  
  # They also list new cases and cumulative cases only need cumulative
  
  not_health_boards <- c("To be confirmed", "Resident outside Wales", "TOTAL")
  
  wales_latest <- wales_latest %>%
    filter(!(health_board %in% not_health_boards)) %>%
    select(-new_cases) %>%
    set_colnames(c("health_board", total_cases_col)) 

  cumulative_cases_wales <- cumulative_cases_wales %>%
    left_join(wales_latest, by = "health_board") %>%
    update_cases(force_update_wales, "Wales")
  
  # Output to keep a case record
  
  wd$output %>%
    paste0("cumulative-cases-wales.csv") %>%
    write_csv(cumulative_cases_wales, .)
  
} else {
  message("Wales already updated")
  
}

# Growths ----------------------------------------------------------------------

difference_england <- calc_difference(cumulative_cases_england, num_days_lag)
difference_scotland <- calc_difference(cumulative_cases_scotland, num_days_lag)
difference_wales <- calc_difference(cumulative_cases_wales, num_days_lag)

# Prepare map data -------------------------------------------------------------

map_data_england <- shape_file_england %>%
  broom::tidy(region= "ctyua19nm") %>%
  dplyr::left_join(cumulative_cases_england, by = c("id" = "gss_nm")) %>%
  tidyr::pivot_longer(cols = starts_with("total_cases")) 

map_data_london <- shape_file_london %>%
  broom::tidy(region= "NAME") %>%
  dplyr::left_join(cumulative_cases_england, by = c("id" = "gss_nm")) %>%
  tidyr::pivot_longer(cols = starts_with("total_cases")) 

map_data_scotland <- shape_file_scotland %>%
  broom::tidy(region= "HBName") %>%
  dplyr::left_join(cumulative_cases_scotland, by = c("id" = "health_board")) %>%
  tidyr::pivot_longer(cols = starts_with("total_cases")) 

# The Welsh health board names are given in full in the shape file but I dont 
# want these in the csv files. Will be too long names for presentation purposes

lhb16nm <- c("Betsi Cadwaladr University Health Board",
             "Powys Teaching Health Board",
             "Hywel Dda University Health Board",
             "Abertawe Bro Morgannwg University Health Board",
             "Cwm Taf University Health Board",
             "Aneurin Bevan University Health Board",
             "Cardiff and Vale University Health Board")

health_board <- c("Betsi Cadwaladr",  "Powys",  "Hywel Dda", "Swansea Bay",
                  "Cwm Taf", "Aneurin Bevan", "Cardiff and Vale")

wales_lookup <- tibble(lhb16nm, health_board)

cumulative_cases_wales_translated <- cumulative_cases_wales %>%
  left_join(wales_lookup, by = "health_board") %>%
  mutate(health_board = lhb16nm) %>%
  select(-lhb16nm)
  
map_data_wales <- shape_file_wales %>%
  tidy(region= "lhb16nm") %>%
  left_join(cumulative_cases_wales_translated, 
                   by = c("id" = "health_board")) %>%
  pivot_longer(cols = starts_with("total_cases"))


