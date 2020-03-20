
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
# https://phw.nhs.wales/news/public-health-wales-statement-on-novel-coronavirus-outbreak/



# Shape files for each geographic breakdown. England data is reported at the 
# County and Unitary Authority level. However, you can't see the small London
# boroughs in this breakdown so doing London separately too. Scotland reports
# data at the Health Board level.

shape_file <- wd$data %>%
  paste0("Counties_and_Unitary_Authorities_April_2019_Boundaries_EW_BFE.shp") %>%
  readOGR()

shape_file_london <- wd$data %>%
  paste0("London_Borough_Excluding_MHW.shp") %>%
  readOGR()

shape_file_scotland <- wd$data %>%
  paste0("SG_NHS_HealthBoards_2019.shp") %>%
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
  # ones and we will add a note to the maps and change the other to Cornwall
  
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
    left_join(cumulative_cases_england, ., by = c("gss_nm"))
}


if(!(total_cases_col %in% colnames(cumulative_cases_scotland))){
  
  # Formatting on Ayrshire and Arran funny so force it to be consistent with
  # shape file.
  
  cumulative_cases_scotland <- scotland_latest %>%
    set_colnames(c("health_board", total_cases_col)) %>%
    left_join(cumulative_cases_scotland, ., by = "health_board")
}

if(!(total_cases_col %in% colnames(cumulative_cases_wales))){
  cumulative_cases_wales <- wales_latest %>%
    set_colnames(c("health_board", total_cases_col)) %>%
    left_join(cumulative_cases_wales, ., by = "health_board")
}

write_csv(scotland_latest, paste0(wd$output, "hello.csv"))
xx <- tibble(shape_file_london$NAME, shape_file_london$GSS_CODE)



# Output
# City of London - investigate (Hackney and City of London?)
# Isles of Scilly - remove


wd$output_data %>%
  paste0("cumulative-cases-england.csv") %>%
  write_csv(cumulative_cases_england, .)

wd$output_data %>%
  paste0("cumulative-cases-scotland.csv") %>%
  write_csv(cumulative_cases_scotland, .)

# Prepare map data -------------------------------------------------------------

map_data_england <- shape_file %>%
  broom::tidy(region= "ctyua19nm") %>%
  dplyr::left_join(cumulative_cases_england, by = c("id" = "gss_nm")) %>%
  tidyr::pivot_longer(cols = starts_with("total_cases")) %>%
  distinct(id, .keep_all = TRUE)

map_data_london <- shape_file_london %>%
  broom::tidy(region= "NAME") %>%
  dplyr::left_join(cumulative_cases_england, by = c("id" = "gss_nm")) %>%
  dplyr::filter(!is.na(gss_cd))  %>%
  dplyr::mutate(total_cases = if_else(id %in% shape_file_london$NAME & is.na(total_cases), 
                                      0, total_cases))

map_data_scotland <- shape_file_scotland %>%
  broom::tidy(region= "HBName") %>%
  dplyr::left_join(cumulative_cases_latest, by = c("id" = "health_board")) 


xx <- tibble(shape_file_london$NAME)
# 
# 
# england <- "https://www.arcgis.com/sharing/rest/content/items/b684319181f94875a6879bbc833ca3a6/data" %>%
#   read_csv() %>%
#   clean_names()
# 
# wd$output_data %>% list.files(full.names = T)
# 
# england_previous <- read_rds("C:/Users/neil_/Desktop/Files/r/covid19/output/data/england_2020-03-17.rds")
# england_previous <- rename(england_previous, id = gss_cd, area = gss_nm, 
#                            cumulative_cases = total_cases) %>%
#   mutate(new_cases = )
# 





