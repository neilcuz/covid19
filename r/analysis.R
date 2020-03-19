
# Setup ------------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(rgdal)
library(here)
library(broom)
library(maptools)
library(lubridate)
library(rvest)

wd <- list()
wd$wd <- "C:/Users/neil_/Desktop/Files/r/covid19/"
wd$data <- paste0(wd$wd, "data/")
wd$output <- paste0(wd$wd, "output/")
wd$output_data <- paste0(wd$output, "data/")
wd$output_charts <- paste0(wd$output, "charts/")

# England ----------------------------------------------------------------------

england <- "https://www.arcgis.com/sharing/rest/content/items/b684319181f94875a6879bbc833ca3a6/data" %>%
  read_csv() %>%
  clean_names()

shape_file <- wd$data %>%
  paste0("Counties_and_Unitary_Authorities_April_2019_Boundaries_EW_BFE.shp") %>%
  readOGR()

map_data <- shape_file %>%
  broom::tidy(region= "ctyua19nm") %>%
  dplyr::left_join(england, by = c("id" = "gss_nm")) %>%
  dplyr::mutate(total_cases = if_else(id %in% england$gss_nm & is.na(total_cases), 
                                      0, total_cases)) %>%
  dplyr::filter(!is.na(gss_cd))

p <- map_data %>%
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = total_cases), 
               color = "#FFFFFF", size = 0.25) + 
  theme_minimal() + 
  coord_fixed(1) + 
  scale_fill_gradient2(low = "blue", mid = "yellow", high = "red") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title.x=element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())

wd$output %>%
  paste0("england_", Sys.Date(), ".png") %>%
  ggsave(p, dpi = 400, height = 9, width = 7)

filename <- paste0(wd$output, "england_", Sys.Date(), ".rds")

write_rds(england, filename)

# Look at the difference -------------------------------------------------------

files <- list.files(wd$output, "england", full.names = TRUE)
files <- files[files != filename]
file_previous <- files[length(files)]

england_previous <- read_rds(file_previous)
colnames(england_previous)[3] <- "total_cases_previous"

england_change <- left_join(england, england_previous) %>%
  mutate(change = total_cases - total_cases_previous,
         percentage_change = 100 * (total_cases / total_cases_previous - 1))



# London boroughs --------------------------------------------------------------

shape_file_london <- wd$data %>%
  paste0("London_Borough_Excluding_MHW.shp") %>%
  readOGR()

map_data_london <- shape_file_london %>%
  broom::tidy(region= "NAME") %>%
  dplyr::left_join(england, by = c("id" = "gss_nm")) %>%
  dplyr::filter(!is.na(gss_cd))  %>%
  dplyr::mutate(total_cases = if_else(id %in% shape_file_london$NAME & is.na(total_cases), 
                                      0, total_cases))

map_london <- map_data_london %>%
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = total_cases), 
               color = "#FFFFFF", size = 0.25) + 
  theme_minimal() + 
  coord_fixed(1) + 
  scale_fill_gradient2(low = "blue", mid = "yellow", high = "red") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title.x=element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())

wd$output %>%
  paste0("london_", Sys.Date(), ".png") %>%
  ggsave(map_london, dpi = 400, height = 9, width = 7)

# Scotland ---------------------------------------------------------------------

scotland <- "https://www.gov.scot/coronavirus-covid-19" %>%
  read_html() %>%
  html_node(xpath = '//*[@id="overview"]/table') %>%
  html_table() %>%
  as_tibble() %>%
  clean_names() %>%
  mutate(total_cases = as.numeric(positive_cases))

shape_file_scotland <- wd$data %>%
  paste0("SG_NHS_HealthBoards_2019.shp") %>%
  readOGR()

map_data_scotland <- shape_file_scotland %>%
  broom::tidy(region= "HBName") %>%
  dplyr::left_join(scotland, by = c("id" = "health_board")) 

map_scotland <- map_data_scotland %>%
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = total_cases), 
               color = "#FFFFFF", size = 0.25) + 
  theme_minimal() + 
  coord_fixed(1) + 
  scale_fill_gradient2(low = "blue", mid = "yellow", high = "red") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title.x=element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())

wd$output %>%
  paste0("scotland_", Sys.Date(), ".png") %>%
  ggsave(map_scotland, dpi = 400, height = 9, width = 7)

filename <- paste0(wd$output, "scotland_", Sys.Date(), ".rds")

write_rds(scotland, filename)
