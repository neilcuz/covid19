
# COVID19 ANALYSIS
# 04_scotland.R

# Scotland data is released by Scottish Government broken down by health board
# area.

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

