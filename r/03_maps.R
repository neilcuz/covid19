
# COVID19 ANALYSIS
# 03_maps.R

# Create Maps ------------------------------------------------------------------


# Need to add in:
# UK excluding London and Shetland.
# England excluding London
# Scotland excluding Shetland

map_data_all <- bind_rows(map_data_england, map_data_scotland, map_data_wales)

map_data_all$name %>%
  unique() %>%
  walk(plot_map, x = map_data_all, area = "Scotland, Wales and England")

map_data_england$name %>%
  unique() %>%
  walk(plot_map, x = map_data_england, area = "England")

map_data_scotland$name %>%
  unique() %>%
  walk(plot_map, x = map_data_scotland, area = "Scotland")

map_data_wales$name %>%
  unique() %>%
  walk(plot_map, x = map_data_wales, area = "Wales")

map_data_london$name %>%
  unique() %>%
  walk(plot_map, x = map_data_london, area = "London")



plot_map <- function(total_case_date, x, area){
  
  data_date <- str_replace(total_case_date, "total_cases_", "")
  
  date_for_plot <- data_date %>%
    str_replace_all("_", "-") %>%
    ymd() %>%
    format("%d %B %Y")
  
  map_x <- x %>%
    filter(name == total_case_date, !is.na(rate)) %>%
    ggplot() + 
    geom_polygon(aes(x = long, y = lat, group = group, fill = rate), 
                 color = "#FFFFFF", size = 0.25) + 
    theme_minimal() + 
    coord_fixed(1) + 
    scale_fill_gradient(low = "yellow", high = "red",
                        limits = c(0, max(x$rate, na.rm = TRUE))) + 
    theme(panel.grid = element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank()) +
    labs(title = paste0("Cumulative COVID-19 postive tests in ", area),
         subtitle = date_for_plot)

  wd$output %>%
    paste0(tolower(area), "-", str_replace_all(data_date, "_", "-"), ".png") %>%
    ggsave(map_x, dpi = 400, height = 9, width = 7)
  
  return(map_x)
}
  



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

# Output -----------------------------------------------------------------------




wd$output_maps %>%
  paste0("london_", Sys.Date(), ".png") %>%
  ggsave(map_london, dpi = 400, height = 9, width = 7)

wd$output_maps %>%
  paste0("scotland_", Sys.Date(), ".png") %>%
  ggsave(map_scotland, dpi = 400, height = 9, width = 7)

