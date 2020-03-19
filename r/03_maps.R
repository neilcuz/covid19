
# COVID19 ANALYSIS
# 03_maps.R

# Create Maps ------------------------------------------------------------------


map_england <- map_data_england %>%
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = total_cases), 
               color = "#FFFFFF", size = 0.25) + 
  theme_minimal() + 
  coord_fixed(1) + 
  scale_fill_gradient2(low = "blue", mid = "yellow", high = "red") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())

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
  paste0("england_", Sys.Date(), ".png") %>%
  ggsave(map_england, dpi = 400, height = 9, width = 7)

wd$output_maps %>%
  paste0("london_", Sys.Date(), ".png") %>%
  ggsave(map_london, dpi = 400, height = 9, width = 7)

wd$output_maps %>%
  paste0("scotland_", Sys.Date(), ".png") %>%
  ggsave(map_scotland, dpi = 400, height = 9, width = 7)

