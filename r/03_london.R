
# COVID19 ANALYSIS
# 03_london.R

# London data released alongside England data broken down by borough but 
# difficult to see on a map of its own

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
