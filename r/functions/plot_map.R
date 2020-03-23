

plot_map <- function(total_case_date, x, area, folder){
  
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
                        limits = c(0, max(x$rate, na.rm = TRUE)))  +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title.y = element_text(size = 12, margin = margin(0, 15, 0, 0)),
          axis.title.x = element_text(size = 12, margin = margin(15, 0, 0, 0)),
          plot.title = element_text(size = 15, margin = margin(0, 0, 10, 0),
                                    face = "bold"),
          plot.subtitle = element_text(size = 12, margin = margin(0, 0, 10, 0)),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          plot.title.position = "plot") +
    labs(title = "Positive COVID-19 tests per 100,000 population",
         subtitle = paste0(area, ", as of ", date_for_plot),
         x = "", y = "")
  
  area_filename <- area %>% 
    tolower() %>% 
    str_replace_all(" ", "-") %>%
    str_replace_all(",", "-")
  
  wd$output %>%
    paste0(folder, "/", area_filename, "-", 
           str_replace_all(data_date, "_", "-"), ".png") %>%
    ggsave(map_x, dpi = 400, height = 9, width = 7)
  
  return(map_x)
}
