
library(shiny)

sigma_max <- 6

shinyServer(function(input, output) {

  output$case_trajectories <- renderPlot({
    
    areas_keep <- unique(line_data$area)
    
    if(input$measurement == TRUE){
      chosen_measurement <- "rate"
      y_lab <- "Cases per 100,000 residents"
      
      if(input$outliers == TRUE){
        
        mu <- mean(line_data$value, na.rm = TRUE)
        sigma <- stats::sd(line_data$value, na.rm = TRUE)
        
        areas_keep <- mutate(line_data, z = (value - mu)/sigma) %>%
          group_by(area) %>%
          summarise(max_z = max(z, na.rm = TRUE)) %>%
          filter(max_z <= sigma_max) %>%
          select(area) %>%
          unlist()
        
        line_data <- filter(line_data, area %in% areas_keep)
      }
      
    } else {
      chosen_measurement <- "cases"
      y_lab <- "Total number of confirmed cases"
    }
    
    line_data <- filter(line_data, measurement == chosen_measurement) 
    
    line_data_area <- filter(line_data, area == input$area)
    
    latest_date <- line_data_area %>%
      arrange(desc(date)) %>%
      slice(1) %>%
      select(date) %>%
      unlist()
    
    line_data_area <- mutate(line_data_area, latest_date = ymd(latest_date))
    
    p <- line_data %>%
      ggplot(aes(x = date, y = value, group = area)) +
      geom_line(colour = "#D3D3D3") +
      theme_bw() +
      theme(panel.grid = element_blank(),
            panel.grid.major.y = element_line(colour = "#EADDDD"),
            axis.text = element_text(size = 12),
            axis.title.x = element_text(size = 12, margin = margin(15, 0, 0, 0)),
            axis.title.y = element_text(size = 12, margin = margin(0, 15, 0, 0)),
            plot.title = element_text(size = 16, margin = margin(15, 0, 0, 0),
                                      face = "bold"),
            plot.subtitle = element_text(size = 12, margin = margin(0, 0, 5, 0)),
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            plot.title.position = "plot") 
    
    if(input$outliers == TRUE & !(input$area %in% areas_keep)){
        p <- p +
          labs(title = "Confirmed COVID-19 case trajectories across the UK by reporting area",
               subtitle = paste0(input$area, 
                                 " is an outlier and has been removed. Untick the box."),
               x = "", y = y_lab)
    } else {
      p <- p +
        labs(title = "Confirmed COVID-19 case trajectories across the UK by reporting area",
             x = "", y = y_lab) +
        geom_line(data = line_data_area, aes(x = date, y = value), 
                  colour = "#CA372D", size = 2)     
    }
    
    p
  })
})