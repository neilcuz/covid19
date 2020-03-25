library(shiny)

line_data <- map_data_all %>%
  distinct(id, name, value, population, rate, .keep_all = TRUE) %>%
  mutate(area = id,
         date = str_replace(name, "total_cases_", ""),
         date = str_replace(date, "_", "-"),
         date = ymd(date)) %>%
  select(area, date, value, rate) %>%
  pivot_longer(cols = value:rate, names_to = "measurement") %>%
  mutate(measurement = str_replace(measurement, "value", "cases"),
         value = if_else(is.na(value), 0, value))

area_input <- line_data %>% 
  select(area) %>% 
  distinct(area) %>% 
  unlist() %>%
  unname()

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Confirmed COVID-19 cases in the UK."),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    selectInput("area", "Choose an area:", area_input),
    checkboxInput("measurement", "Per capita", FALSE),
    checkboxInput("outliers", "Trim outliers", TRUE)
  ),
  
  mainPanel(
    plotOutput("case_trajectories")
  )
))