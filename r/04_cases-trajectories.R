
# COVID19 ANALYSIS
# 04_case-trajectories.R

case_trajectory_england <- cumulative_cases_england %>%
  select(-gss_cd) 
colnames(case_trajectory_england)[1] <- "area"

case_trajectory_scotland <- cumulative_cases_scotland
colnames(case_trajectory_scotland)[1] <- "area"

case_trajectory_wales <- cumulative_cases_wales
colnames(case_trajectory_wales)[1] <- "area"

trajectory_data_all <- bind_rows(case_trajectory_england, case_trajectory_wales,
                                 case_trajectory_scotland)

start_cases <- 100

cases_split <- trajectory_data_all %>%
  pivot_longer(cols = -area) %>%
  filter(value >= start_cases) %>%
  arrange(area, name) %>%
  group_split(area) 

names(cases_split) <- cases_split %>% map(1) %>% unlist() %>% unique()

cases_split <- cases_split %>% 
  map(select, value) %>%
  map(unlist) 

case_lengths <- cases_split %>% map_dbl(length)
max_area <- names(which(case_lengths == max(case_lengths)))

series_list <- cases_split %>%
  map(~c(.x, rep(NA, max_length - length(.x)))) %>%
  map(~c(start_cases, .x))

trajectory_chart_data <- series_list %>%
  as_tibble() %>%
  mutate(day = 0:(n() - 1)) %>%
  pivot_longer(cols = -day) %>%
  mutate(value_log = log(value)) %>%
  filter(!is.na(value))

chart_colours <- rep("#D3D3D3", length(series_list)) %>%
  set_names(names(series_list))

chart_labels <- seq(100, max(trajectory_chart_data$value), 25)
chart_breaks <- log(chart_labels)

p_trajectories <- trajectory_chart_data %>%
  ggplot(aes(x = day, y = value_log, group = name, colour = name)) +
  scale_color_manual(values = chart_colours) +
  geom_line()  +
  guides(colour = FALSE) +
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
        plot.title.position = "plot") +
  labs(title = "Confirmed COVID-19 case trajectories across the UK by reporting area",
       subtitle = "Excludes Northern Ireland. Scotland and Wales by health board, England by UTLA.",
       x = paste0("Days since ", start_cases, " cases recorded"),
       y = "Cumulative COVID-19 postive cases") +
  scale_y_continuous(breaks = chart_breaks, labels = chart_labels)

