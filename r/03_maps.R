
# COVID19 ANALYSIS
# 03_maps.R

# Create Maps of Rate Per 100k -------------------------------------------------

# Whole UK

map_data_all$name %>%
  unique() %>%
  walk(plot_map, x = map_data_all, area = "UK Excluding Northern Ireland", 
       folder = "maps-uk")

map_data_all$name %>%
  unique() %>%
  walk(plot_map, x = map_data_all_excl, 
       area = "UK Excluding Northern Ireland, London Boroughs and Shetland", 
       folder = "maps-uk")

# England

map_data_england$name %>%
  unique() %>%
  walk(plot_map, x = map_data_england, area = "England")

map_data_england_excl$name %>%
  unique() %>%
  walk(plot_map, x = map_data_england_excl, 
       area = "England Excluding London Boroughs", folder = "maps-england")

map_data_london$name %>%
  unique() %>%
  walk(plot_map, x = map_data_london, area = "London", folder = "maps-england")

# Scotland

map_data_scotland$name %>%
  unique() %>%
  walk(plot_map, x = map_data_scotland, area = "Scotland", 
       folder = "maps-scotland")

map_data_scotland_excl$name %>%
  unique() %>%
  walk(plot_map, x = map_data_scotland_excl, 
       area = "Scotland Excluding Shetland", folder = "maps-scotland")

# Wales

map_data_wales$name %>%
  unique() %>%
  walk(plot_map, x = map_data_wales, area = "Wales",  folder = "maps-wales")
