
# COVID19 ANALYSIS
# 01_setup.R

library(raster, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(janitor, warn.conflicts = FALSE)
library(rgdal, warn.conflicts = FALSE)
library(here, warn.conflicts = FALSE)
library(broom, warn.conflicts = FALSE)
library(maptools, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(rvest, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)

wd <- list()
wd$wd <- here::here() %>% paste0("/")
wd$data <- paste0(wd$wd, "data/")
wd$output <- paste0(wd$wd, "output/")
wd$r_functions <- paste0(wd$wd, "r/functions/")

invisible(
  wd$r_functions %>%
    list.files(full.names = TRUE) %>%
    map(source)
)

# Toggle this if you want your datasets to force_update 

force_update_england <- FALSE
force_update_scotland <- FALSE
force_update_wales <- FALSE

# How many days ago to compare the latest cumulative data to
num_days_lag <- 7


