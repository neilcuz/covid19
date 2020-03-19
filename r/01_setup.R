
# COVID19 ANALYSIS
# 01_setup.R

# Libraries --------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(rgdal)
library(here)
library(broom)
library(maptools)
library(lubridate)
library(rvest)
library(magrittr)

wd <- list()
wd$wd <- paste0(here::here(), "/")
wd$data <- paste0(wd$wd, "data/")
wd$output <- paste0(wd$wd, "output/")
wd$output_data <- paste0(wd$output, "data/")
wd$output_maps <- paste0(wd$output, "maps/")
