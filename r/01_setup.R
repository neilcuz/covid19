
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
wd$wd <- here::here() %>% paste0("/")
wd$data <- paste0(wd$wd, "data/")
wd$output <- paste0(wd$wd, "output/")

