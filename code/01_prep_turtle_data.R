## 01_prep_turtle_data.R

# script to pull:
#   - wc data (currently raw ARGOS data) from STRETCH cohort 1 (released July 2023)
#   - eventually will update to use pre-processed tracks

# save output as rds or just load straight into EDA rmd


## Load Libraries ----
library(tidyverse)
library(data.table)


## Source functions ----
# # dir depends on rmd or r
# tryCatch( { source('../code/00_northern_lats_helper_functions.R') }
#           , warning = function(w) { print("Cannot load source code (../)") })
# 
# tryCatch( { source('./code/00_northern_lats_helper_functions.R') }
#           , warning = function(w) { print("Cannot load source code (./)") })


# ## Source functions ----
source('../code/00_northern_lats_helper_functions.R')


## 1) Load Cohort Data ----
wc_files <- list.files('~/Downloads/batch/', pattern = "All.csv", recursive=T, full.names=T) # change dir later

raw_data_cohort_1 <- load_wc_downloads(wc_files) %>%
    filter(lat > 0 & lat < 60) %>% # discard extraneous loc's
    filter(date >= '2023-07-11 04:30:00') # guarantees pre-release locs aren't included

# head(raw_data_cohort_1)

## 2) Add turtle metadata ----
# load metadata
# # dir depends on rmd or r
# tryCatch( { meta_files <- "../utils/2023_Turtle_Info.xlsx" }
#           , warning = function(w) { print("Cannot load meta file (../)") })
# 
# tryCatch( { meta_files <- "./utils/2023_Turtle_Info.xlsx" }
#           , warning = function(w) { print("Cannot load meta file (./)") })


meta_files <- "../utils/2023_Turtle_Info.xlsx"
meta_cohort_1 <- load_metadata_xls(meta_files)

# merge dfs to align ID and Turtle Names
raw_data_cohort_1_w_names <- raw_data_cohort_1 %>%
    attach_metadata(., meta_cohort_1)


## 3) Calc Daily Location Avg for each ID -----
daily_avg_data_cohort_1 <- raw_data_cohort_1_w_names %>%
    calc_avg_daily_loc(., c("id", "turtle_num", "turtle_name", "date")) %>%
    mutate(lon360 = make360(lon)) %>%
    relocate("lon360", .after = "lon") %>%
    mutate(year = lubridate::year(date),
           month = lubridate::month(date),
           day = lubridate::day(date)) %>%
    mutate(group = 'cohort1') %>%
    as_tibble()
    

## 4) Load histori tracks ----
historic_tags <- load_all_tags_historic()


# print('dataframes: ')
# print('raw_data_cohort_1_w_names') 
# print('daily_avg_data_cohort_1')
# print('historic_tags')

## fin
