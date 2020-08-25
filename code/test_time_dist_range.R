## test_time_dist_range.R ##
# RANGING, target practice, trying to see (testing) what time and distance 
# bounds should be used around the deployed sonobuoys 

# libraries
library(lubridate)
library(stringr)
library(oce)
library(ggplot2)
source('src/functions.R')

# read in the data

# sighting effort data
sight_df = read.csv("data/raw/visual/sightings/sightings_eff_2017-2019.csv")

# complete acoustic data
acou_df = read_rds("data/interim/all_noaa_acoustic.rds")


# making time column
tmp = paste0(sight_df$YEAR,"-",sight_df$MONTH,"-",sight_df$DAY," ",sprintf("%06.0f", sight_df$TIME.EST.))
sight_df$datetime = as.POSIXct(tmp, format = "%Y-%m-%d %H%M%S", tz = 'EST')


# 1. plot sonobuoy location on map (acou)
# 2. subset for distance (make this a fluid variable)(sight)
# 3. using the sono point's time (acou) subset for time (make this a fluid variable)(sight)
# 4. put whale sightings in this subset on map (sight)
# 5. put track lines in the subset on map (sight)
# 6. repeat for all sonobuoy locations