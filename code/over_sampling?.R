# over_sampling?.R

# Adding time and distance columns to dataframe to see if we are over sampling.
# The columns compare a row with the row below.

# libraries
library(tidyverse)
library(oce)

# data
df = readRDS('data/processed/proc_acou_photoid.rds')

# time between each deployment --------------------------------------------

# subset the data - because the dataframe is way to big and all columns are not necessary for this exercise
dfs = df %>%
  select(id, lat, lon, num_sighting, up, mf, gs, dep_duration, rec_duration, up_per_hr,
                 mf_per_hr, gs_per_hr, up_per_hr_per_whale, mf_per_hr_per_whale,
                 gs_per_hr_per_whale,
                 juvenile_female, juvenile_male, adult_female, adult_male,
                 foraging_bhv_whale, social_bhv_whale, other_bhv_whale,
                 datetime, date, yday, month, year)

# get the difference between each datetime in HOURS
tmp = dfs %>% 
  select(date)%>%
  mutate(time_diff_h = dfs$datetime - lag(dfs$datetime))

# drop the column not needed
drop <- c("date")
tmp = tmp[,!(names(tmp) %in% drop)]

# add difference column to the subsetted dataframe
dfs$diff_time_h = tmp

# dfs$datetime = as.POSIXct(dfs$datetime,
#            format='%m/%d/%Y %H:%M:%S')

# get the difference between each datetime in DAYS
tmp = difftime(dfs$datetime, lag(dfs$datetime), units = c('days'))

# add difference column in the subsetted dataframe
dfs$diff_time_day = tmp


# distance from each deployment -------------------------------------------

# create new column where each distance difference is calcualted using the previous distance
dfs$diff_dist = geodDist(
  longitude2 = lag(dfs$lon),
  latitude2 = lag(dfs$lat),
  longitude1 = dfs$lon,
  latitude1 = dfs$lat,
  alongPath = FALSE
)

