## wrg_sight_id.R ##

# wrangling sightings data - 2017 to 2019, data from Bob Kinney

# libraries ---------------------------------------------------------------

library(lubridate)
library(stringr)
library(oce)
library(tidyverse)

# input -------------------------------------------------------------------

# sighting effort data
sight_df = read.csv("data/raw/visual/sightings/sightings_eff_2017-2019.csv")

# complete acoustic data
log_df = readRDS('data/processed/log.rds')

# set up ------------------------------------------------------------------

# maximum distance in km
dmax = 15

# time before and after duration
t_buffer = 60*60*1

# process -----------------------------------------------------------------

# add datetime column for sightings data
tmp = paste0(sight_df$YEAR,"-",sight_df$MONTH,"-",sight_df$DAY," ",sprintf("%06.0f", sight_df$TIME.EST.))
sight_df$datetime = as.POSIXct(tmp, format = "%Y-%m-%d %H%M%S", tz = 'EST')

# add date column to sightings data
tmp = paste0(sight_df$YEAR,"-",sight_df$MONTH,"-",sight_df$DAY)
sight_df$date = as.Date(tmp)

# change sighting data time to UTC
sight_df$datetime = as.POSIXct(sight_df$datetime, tz = "EST")
attributes(sight_df$datetime)$tzone = "UTC"

# loop in time and space
DF = vector('list', length = nrow(log_df))
for(ii in 1:nrow(log_df)){
  
  # get acou_df data
  ilat = log_df$lat[ii]
  ilon = log_df$lon[ii]
  idate = log_df$date[ii]
  itime = log_df$datetime[ii]
  idur = log_df$duration[ii]
  idep = log_df$id[ii]
  
  # subset sightings by time
  idf = sight_df %>% 
    filter(date == idate)
  
  # compute distance from sonobuoy to each whale
  idf$dist = geodDist(longitude2 = ilon, latitude2 = ilat, 
                      longitude1 = idf$LONGITUDE, latitude1 = idf$LATITUDE, alongPath = FALSE)
  
  # only sightings with in our specified range (defined as dmax)
  idf = idf %>% filter(dist <= dmax)
  
  # add deployment id
  idf$id = idep
  
  # store output
  DF[[ii]] = idf 
  
  message('Done ', ii)
}

# flatten list to data frame
df = bind_rows(DF)

# remove deployments that have no useful information
#df = df[!(df$id=="2017_noaa_DEP17"),]
#df = df[!(df$id=="2018_noaa_DEP07"),]
#df = df[!(df$id=="2018_noaa_DEP13"),]
#df = df[!(df$id=="2018_noaa_DEP14"),]

test = sight_df %>%
  filter(MONTH == '6')
