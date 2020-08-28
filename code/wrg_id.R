## wrg_id.R ##
# wrangling noaa photoid so it can be merged with acoustic data

# input -------------------------------------------------------------------

# read in NOAA sightings data
id_df = readRDS("data/interim/all_noaa_photoid.rds")

# read in processed acoustic deployment data
acou_df = readRDS('data/interim/all_noaa_acoustic.rds')

# maximum km
dmax = 15

# output file
ofile = 'data/processed/all_noaa_photoid.rds'

# set up ------------------------------------------------------------------

# libraries
library(tidyverse)
library(stringr)
library(lubridate)
library(tidyr)
library(oce)
library(ggplot2)
library(splitstackshape)
library(dplyr)

# make photo-id data and acoustic data in the same time zone UTC,acoustic 
# is already in UTC so only photo-id needs to be in UTC
id_df$date = as.POSIXct(id_df$date, tz = "EST")
attributes(id_df$date)$tzone = "UTC"
id_df$datetime = as.POSIXct(id_df$datetime, tz = "EST")
attributes(id_df$datetime)$tzone = "UTC"

# identifying unique deployment dates
dep_dates = unique(acou_df$date)  

# subset photo-id df to only consider dates on which sonobuoys were deployed
id_dep = id_df %>% filter(date %in% dep_dates)

# getting the sightings within our 15 km range
dep_df = acou_df %>% 
  filter(call_type == 'START') %>%
  transmute(
    id = id,
    lat= lat,
    lon = lon, 
    date = date, 
    #time = as.POSIXct(datetime_UTC, format = "%H:%M:%S"), #as.POSIXct(strptime(datetime_UTC, format = "%Y-%m-%d %H:%M:%S"), format = "%H:%M:%S"), #as.POSIXct(datetime_UTC, format = '%H:%M:%S'), #format(datetime_UTC, "%H:%M:%S"), #format(strptime(datetime_UTC, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S"), #as.POSIXct(substr(datetime_UTC,12,19), tz = "UTC"),
    datetime = as.POSIXct(datetime_UTC, format = "%Y-%m-%d %H:%M:%S"),
    duration = (as.numeric(duration)*60*60)
  )

# remove duplicated rows
dep_df = dep_df[!duplicated(dep_df$id),]


# for changing the time range before and after sono deployment
DF = list()
t_buffer = 60*60*1

# loop in time and space
for(ii in 1:nrow(dep_df)){
  
  # get acou_df data
  ilat = dep_df$lat[ii]
  ilon = dep_df$lon[ii]
  itime = dep_df$datetime[ii]
  idur = dep_df$duration[ii]
  idep = dep_df$id[ii]
  
  # add deployment id
  idf = id_df
  idf$id = idep
  
  # subset by time
  idf = idf %>% 
    filter(datetime > itime-t_buffer & datetime < itime+idur+t_buffer)
  
  # subset by space
  
  # make new column to calculate distance from the buoy to maximum distance away
  # a whale might be 
  idf$dist = geodDist(longitude2 = ilon, latitude2 = ilat, 
                      longitude1 = idf$lon, latitude1 = idf$lat, alongPath = FALSE)
  
  # only sightings with in our specified range (defined as dmax)
  idf = idf %>%
    filter(dist<dmax)
  
  # store output
  DF[[ii]] = idf 
  
  message('Done ', ii)
}

# flatten list to data frame
df = bind_rows(DF)

# remove dead whale
df = df[-grep("FLTG DEAD", df$behaviour),]

# make NA behaviours NONE so they don't get lost
df$behaviour[is.na(df$behaviour)] = 'NONE'

# replace blank behaviour with NONE so they don't get lost
df$behaviour[df$behaviour == ''] = 'NONE'

# separate multiple behaviours into rows
tmp = separate_rows(df, behaviour, sep = ",")

# trim leading white space from split behaviours
tmp$behaviour = trimws(tmp$behaviour)

# replace tmp with df
df = tmp

# look to see which behaviour needs to be renamed 
unique(df$behaviour)
table(df$behaviour)

# names on left are now written as names on the right
df$behaviour = gsub('CALF W/ MOM', 'CALF W/MOM', df$behaviour)
df$behaviour = gsub('W/ CALF' , 'W/CALF', df$behaviour)
df$behaviour = gsub('MCSLG' , 'MCLSG', df$behaviour)
df$behaviour = gsub('AGG', 'AGG VSL', df$behaviour)

# double checking all the behaviours
unique(df$behaviour)
table(df$behaviour)

# save file
saveRDS(df, ofile)
