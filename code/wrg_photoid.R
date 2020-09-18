## wrg_id.R ##
# wrangling noaa photoid so it can be merged with acoustic data

# input -------------------------------------------------------------------

# read in NOAA sightings data
id_df = readRDS("data/interim/all_noaa_photoid_comb.rds")

# read in processed acoustic deployment data
acou_df = readRDS('data/processed/all_noaa_acoustic.rds')

# maximum km
dmax = 15

# output file
ofile = 'data/processed/all_noaa_photoid.rds'

# set up ------------------------------------------------------------------

# libraries
library(tidyverse)
library(stringr)
library(lubridate)
library(oce)

# make photo-id data and acoustic data in the same time zone UTC,acoustic 
# is already in UTC so only photo-id needs to be in UTC
id_df$date = as.POSIXct(id_df$date, tz = "EST")
attributes(id_df$date)$tzone = "UTC"
id_df$datetime = as.POSIXct(id_df$datetime, tz = "EST")
attributes(id_df$datetime)$tzone = "UTC"

# covert dates to Date objects (not POSIX)
id_df$date = as.Date(id_df$date)
acou_df$date = as.Date(acou_df$date)

# identifying unique deployment dates
#dep_dates = unique(acou_df$date)
#dep_dates

# subset photo-id df to only consider dates on which sonobuoys were deployed
#id_dep = id_df %>% filter(date %in% dep_dates)
#id_dep

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
#DF = list()
#t_buffer = 60*60*1
DF = vector('list', length = nrow(dep_df))

# loop in time and space
for(ii in 1:nrow(dep_df)){
  
  # get acou_df data
  ilat = dep_df$lat[ii]
  ilon = dep_df$lon[ii]
  idate = dep_df$date[ii]
  itime = dep_df$datetime[ii]
  idur = dep_df$duration[ii]
  idep = dep_df$id[ii]
  
  # subset sightings by time
  idf = id_df %>% 
    filter(date == idate)
  
  # compute distance from sonobuoy to each whale
  idf$dist = geodDist(longitude2 = ilon, latitude2 = ilat, 
                      longitude1 = idf$lon, latitude1 = idf$lat, alongPath = FALSE)
  
  # only sightings with in our specified range (defined as dmax)
  idf = idf %>% filter(dist <= dmax)
  
  # add deployment id
  idf$id = idep
  
  # identify duplicates
  idf$dup = duplicated(idf$EGNO)
  
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

# make NA sex NONE so they don't get lost
df$sex[is.na(df$sex)] = 'NONE'

# replace blank sex with NONE so they don't get lost
df$sex[df$sex == ''] = 'NONE'

dfs = df %>%
  filter(dup)

# for now leaving in all duplicates!!!!! 
# do via aggregate? https://stackoverflow.com/questions/16596515/aggregating-by-unique-identifier-and-concatenating-related-values-into-a-string
# remove duplictes that do not have any behaviours
#tmp = df[!(df$dup == "TRUE" & df$behaviour == 'NONE'),]
#df = tmp

#dupe = df[,c('id','EGNO')] # select columns to check duplicates
#test = df[duplicated(dupe) | duplicated(dupe, fromLast=TRUE),]
#tmp = test[!(test$behaviour == 'NONE'),]
#test = tmp

# save file
saveRDS(df, ofile)
