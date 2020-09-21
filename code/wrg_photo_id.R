## wrg_photoid.R ##
# Combining 2017, 2018 NOAA data from the NARWC with 2019 NOAA data from NOAA 
# - Leah Crowe. Then wrangling the data so its in a useable state.

# libraries ---------------------------------------------------------------

library(readxl)
library(tidyverse)
library(stringr)
library(lubridate)
library(oce)

# input -------------------------------------------------------------------

# read in photo-id files
noa1718 = read_excel('data/raw/visual/photo-id/2020-05-20-FranklinNEFSCGSL2017-2018DatawithAssocAgeSex.xlsx')
noa19 = read.csv('data/raw/visual/photo-id/2019NOAAGSL_sightingsUPDATED.csv')

# read in processed acoustic deployment data
acou_df = readRDS('data/processed/all_noaa_acoustic.rds')

# setup -------------------------------------------------------------------

# output file for combining photo-id data
ofilea = 'data/interim/all_noaa_photoid_comb.rds'

# output file for fully wrangled photo-id data
ofileb = 'data/processed/all_noaa_photoid.rds'

# maximum km for filtering sightings
dmax = 15

# time added before and after each deployment to filter sightings
#t_buffer = 60*60*1

# process -----------------------------------------------------------------

# combining photo-id data

# check names on files
names(noa1718)
names(noa19)

# condensing dataframes and renaming columns
# for 2017 2018 data from the consortium
tmp = noa1718 %>%
  transmute(
    EGNO = SightingEGNo,
    year = SightingYear,
    month = SightingMonth,
    day = SightingDay,
    time = SightingTime,
    lat = Latitude,
    lon = Longitude,
    age = Age,
    sex = GenderCode,
    behaviour = Behaviors
  )

noa1718 = tmp

# adding datetime column
tmp = paste0(noa1718$year,"-",noa1718$month,"-",noa1718$day," ",noa1718$time)
noa1718$datetime = as.POSIXct(tmp, format = "%Y-%m-%d %H%M", tz = "Etc/GMT+4")

# place all numeric ages into categorical age groups calf is 0 years old or 
# categorized as C (C=0) juvenile is 1 to 9 years old or categorized as J 
# (1=<J<9) adult is 9 years old or older and categorized as A (9=<A) unknown 
# ages are categorized as U see email from Leah Crowe Nov 1, 2019 for reference
noa1718$age[is.na(noa1718$age)] = 'NA'

noa1718sub = noa1718

noa1718sub$age = as.numeric(noa1718sub$age)

noa1718sub$age = ifelse(noa1718sub$age<=9,"J","A")

noa1718sub = dplyr::filter(noa1718sub,  !is.na(age))

noa1718sub2 = noa1718 %>%
  filter(age %in% c("A","NA","U"))

tmp = rbind(noa1718sub, noa1718sub2)

noa1718 = tmp

# for 2019 data from NOAA (Tim ColeLeah Crowe)
tmp = noa19 %>%
  transmute(
    EGNO = EGNO,
    year = SightingYear,
    month = SightingMonth,
    day = SightingDay,
    time = SightingTime,
    datetime = as.POSIXct(DateTime, format = "%m/%d/%Y %H:%M", tz = "Etc/GMT+4"),
    lat = Latitude,
    lon = Longitude,
    age = AgeClass,
    sex = Sex,
    behaviour = Behaviors
  )

noa19 = tmp

# remove bowhead sightings
noa19 = noa19[!(noa19$EGNO=="BOWH"),]

# change NA ages to 'NA' so data can read NA as a type of age
noa19$age[is.na(noa19$age)] = 'NA'

# combining the two dataframes
id_df = rbind(noa1718, noa19) 

# create a date column
id_df$date = as.Date(id_df$datetime)

# convert datetime to UTC from EDT
# note to check what time zones r recognizes type 'OlsonNames()' in console
attributes(id_df$datetime)$tzone = "UTC"

# save id dataframe - just as a precaution
saveRDS(id_df, ofilea)

# wrangling the data

# covert acoustic data dates to Date objects
acou_df$date = as.Date(acou_df$date)

# in progress to figure out how to subset the visual data by time WORK IN PROGRESS!!!
# # identifying unique deployment dates
# dep_dates = unique(acou_df$date)
# dep_dates
# 
# # subset photo-id df to only consider dates on which sonobuoys were deployed
# id_dep = id_df %>% filter(date %in% dep_dates)
# id_dep

# subsetting acoustic data to properly subset photo-id data (have corresponding time 
# and distance ranges)
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

# loop in time and space
DF = vector('list', length = nrow(dep_df))
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

# for now leaving in all duplicates!!!!! WORK IN PROGRESS
# do via aggregate? https://stackoverflow.com/questions/16596515/aggregating-by-unique-identifier-and-concatenating-related-values-into-a-string
# remove duplictes that do not have any behaviours
#tmp = df[!(df$dup == "TRUE" & df$behaviour == 'NONE'),]
#df = tmp

#dupe = df[,c('id','EGNO')] # select columns to check duplicates
#test = df[duplicated(dupe) | duplicated(dupe, fromLast=TRUE),]
#tmp = test[!(test$behaviour == 'NONE'),]
#test = tmp

# save file
saveRDS(df, ofileb)
