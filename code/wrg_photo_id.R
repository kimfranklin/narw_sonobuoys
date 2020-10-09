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
log_df = readRDS('data/processed/log.rds')


# setup -------------------------------------------------------------------

# output file for combining photo-id data
ofilea = 'data/interim/all_noaa_photoid_comb.rds'

# output file for fully wrangled photo-id data
ofileb = 'data/processed/all_noaa_photoid.rds'

# maximum km for filtering sightings
dmax = 10

# time added before and after each deployment to filter sightings
t_buffer = 30*60*1

# process part I ----------------------------------------------------------
# combining photo-id data
# two different sources for photo-id data so making both data frames have the 
# same number of columns and column names are the same to merge properly 

# # check names on files
# names(noa1718)
# names(noa19)

# for 2017 2018 data from the NARWC
# condensing dataframe and renaming columns
noa1718 = noa1718 %>%
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

# adding datetime column
tmp = paste0(noa1718$year,"-",noa1718$month,"-",noa1718$day," ",noa1718$time)
noa1718$datetime = as.POSIXct(tmp, format = "%Y-%m-%d %H%M", tz = "Etc/GMT+4")

# place all numeric ages into categorical age groups calf is 0 years old or 
# categorized as C (C=0) juvenile is 1 to 9 years old or categorized as J 
# (1=<J<9) adult is 9 years old or older and categorized as A (9=<A) unknown 
# ages are categorized as U see email from Leah Crowe Nov 1, 2019 for reference

# change NA ages to 'NA' so data can read NA as a type of age (read as character)
noa1718$age[is.na(noa1718$age)] = 'NA'

# subset the noaa 2017 2018 data to convert numeric ages to categorical
noa1718sub = noa1718

# change subset ages to numeric (the numbers are read as characters which is no no)
noa1718sub$age = as.numeric(noa1718sub$age)

# change subset ages from numeric to age category
# individuals 9 or under to J for juvenile and individuals older than 9 to A for adult
noa1718sub$age = ifelse(noa1718sub$age<=9,"J","A")

# since this subset was converted to character, all the properly categorized ages got change to na's so removing na's
noa1718sub = dplyr::filter(noa1718sub,  !is.na(age))

# make another subset from noaa 2017 2018 data to obtain age categories that were properly categorized
noa1718sub2 = noa1718 %>%
  filter(age %in% c("A","NA","U"))

# combine the sightings newly converted to age category with sightings that were already categorized properly
noa1718 = rbind(noa1718sub, noa1718sub2)

# for 2019 data from NOAA (Tim Cole/Leah Crowe)
# condensing dataframe and renaming columns
noa19 = noa19 %>%
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

# remove bowhead sightings
noa19 = noa19[!(noa19$EGNO=="BOWH"),]

# change NA ages to 'NA' so data can read NA as a type of age (read as character)
noa19$age[is.na(noa19$age)] = 'NA'

# combining the two dataframes (noa1718 and noa19)
id_df = rbind(noa1718, noa19) 

# create a date column
id_df$date = as.Date(id_df$datetime)

# convert datetime from EDT to UTC 
# note to check what time zones r recognizes type 'OlsonNames()' in console
attributes(id_df$datetime)$tzone = "UTC"

# save id dataframe - just as a precaution
saveRDS(id_df, ofilea)


# process part II ---------------------------------------------------------
# wrangling the data in TIME and SPACE

# loop to subset time and space
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
  idf = id_df %>%
     filter(datetime >= itime - t_buffer & datetime <= itime + idur + t_buffer)
  
  # only calculate distances if there are whales present
  if(nrow(idf)>0){
    # compute distance from sonobuoy to each whale
    idf$dist = geodDist(longitude2 = ilon, latitude2 = ilat, 
                        longitude1 = idf$lon, latitude1 = idf$lat, alongPath = FALSE)
    
    # only sightings with in our specified range (defined as dmax)
    idf = idf %>% filter(dist <= dmax)
  } else {
    idf$dist = NA
  }
  
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
