## filtering_noaa_sightings ##

# input -------------------------------------------------------------------

# read in NOAA sightings data
sightings_df = read.csv('data/raw/noaa_2017_2018_sightings.csv')

# read in processed acoustic deployment data
aco = readRDS('data/processed/acoustic.rds')

# maximum km
dmax = 15

# output file
ofile = 'data/processed/photo_id.rds'

# set up ------------------------------------------------------------------

# libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(oce)
library(ggplot2)
library(splitstackshape)

# identifying the sightings columns we want to use
sightings_df = sightings_df %>% 
  transmute(
    datetime = as.POSIXct(DateTime, format='%Y-%m-%d %H:%M'),
    date = as.Date(datetime),
    year = year(date),
    year_day = yday(date),
    lat = Latitude, 
    lon = Longitude,
    association_type = Association_Type,
    behaviour = Behaviors,
    EG.Letter = EG.Letter,
    EGNO = EGNO,
    sex = Sex,
    age = Age,
    age_class = AgeClass,
  )

# identifying unique deployment dates
dep_dates = unique(aco$date)  

# subset sightings df to only consider dates on which sonobuoys were deployed
sightings_dep = sightings_df %>% filter(date %in% dep_dates)

# getting the sightings within our 15 km range

dep_df = aco %>% 
  filter(call_type == 'START') %>%
  transmute(
    id = id,
    lat= lat,
    lon = lon, 
    date = date, 
    time = as.POSIXct(start_time, format = '%H:%M:%S'),
    deplyoment_duration = (deployment_duration*60*60)
  )


# remove duplicated rows
dep_df = dep_df[!duplicated(dep_df$id),]


# for changing the time range before and after sono deplyoment
DF = list()
t_buffer = 60*60*1

# loop in time and space
for(ii in 1:nrow(dep_df)){
  
  # get aco data
  ilat = dep_df$lat[ii]
  ilon = dep_df$lon[ii]
  itime = dep_df$time[ii]
  idur = dep_df$deplyoment_duration[ii]
  idep = dep_df$id[ii]
  
  # add deployment id
  idf = sightings_dep
  idf$id = idep

  # subset by time
  idf = idf %>% 
    filter(datetime > itime-t_buffer & datetime < itime+idur+t_buffer)
  
  # subset by space
  
  # make new column to claculate distace from the buoy to maximum distance away
  # a whale might be 
  idf$dist = geodDist(longitude2 = ilon, latitude2 = ilat, 
                      longitude1 = idf$lon, latitude1 = idf$lat, alongPath = FALSE)
  
  # only sightings with in our specificed range (defined as dmax)
  idf = idf %>%
    filter(dist<dmax)
  
  # store output
  DF[[ii]] = idf 
  
  message('Done ', ii)
}
  
# flatten list to data frame
df = bind_rows(DF)

# remove dead whale
df = df[-grep("FLTG DEAD, TELBUOY", df$behaviour),]

# remove deployments that have no useful information
df = df[!(df$id=="2017_noaa_DEP17"),]
df = df[!(df$id=="2018_noaa_DEP07"),]
df = df[!(df$id=="2018_noaa_DEP13"),]
df = df[!(df$id=="2018_noaa_DEP14"),]

# checking the right number of IDs are still there
unique(df$id)

# convert behaviour to character
df$behaviour = as.character(df$behaviour)
class(df$behaviour)

# separating each behaviour so it has its own row in df
#df = df %>%
  #mutate(behaviour = strsplit(as.character(behaviour), ", ")) 
#%>%unnest(behaviour)
#separate_rows(df, behaviour, convert = TRUE)
#s = strsplit(df$behaviour, split = ", ")
#dfsp = data.frame(id = rep(df$id, sapply(s, length)), behaviour = unlist(s))
# merge behaviour to df
#df = merge(x = df, y = dfsp, by.x = 'id', by.y = 'id', all.x = TRUE)
df = separate_rows(df, behaviour, sep = ", ") # < super close but it splits the spaces as well as the commas too
#df = listCol_1(cSplit_1(df, "behaviour", ", ", "long"))[]

# look to see which behaviour needs to be renamed 
unique(df$behaviour)
table(df$behaviour)
# 'HD LFT' and 'HDLFT' are the same
# 'MCLSG' and 'MCLSG ' are the same
# 'SAG' and 'SAG ' are the same
# 'SUB FD' and 'SUB FD ' and 'SUBFD' are the same
# 'TL SLH' and 'TL SLSH' are the same thing
# renaming the behaviour
df$behaviour = gsub('HDLFT' , 'HD LFT', df$behaviour)
df$behaviour = gsub('MCLSG ' , 'MCLSG', df$behaviour)
df$behaviour = gsub('SAG ' , 'SAG', df$behaviour)
df$behaviour = gsub('SUBFD' , 'SUB FD', df$behaviour)
df$behaviour = gsub('SUB FD ' , 'SUB FD', df$behaviour)
df$behaviour = gsub('TL SLH' , 'TL SLSH', df$behaviour)

# double checking all the behaviours
unique(df$behaviour)
table(df$behaviour)

# save file
saveRDS(df, ofile)
