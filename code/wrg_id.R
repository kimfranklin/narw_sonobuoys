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
ofile = 'data/processed/wrg_all_noaa_photoid.rds'

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
    time = as.POSIXct(datetime, format = '%H:%M:%S', tz = "EST"),
    duration = (as.numeric(duration)*60*60)
  )

# remove duplicated rows
dep_df = dep_df[!duplicated(dep_df$id),]


# for changing the time range before and after sono deplyoment
DF = list()
t_buffer = 60*60*1

# loop in time and space
for(ii in 1:nrow(dep_df)){
  
  # get acou_df data
  ilat = dep_df$lat[ii]
  ilon = dep_df$lon[ii]
  itime = dep_df$time[ii]
  idur = dep_df$duration[ii]
  idep = dep_df$id[ii]
  
  # add deployment id
  idf = id_df
  idf$id = idep
  
  # subset by timeFIXFIXFIXFIXFIXFIXFIXFIXFIXFIXFIXFIXFIXFIXFIXFIXFIX
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