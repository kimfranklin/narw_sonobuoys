## wrg_acoustic.R ##
# Combining all selection tables produced from analyzed NOAA acoustic recordings 
# in Raven. Then combining this master acoustic data set with corresponding log 
# of all sonobuoy deployment positions and times.

# libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(readxl)


# input -------------------------------------------------------------------

# defines input directory
idir = 'data/raw/acoustic'

# defines output directory
odir = 'data/interim/'

# defining characteristic for output file
char = 'all_noaa_'

# master log file - edited version
log = read_excel('data/raw/acoustic/position/noaa_sono_positions_ALL.xlsx')


# setup -------------------------------------------------------------------

# saving output file for selection tables 
ofilea = paste0(odir, char, 'selections.rds')

# saving output file for the full complete acoustic data set
ofileb = 'data/processed/all_noaa_acoustic.rds'

# saving log file for easy access
ofilec = 'data/processed/log.rds'
  

# process part I ----------------------------------------------------------
# combining selection tables 

# find all years 
flista = list.files(path = idir, pattern = '20*', full.names = TRUE)

# list data files
flist = list.files(path = paste0(flista,'/noaa/processed'), pattern = '*_selections.txt$', full.names = TRUE)

# define empty list to hold selections
DF = vector('list',length = length(flist))

# loop through and read in selection tables
for(ii in seq_along(flist)){
  
  # define input file
  ifile = flist[ii]
  
  # read selection table
  tmp = read.delim(file = ifile, sep = '\t', as.is = TRUE)
  
  # isolate deployment name
  dname = gsub(pattern = '_selections.txt', replacement = '', x = basename(ifile))
  
  # adding column of deployment information
  tmp$sono_id = dname
  
  # isolate year
  yname = as.character(strsplit(ifile,"/")[[1]][4])
  
  # adding year column
  tmp$year= yname
  
  # isolate platform name
  pname = as.character(strsplit(flist,"/")[[1]][5])
  
  # add column with platform name
  tmp$platform = pname
  
  # add selection table to list
  DF[[ii]] = tmp
}

# combine all selection tables into one df
df = bind_rows(DF)

# editing the id column (this is to match the logs formating)
df$id = paste0(df$year,'_', df$platform,'_', df$sono_id)

# remove deployments that have no useful information
# note: 2017_noaa_DEP17 and 2018_noaa_DEP07 do not have high scoring calls and were not included in thesis data
df = df[!(df$id=="2019_noaa_DEP75b"),] # still recording when went back to same area

# save all selection tables combined dataframe - just as a precaution 
saveRDS(df, file = ofilea)


# process part II ---------------------------------------------------------
# combining log information with all selections

# get recording duration from selection tables
# separate start and end call types from call data
dur_calc = df %>%
  filter(call_type %in% c('START','END'))

# get the difference between each start and end time
dur_calc = dur_calc %>%
  mutate(duration = End.Time..s. - lag(Begin.Time..s.))

# set na duration to 0
dur_calc$duration[is.na(dur_calc$duration)] = 0

# set all START call types durations to 0; we don't want to include the time where there's no difar/blacking out
dur_calc$duration[dur_calc$call_type == 'START'] <- 0

# sum the durations by deployment id
tmp = aggregate(dur_calc$duration, by=list(id = dur_calc$id), FUN=sum)

# change duration column name
colnames(tmp)[which(names(tmp) == "x")] <- "rec_duration"

# fix up log data
log = log %>% 
  transmute(date = as.Date(date), # this is POSIXct POSIXt
            datetime = datetime_UTC_correct,
            lat = lat, # this is numeric
            lon = lon, # this is numeric
            yday = yday,
            week = week,
            month = month,
            id = id,
            deploy_sucess = deploy_sucess
  )

# combine calculated recording duration to log
log = merge(x = log, y = tmp, by.x = 'id', by.y = 'id', all.x = TRUE)

# get deployment duration 
# filter start and end times for each deployment and calculate the difference
dep_dur = df %>%
  filter(call_type %in% c('START', 'END')) %>%
  group_by(id) %>%
  summarize(difference = last(End.Time..s.) - first(Begin.Time..s.))

# rename difference as dep_duration
dep_dur = dep_dur %>%
  rename(dep_duration = difference)

# combine calculated deployment duration to log
log = merge(x = log, y = dep_dur, by.x = 'id', by.y = 'id', all.x = TRUE)

# remove deployments  not useful
# note: 2017_noaa_DEP17 and 2018_noaa_DEP07 do not have high scoring calls and were not included in thesis
log = log[!(log$id=="2019_noaa_DEP75b"),] # still recording when went back to same area

# remove deployments not successful
log = log %>%
  filter(deploy_sucess == 'yes')

# only take deployments that happened in June, July, August
log = log %>%
  filter(month %in% c('6', '7', '8'))

# take deployments longer than half an hour
log = log %>%
  filter(rec_duration >= 900)

# save changes to log
saveRDS(log, ofilec)

# merge log with selection table 
df = merge(x = df, y = log, by.x = 'id', by.y = 'id', all.x = TRUE)

# save selections combined with log dataframe (the df file) 
saveRDS(df, ofileb)
