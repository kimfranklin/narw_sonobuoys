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

# saving output file for the full complete acousitc data set
ofileb = 'data/processed/all_noaa_acoustic.rds'

# process -----------------------------------------------------------------

# combining selection tables 
# find all years 
flista = list.files(path = idir, pattern = '20*', full.names = TRUE)

# list data files
flist = list.files(path = paste0(flista,'/noaa/processed'), pattern = '*_selections.txt$', full.names = TRUE)

# define empty list to hold selections
DFa = vector('list',length = length(flista))
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
df = df[!(df$id=="2017_noaa_DEP17"),]
df = df[!(df$id=="2018_noaa_DEP07"),]
df = df[!(df$id=="2018_noaa_DEP13"),]
df = df[!(df$id=="2018_noaa_DEP14"),]
df = df[!(df$id=="2019_noaa_DEP75b"),]
df = df[df$id !='2017_noaa_DEP17',]

# save all selection tables combined dataframe - just as a precaution 
saveRDS(df, file = ofilea)

# combining log with all selections
# fix up log data
log = log %>% 
  transmute(date = date, # this is POSIXct POSIXt
            # datetime = time, # this is character
            # time_UTC_correct_from_Tim_Cole_gps_records = format(as.POSIXct(`time UTC`, format= "%Y-%m-%d %H:%M:%S"),
            #                   format = "%H:%M:%S"),
            datetime_UTC = datetime_UTC_correct,
            lat = lat, # this is numeric
            lon = lon, # this is numeric
            yday = yday,
            week = week,
            month = month,
            id = id,
            duration = duration
  )

# merge log with selection table 
tmp = merge(x = df, y = log, by.x = 'id', by.y = 'id', all.x = TRUE)
df = tmp

# save selections combined with log dataframe (the df file) 
saveRDS(df, ofileb)
