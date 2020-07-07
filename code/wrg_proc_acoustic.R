## proc_acoustic
# combine_logs and process_selection_tables 
# combing the combine_logs and yearly selection.rda files

# input -------------------------------------------------------------------

# the over all metadata file for all 2017 and 2018 noaa deployments
log_file = 'data/interim/sono_log.rds'

# processsed 2017 acoustic data
selections_2017_file = 'data/interim/noaa_2017_selections.rds'

# processed 2018 acoustic data
selections_2018_file = 'data/interim/noaa_2018_selections.rds'

# outfile name 
ofile = 'data/processed/acoustic.rds'

# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)

# process -----------------------------------------------------------------

log = readRDS(log_file) 
selections_2017 = readRDS(selections_2017_file)
selections_2018 = readRDS(selections_2018_file)

acoustic = rbind(selections_2017, selections_2018)

acoustic$time = as.POSIXct(x = paste0(acoustic$Begin.Date, ' ', acoustic$Begin.Clock.Time), 
                           format = '%Y/%m/%d %H:%M:%S', tz = 'America/Halifax')

acoustic$year = year(acoustic$time)

unique(acoustic$year) # remove when fix 2018 dep 16 

# merge

df = merge(x = acoustic, y = log, by.x = 'id', by.y = 'id', all.x = TRUE)

# selection the important/ useful columns 

df = df %>% 
  transmute(
    id = id, 
    selection = Selection, 
    start_time = as.POSIXct(paste0(Begin.Date, ' ', Begin.Clock.Time), 
                            format='%Y/%m/%d %H:%M:%S'), 
    end_time = as.POSIXct(paste0(Begin.Date, ' ', End.Clock.Time), 
                          format='%Y/%m/%d %H:%M:%S'), 
    date = as.Date(start_time),
    year = year(date),
    month = month(date),
    year_day = yday(date),
    platform = platform,
    call_type = call_type,
    score = score, 
    lat = lat, 
    lon = lon,
    deployment_duration = duration
  )

df$deployment_duration = df$deployment_duration/60/60

# save the dataframe

saveRDS(df, file = ofile)
