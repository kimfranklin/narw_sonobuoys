## wrg_proc_comb_log_selection.R ##
# combining the full noaa selection table with log information

# libraries
library(tidyverse)
library(lubridate)
library(readxl)

# files
# read in log and combined selection data files
log = read_excel('data/raw/acoustic/position/noaa_sono_positions_ALL.xlsx')

sel = readRDS('data/interim/all_noaa_selections.rds')

# output file 
ofile = 'data/processed/all_noaa_acoustic.rds'

# process

# fix up log
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
tmp = merge(x = sel, y = log, by.x = 'id', by.y = 'id', all.x = TRUE)

# save tmp file
saveRDS(tmp, ofile)
