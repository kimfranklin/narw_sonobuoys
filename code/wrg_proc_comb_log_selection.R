## wrg_proc_comb_log_selection.R ##
# combining the full noaa selection table with log information

# libraries
library(tidyverse)
library(lubridate)
library(readxl)

# files
log =  read_excel('data/raw/acoustic/position/noaa_sono_positions_ALL.xlsx')

sel = readRDS('data/interim/all_noaa_selections.rds')

ofile = 'data/interim/all_noaa_acoustic.rds'

# process
# fix up log
log = log %>% 
  transmute(date = date, # this is POSIXct POSIXt
            time = time, # this is character
            lat = lat, # this is numeric
            lon = lon, # this is numeric
            yday = yday,
            week = week,
            month = month,
            id = id
            )

# merge log with selection table 
tmp = merge(x = sel, y = log, by.x = 'id', by.y = 'id', all.x = TRUE)

# save tmp file
saveRDS(tmp, ofile)
