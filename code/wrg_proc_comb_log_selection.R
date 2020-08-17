


# libraries
library(readxl)
library(tidyverse)
library(lubridate)


# read files
log =  read_excel('data/raw/acoustic/position/noaa_sono_positions_ALL.xlsx')

sel = readRDS('data/interim/all_noaa_selections.rds')


# process
tmp = as.character(log$date)
class(log$date)

# fix up log
log = log %>% 
  transmute(date = as.Date(Date, format = '%Y-%m-%d'),
            time = force_tz(time = as.POSIXct(paste0(date, ' ', `Time (ET)`), 
                                              format = '%Y-%m-%d %H:%M:%S', tz = 'US/Eastern'), 
                            tzone = 'UTC'),
            lat = as.numeric(as.character(`Lat (Deg Min)`)),
            lon = as.numeric(as.character(`Long (Deg Min)`)),
            yday = yday(time),
            week = week(time),
            month = month(time),
            year = year(time),
            id = id)

# merge log with selection table 
sel = merge(x = log, y = log, by.x = 'id', by.y = 'id', all.x = TRUE)


