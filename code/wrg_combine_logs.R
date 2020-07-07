## combine_logs ##
# combine logs from noaa twin otter and dal jd martin

# user input --------------------------------------------------------------

# dal file
#dfile = 'data/interim/jdmartin_2019_deployment_combined_log.csv'

# noaa file
nfile = 'data/interim/position/NERW_Sonobuoy_Log_MASTER.csv'

# load in the acoustic recordings duration times
duration_times = readRDS('data/interim/duration_times.rds')

# output file
out_file = 'data/interim/sono_log.rds'

# setup -------------------------------------------------------------------

# libraries
library(tidyverse)
library(lubridate)

# format dal data ---------------------------------------------------------

# read in dal data
# dal = read_csv(dfile)

# format
# dal = dal %>%
#    transmute(
#      date = as.Date(date, format = '%d-%b-%y'),
#      time = as.POSIXct(time_utc, format = '%y-%m-%d %H:%M', tz = 'UTC'),
#      lat,
#      lon,
#      yday = yday(time),
#      week = week(time),
#      month = month(time),
#      year = year(time),
#      platform_type = 'vessel',
#      platform_name = 'jdmartin',
#      sono_number,
#      sono_id = paste(year, platform_name, sono_number, sep = '_'),
#      deploy_success,
#      phone_depth,
#      sonobuoy_type = type,
#      recording_mode,
#      serial_number,
#      channel,
#      scuttle_time,
#      comments
#    )

# format noaa data --------------------------------------------------------

# read in data
noa = read.csv(nfile)

# remove first lines (flights in GOM)
noa = noa[-c(1:15),]

# convert to station number
noa$Station = gsub(replacement = '', pattern = '18-', noa$Station)
noa$Station = gsub(replacement = '', pattern = '18_', noa$Station)
noa$Station = as.character(noa$Station)

# format
noa = noa %>%
  transmute(
    date = as.Date(Date, format = '%d-%b-%y'),
    time = force_tz(time = as.POSIXct(paste0(date, ' ', `Time (ET)`), 
                                      format = '%Y-%m-%d %H:%M:%S', tz = 'US/Eastern'), 
                    tzone = 'UTC'),
    lat = as.numeric(as.character(`Lat (Deg Min)`)),
    lon = as.numeric(as.character(`Long (Deg Min)`)),
    yday = yday(time),
    week = week(time),
    month = month(time),
    year = year(time),
    platform_type = 'plane',
    platform_name = 'noaa',
    sono_number = paste0('DEP', Station),
    id = paste(year, platform_name, sono_number, sep = '_'),
    deploy_success = `Deploy Success?`,
    phone_depth = 90,
    sonobuoy_type = '53F',
    recording_mode = toupper(Mode),
    serial_number = `Sonobuoy Lot`,
    channel = Channel,
    scuttle_time = 8,
    comments = Comments
  )

# select only actual positions (without dups)
noa = noa[grepl(x = noa$comments, pattern = 'ap', ignore.case = TRUE) &
            !grepl(x = noa$comments, pattern = 'dup', ignore.case = TRUE),]

# remove duplicates
noa = noa[!duplicated(noa$id),]

# merge the acoustic recording duration with log by id
noa = merge(x = noa, y = duration_times, by.x = 'id', by.y = 'id', all.x = TRUE)

# combine and save --------------------------------------------------------

# combine
# sono = rbind(dal, noa)
sono = noa

# save in R format
saveRDS(sono, file = out_file)

# save as csv
# write.csv(x = sono, file = 'sono.csv', row.names = FALSE)
