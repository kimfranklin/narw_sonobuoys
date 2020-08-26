## wrg_comb_id ##
# combining all photoid data into one dataframe

# libraries
library(readxl)
library(tidyverse)

# files
# read in photo-id files
noa1718 = read_excel('data/raw/visual/photo-id/2020-05-20-FranklinNEFSCGSL2017-2018DatawithAssocAgeSex.xlsx')
noa19 = read.csv('data/raw/visual/photo-id/2019NOAAGSL_sightings.csv')

# out put file
ofile = 'data/interim/all_noaa_photoid_comb.rds'

# process

# check names on files
names(noa1718)
names(noa19)

# condensing dataframes and renaming columns
# for 2017 2018 data from the consortium
tmp = noa1718 %>%
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

noa1718 = tmp

# adding datetime column
tmp = paste0(noa1718$year,"-",noa1718$month,"-",noa1718$day," ",noa1718$time)
noa1718$datetime = as.POSIXct(tmp, format = "%Y-%m-%d %H%M", tz = 'EST')

# for 2019 data from NOAA (Tim Cole)
tmp = noa19 %>%
  transmute(
    EGNO = EGNO,
    year = SightingYear,
    month = SightingMonth,
    day = SightingDay,
    time = SightingTime,
    datetime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = 'EST'),
    lat = Latitude,
    lon = Longitude,
    age = AgeClass,
    sex = Sex,
    behaviour = Behaviors
    )

noa19 = tmp

# combining the two dataframes
id = rbind(noa1718, noa19) 

# create a date column
id$date = as.Date(id$datetime)

# save id dataframe
saveRDS(id, ofile)
