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

# place all numeric ages into categorical age groups
# calf is 0 years old or categorized as C (C=0)
# juvenile is 1 to 9 years old or categorized as J (1=<J<9)
# adult is 9 years old or older and categorized as A (9=<A)
# unknown ages are categorized as U
# see email from Leah Crowe Nov 1, 2019 for reference
noa1718$age[is.na(noa1718$age)] = 'NA'

noa1718sub = noa1718

noa1718sub$age = as.numeric(noa1718sub$age)

noa1718sub$age = ifelse(noa1718sub$age<=9,"J","A")

noa1718sub = dplyr::filter(noa1718sub,  !is.na(age))

noa1718sub2 = noa1718 %>%
  filter(age %in% c("A","NA","U"))

tmp = rbind(noa1718sub, noa1718sub2)

noa1718 = tmp

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

# change NA ages to 'NA' so data can read NA as a type of age
noa19$age[is.na(noa19$age)] = 'NA'

# combining the two dataframes
id = rbind(noa1718, noa19) 

# create a date column
id$date = as.Date(id$datetime)

# save id dataframe
saveRDS(id, ofile)
