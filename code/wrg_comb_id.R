## wrg_comb_id ##
# combining all photoid data into one dataframe



# libraries
library(readxl)
library(tidyverse)

# read in files
noa1718 = read_excel('data/raw/visual/photo-id/2020-05-20-FranklinNEFSCGSL2017-2018DatawithAssocAgeSex.xlsx')
noa19 = read.csv('data/raw/visual/photo-id/2019NOAAGSL_sightings.csv')

# check names on files
names(noa1718)
names(noa19)

# condensing dataframes and renaming columns
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

tmp = noa19 %>%
  transmute(
    EGNO = EGNO,
    year = SightingYear,
    month = SightingMonth,
    day = SightingDay,
    time = SightingTime,
    lat = Latitude,
    lon = Longitude,
    age = AgeClass,
    sex = Sex,
    behaviour = Behaviors
    )

noa19 = tmp

# combining the two dataframes
id = rbind(noa1718, noa19) 
