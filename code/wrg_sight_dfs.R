## sight_dfs
# combining both acoustic and visual data in one dataset
# this code is very custom made for noaa 2017 and 2018 data 

# read in the data 
ifile = readRDS('data/processed/acoustic.rds')
df = ifile
sight_df = readRDS('data/processed/photo_id.rds')
udur = readRDS('data/interim/duration_times.rds')
sono_log = readRDS('data/interim/sono_log.rds')

# setup
library(tidyverse)
library(dplyr)
library(stringr)

# process
# count the sightings per deployment
sighting = count(sight_df, id)
sighting

# count the number of up calls, mid-freq and gunshots per deployment 
up = df %>%
  filter(call_type == 'up') %>%
  filter(is.na(score) | score == 1) %>%
  count(id)
mf = df %>%
  filter(call_type == 'mf') %>%
  filter(is.na(score) | score == 1) %>%
  count(id)
gs = df %>% 
  filter(call_type == 'gs') %>%
  filter(is.na(score) | score == 1) %>%
  count(id)

# merging sightings with number of calls for up, mf and gs
sight_dfs = merge(x = sighting, y = up, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% 
  transmute(
    id = id, 
    num_sighting = n.x,
    num_up = n.y) 
sight_dfs = merge(x = sight_dfs, y = mf, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% 
  transmute(
    id = id, 
    num_sighting = num_sighting,
    num_up = num_up,
    num_mf = n)
sight_dfs = merge(x = sight_dfs, y = gs, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% 
  transmute(
    id = id, 
    num_sighting = num_sighting,
    num_up = num_up,
    num_mf = num_mf,
    num_gs = n)
sight_dfs = merge(x = sight_dfs, y = udur, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% 
  transmute(
    id = id, 
    num_sighting = num_sighting,
    num_up = num_up,
    num_mf = num_mf,
    num_gs = num_gs,
    duration = duration)

# count the number of juvenile female, juvenile male, adult female, adult male
# per deployment 
jf = sight_df %>%
  filter(age_class == 'J') %>%
  filter(sex == 'F') %>%
  count(id)
jm = sight_df %>%
  filter(age_class == 'J') %>%
  filter(sex == 'M') %>%
  count(id)
af = sight_df %>%
  filter(age_class == 'A') %>%
  filter(sex == 'F') %>%
  count(id)
am = sight_df %>%
  filter(age_class == 'A') %>%
  filter(sex == 'M') %>%
  count(id)

# merging sightings with number of jf, jm, af, am
sight_dfs = merge(x = sight_dfs, y = jf, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(juvenile_female = n)

sight_dfs = merge(x = sight_dfs, y = jm, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(juvenile_male = n)

sight_dfs = merge(x = sight_dfs, y = af, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(adult_female = n)

sight_dfs = merge(x = sight_dfs, y = am, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(adult_male = n)

# count for each behaviour for each deployment
subfed = sight_df %>% 
  filter(behaviour == 'SUB FD') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = subfed, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(SUB_FD = n)

telebuoy = sight_df %>% 
  filter(behaviour == 'TELBUOY') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = telebuoy, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(TELBUOY = n)

roll = sight_df %>% 
  filter(behaviour == 'ROLL') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = roll, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(ROLL = n)

MCLSG = sight_df %>% 
  filter(behaviour == 'MCLSG') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = MCLSG, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(MCLSG = n)

Blkbel = sight_df %>% 
  filter(behaviour == 'BLK BEL') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = Blkbel, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(BLK_BEL = n)

poop = sight_df %>% 
  filter(behaviour == 'DFCN') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = poop, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(DFCN = n)

disent = sight_df %>% 
  filter(behaviour == 'DSENTGL') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = disent, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(DSENTGL = n)

whibel = sight_df %>% 
  filter(behaviour == 'WH BEL') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = whibel, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(WH_BEL = n)

mud = sight_df %>% 
  filter(behaviour == 'MUD') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = mud, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(MUD = n)

blkchin = sight_df %>% 
  filter(behaviour == 'BLK CHN') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = blkchin, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(BLK_CHN = n)

whichin = sight_df %>% 
  filter(behaviour == 'WH CHN') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = whichin, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(WH_CHN = n)

ligo = sight_df %>% 
  filter(behaviour == 'LN GONE') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = ligo, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(LN_GONE = n)

lbtl = sight_df %>% 
  filter(behaviour == 'LBTL') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = lbtl, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(LBTL = n)

bodc = sight_df %>% 
  filter(behaviour == 'BOD CNT') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = bodc, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(BOD_CNT = n)

belup = sight_df %>% 
  filter(behaviour == 'BEL UP') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = belup, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(BEL_UP = n)

fenta = sight_df %>% 
  filter(behaviour == 'FRSTENTGL') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = fenta, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(FRSTENTGL = n)

tlsh = sight_df %>% 
  filter(behaviour == 'TL SLSH') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = tlsh, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(TL_SLSH = n)

skmf = sight_df %>% 
  filter(behaviour == 'SKM FD') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = skmf, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(SKM_FD = n)

mopn = sight_df %>% 
  filter(behaviour == 'MOPN') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = mopn, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(MOPN = n)

sag = sight_df %>% 
  filter(behaviour == 'SAG') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = sag, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(SAG = n)

hdlft = sight_df %>% 
  filter(behaviour == 'HD LFT') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = hdlft, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(HD_LFT = n)

cofd = sight_df %>% 
  filter(behaviour == 'CO FD') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = cofd, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(CO_FD = n)

uwexh = sight_df %>% 
  filter(behaviour == 'UW EXH') %>%
  count(id)
sight_dfs = merge(x = sight_dfs, y = uwexh, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% rename(UW_EXH = n)

# convert calls to calls per hour for each deployment duration
sight_dfs$up_dur_hr = sight_dfs$num_up/(sight_dfs$duration/60/60)
sight_dfs$mf_dur_hr = sight_dfs$num_mf/(sight_dfs$duration/60/60)
sight_dfs$gs_dur_hr = sight_dfs$num_gs/(sight_dfs$duration/60/60)

# round the sightings per hour to 3 decimal places 
round(sight_dfs$up_dur_hr, 3)
round(sight_dfs$mf_dur_hr, 3)
round(sight_dfs$gs_dur_hr, 3)

# change all NAs to 0 
sight_dfs[is.na(sight_dfs)] = 0

# seperate the year from the id to make it a new column
yr = str_split_fixed(sight_dfs$id, "_", 2)
yr
yr<-yr[,-2] # delete column 2 which is the other half of the id
sight_dfs$year = yr

# change the year to numeric instead of character
sight_dfs$year = as.numeric(sight_dfs$year)
class(sight_dfs$year)

# other columns of interest added
sight_dfs$sum_calls = (sight_dfs$num_up+sight_dfs$num_mf+sight_dfs$num_gs)
sight_dfs$ratio_female_male = (sight_dfs$adult_male/sight_dfs$adult_female)
sight_dfs$sum_calls_dur = (sight_dfs$sum_calls/sight_dfs$duration)
sight_dfs$sum_juvenile = (sight_dfs$juvenile_female+sight_dfs$juvenile_male)
sight_dfs$sum_female = (sight_dfs$juvenile_female+sight_dfs$adult_female)
sight_dfs$sum_male = (sight_dfs$juvenile_male+sight_dfs$adult_male)

#adding lat and lon and date and yday 
lat = sono_log %>% 
  transmute(
    id = id, 
    lat = lat)
sight_dfs = merge(x = sight_dfs, y = lat, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% 
  mutate(lat = lat)

lon = sono_log %>% 
  transmute(
    id = id, 
    lon = lon)
sight_dfs = merge(x = sight_dfs, y = lon, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% 
  mutate(lon = lon)

date = sono_log %>% 
  transmute(
    id = id, 
    date = date)
sight_dfs = merge(x = sight_dfs, y = date, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% 
  mutate(date = date)

yday = sono_log %>% 
  transmute(
    id = id, 
    yday = yday)
sight_dfs = merge(x = sight_dfs, y = yday, by.x = 'id', by.y = 'id', all.x = TRUE)
sight_dfs = sight_dfs %>% 
  mutate(yday = yday)

drops <- c("WH_CHN","BLK_BEL","WH_BEL","BLK_CHN")
sight_dfs = sight_dfs[ , !(names(sight_dfs) %in% drops)]

# recategorizing behviours (lumping)
sight_dfs$feed = (sight_dfs$SUB_FD+
                    sight_dfs$MCLSG+
                    sight_dfs$SKM_FD+
                    sight_dfs$CO_FD)
sight_dfs$social = (sight_dfs$SAG+
                      sight_dfs$BOD_CNT+
                      sight_dfs$ROLL)
sight_dfs$other_behv = (sight_dfs$MUD+
                          sight_dfs$LBTL+
                          sight_dfs$BEL_UP+
                          sight_dfs$TL_SLSH+
                          sight_dfs$MOPN+
                          sight_dfs$DFCN+
                          sight_dfs$HD_LFT+
                          sight_dfs$UW_EXH)

# save file 
saveRDS(sight_dfs, file = 'data/processed/sight_dfs.rds')
