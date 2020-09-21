## wrg_comb_acou_photoid ##
# combining and warngling processed NOAA 2017, 2018, 2019 acoustic and photo-id
# data into the complete data file for drawing results and conclusions

# libraries ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(stringr)

# input -------------------------------------------------------------------

# read in processed acoustic data file
acou_df = readRDS("data/processed/all_noaa_acoustic.rds")
  
# read in processed photo-id data file
id_df = readRDS("data/processed/all_noaa_photoid.rds")

# setup -------------------------------------------------------------------

# output file name
ofile = 'data/processed/proc_acou_photoid.rds'

# process -----------------------------------------------------------------

# count the sightings per deployment
vis = count(id_df, id)

# adding acoustic data to data frame 

# replace miss-spelled call names
acou_df$call_type[acou_df$call_type == 'ga'] = 'gs'
acou_df$call_type[acou_df$call_type == 'mg'] = 'mf'
acou_df$call_type[acou_df$call_type == 'mf '] = 'mf'

# count the number of up calls, mid-freq and gunshots per deployment 
up = acou_df %>%
  filter(call_type == 'up') %>%
  filter(is.na(score) | score == 1) %>%
  count(id)
mf = acou_df %>%
  filter(call_type == 'mf') %>%
  filter(is.na(score) | score == 1) %>%
  count(id)
gs = acou_df %>% 
  filter(call_type == 'gs') %>%
  filter(is.na(score) | score == 1) %>%
  count(id)

duration = acou_df %>%
  filter(call_type == 'START') %>%
  select(c('id','duration'))
duration = duration[!duplicated(duration$id), ]

# adding demogrpahic data to data frame

# merging sightings with number of calls for up, mf and gs
df = merge(x = vis, y = up, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% 
  transmute(
    id = id, 
    num_sighting = n.x,
    num_up = n.y) 
df = merge(x = df, y = mf, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% 
  transmute(
    id = id, 
    num_sighting = num_sighting,
    num_up = num_up,
    num_mf = n)
df = merge(x = df, y = gs, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% 
  transmute(
    id = id, 
    num_sighting = num_sighting,
    num_up = num_up,
    num_mf = num_mf,
    num_gs = n)
df = merge(x = df, y = duration, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% 
  transmute(
    id = id, 
    num_sighting = num_sighting,
    num_up = num_up,
    num_mf = num_mf,
    num_gs = num_gs,
    duration = duration)

# count the number of juvenile female, juvenile male, adult female, adult male,
# calf, unknown age, unknown age per deployment 
jf = id_df %>%
  filter(age == 'J') %>%
  filter(sex == 'F') %>%
  count(id)
jm = id_df %>%
  filter(age == 'J') %>%
  filter(sex == 'M') %>%
  count(id)
af = id_df %>%
  filter(age == 'A') %>%
  filter(sex == 'F') %>%
  count(id)
am = id_df %>%
  filter(age == 'A') %>%
  filter(sex == 'M') %>%
  count(id)
calf = id_df %>%
  filter(age == 'C') %>%
  count(id)
uns = id_df %>%
  filter(sex == 'X') %>%
  count(id)
una = id_df %>%
  filter(age == 'U') %>%
  count(id)

# merging sightings with number of jf, jm, af, am
df = merge(x = df, y = jf, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(juvenile_female = n)

df = merge(x = df, y = jm, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(juvenile_male = n)

df = merge(x = df, y = af, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(adult_female = n)

df = merge(x = df, y = am, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(adult_male = n)

df = merge(x = df, y = calf, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(calf = n)

df = merge(x = df, y = uns, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(unknown_sex = n)

df = merge(x = df, y = una, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(unknown_age = n)

# adding behaviour data to data frame 

# separate multiple behaviours into rows
tmp = separate_rows(id_df, behaviour, sep = ",")

# trim leading white space from split behaviours
tmp$behaviour = trimws(tmp$behaviour)

# replace tmp with df
id_dfs = tmp

# look to see which behaviour needs to be renamed 
unique(id_dfs$behaviour)
table(id_dfs$behaviour)

# names on left are now written as names on the right
id_dfs$behaviour = gsub('CALF W/ MOM', 'CALF W/MOM', id_dfs$behaviour)
id_dfs$behaviour = gsub('W/ CALF' , 'W/CALF', id_dfs$behaviour)
id_dfs$behaviour = gsub('MCSLG' , 'MCLSG', id_dfs$behaviour)
id_dfs$behaviour = gsub('AGG', 'AGG VSL', id_dfs$behaviour)
id_dfs$behaviour = gsub('AGG VSL VSL', 'AGG VSL', id_dfs$behaviour)
id_dfs$behaviour <- sub(" ", "_", id_dfs$behaviour)

# double checking all the behaviours
unique(id_dfs$behaviour)
table(id_dfs$behaviour)

# count for each behaviour for each deployment
df_test = subset(id_dfs, select = c("id","behaviour"))
unique((df_test$behaviour))

df_test = count(df_test, vars = id, wt_var = behaviour)

df_test = df_test %>%
  group_by(vars, wt_var) %>% 
  summarise(sum = sum(n)) %>%
  spread(wt_var, sum)

colnames(df_test)[1] = "id"

df = merge(x = df , y = df_test, by.x = 'id', by.y = 'id', all.x = TRUE)

# recategorizing behviours (lumping)
df$foraging = (df$SUB_FD+
                 df$MCLSG+
                 df$SKM_FD+
                 df$CO_FD+
                 df$LEAD)
#df$foraging = df$foraging/df$num_sightings
df$social = (df$SAG+
               df$BOD_CNT+
               df$ROLL+
               df$BEL_UP+
               # df$APPR+
               df$`BEL/BEL`+
               df$LBTL+
               df$UW_EXH+
               df$BRCH+
               # df$BUBLS+
               df$TL_SLSH+
               df$RACE+
               df$FCL)
#df$social = df$social/df$num_sightings
# df$social_notsag = (df$BOD_CNT+
#                       df$ROLL+
#                       df$BEL_UP+
#                       df$`BEL/BEL`+
#                       df$LBTL+
#                       df$UW_EXH+
#                       df$BRCH+
#                       # df$BUBLS+
#                       df$TL_SLSH)
# #df$social_notsag = (df$social_notsag/df$num_sightings)
# df$social_sag = (df$SAG+
#                    df$FCL+
#                    # df$APPR+
#                    df$RACE)
#df$social_sag = df$social_sag/df$num_sightings
df$other_bhv= (df$MUD+
                 df$MOPN+
                 df$DFCN+
                 df$HDLFT+
                 # df$`SUB_FD?`+
                 df$AVD+
                 df$FL+
                 df$POST)#+
# df$UNUSUAL_BEH)
#df$other_bhv = df$other_bhv/df$num_sightings
df$mom_calf = (df$CALF+
                 df$`CALF_W/MOM`+
                 df$`CALF_W/OTHERS`+
                 df$`W/CALF`+
                 df$NURS)
#df$mom_calf = df$mom_calf/df$num_sightings
df$entg_eff = (df$AGG_VSL+
                 df$DSENTGL_ATT+
                 df$ENTGL+
                 df$FRST_ENTGL+
                 df$LN_GONE+
                 df$NOT_FL+
                 df$PRT_DSENTGL+
                 df$SATTG_GONE)
#df$entg_eff = df$entg_eff/df$num_sightings
drop <- c("AGG_VSL","APPR","AVD","BEL_UP","BEL/BEL","BOD_CNT","BRCH","BUBLS",
          "CALF","CALF_W/MOM","CALF_W/OTHERS","CO_FD","DFCN","DSENTGL_ATT","ENTGL","FCL",
          "FL","FRST_ENTGL","HDLFT","LBTL","LEAD","LN_GONE","MCLSG","MOPN","MUD","NONE",
          "NOT_FL","NURS","POST","PRT_DSENTGL","RACE","ROLL","SAG","SATTG_GONE","SKM_FD",
          "SUB_FD","SUB_FD?","TL_SLSH","UNUSUAL_BEH","UW_EXH","W/CALF","W/CETACEAN")
tmp = df[,!(names(df) %in% drop)]
df= tmp

# adding other variables of interest ( based off of current variables) to data frame

# convert calls to calls per hour for each deployment duration
tmp = as.numeric(df$duration)
df$duration = tmp
df$up_dur_hr = df$num_up/(df$duration/60/60)
df$mf_dur_hr = df$num_mf/(df$duration/60/60)
df$gs_dur_hr = df$num_gs/(df$duration/60/60)

# round the sightings per hour to 3 decimal places 
round(df$up_dur_hr, 3)
round(df$mf_dur_hr, 3)
round(df$gs_dur_hr, 3)

# change all NAs to 0 
df[is.na(df)] = 0

# seperate the year from the id to make it a new column
yr = str_split_fixed(df$id, "_", 2)
yr
yr<-yr[,-2] # delete column 2 which is the other half of the id
df$year = yr

# change the year to numeric instead of character
df$year = as.numeric(df$year)
class(df$year)

# other columns of interest added
df$sum_calls = (df$num_up+df$num_mf+df$num_gs)
df$ratio_female_male = (df$adult_male/df$adult_female)
df$sum_calls_dur = (df$sum_calls/df$duration)
df$sum_juvenile = (df$juvenile_female+df$juvenile_male)
df$sum_female = (df$juvenile_female+df$adult_female)
df$sum_male = (df$juvenile_male+df$adult_male)

#adding lat and lon and date and yday 
df_test = subset(acou_df, select = c("id","lat"))
df_test = df_test[!duplicated(df_test), ]
df = merge(x = df , y = df_test, by.x = 'id', by.y = 'id', all.x = TRUE)

df_test = subset(acou_df, select = c("id","lon"))
df_test = df_test[!duplicated(df_test), ]
df = merge(x = df , y = df_test, by.x = 'id', by.y = 'id', all.x = TRUE)

df_test = subset(acou_df, select = c("id","date"))
df_test = df_test[!duplicated(df_test), ]
df = merge(x = df , y = df_test, by.x = 'id', by.y = 'id', all.x = TRUE)

df_test = subset(acou_df, select = c("id","yday"))
df_test = df_test[!duplicated(df_test), ]
df = merge(x = df , y = df_test, by.x = 'id', by.y = 'id', all.x = TRUE)

# remove columns that aren't behaviours
drops <- c("WH_CHN","BLK_BEL","WH_BEL","BLK_CHN","FEM","MALE","SUCTG")
df = df[ , !(names(df) %in% drops)]
colnames(df)

# save file 
saveRDS(df, file = ofile)
