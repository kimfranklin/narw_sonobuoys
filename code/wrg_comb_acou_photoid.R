## wrg_comb_acou_photoid ##
# combining and wangling processed NOAA 2017, 2018, 2019 acoustic and photo-id
# data into the complete data file for drawing results and conclusions

# libraries ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(stringr)
library(oce)
library(lubridate)

# input -------------------------------------------------------------------

# read in processed acoustic data file
acou_df = readRDS("data/processed/all_noaa_acoustic.rds")
  
# read in processed photo-id data file
id_df = readRDS("data/processed/all_noaa_photoid.rds")


# setup -------------------------------------------------------------------

# output file name
ofile = 'data/processed/proc_acou_photoid.rds'


# process -----------------------------------------------------------------

# # remove unphotographed whales from dataset
id_df = id_df %>%
  drop_na(EGNO)

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

dep_duration = acou_df %>%
  filter(call_type == 'START') %>%
  dplyr::select(c('id','dep_duration'))
dep_duration = dep_duration[!duplicated(dep_duration$id), ]

rec_duration = acou_df %>%
  filter(call_type == 'START') %>%
  dplyr::select(c('id','rec_duration'))
rec_duration = rec_duration[!duplicated(rec_duration$id), ]

# merging sightings with number of calls for up, mf and gs
df = merge(x = vis, y = up, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% 
  transmute(
    id = id, 
    num_sighting = n.x,
    up = n.y) 

df = merge(x = df, y = mf, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(mf = n)

df = merge(x = df, y = gs, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(gs = n)

df = merge(x = df, y = dep_duration, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(dep_duration = dep_duration)

df = merge(x = df, y = rec_duration, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(rec_duration = rec_duration)

# adding demographic data to data frame

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
fcalf = id_df %>%
  filter(age == 'C') %>%
  filter(sex == 'F') %>%
  count(id)
mcalf = id_df %>%
  filter(age == 'C') %>%
  filter(sex == 'M') %>%
  count(id)
xcalf = id_df %>%
  filter(age == 'C') %>%
  filter(sex == 'X') %>%
  count(id)
auns = id_df %>%
  filter(age == 'A') %>%
  filter(sex == 'X') %>%
  count(id)
juns = id_df %>%
  filter(age == 'J') %>%
  filter(sex == 'X') %>%
  count(id)
un = id_df %>%
  filter(age == 'U') %>%
  filter(sex == 'X') %>%
  count(id)
unf = id_df %>%
  filter(age == 'U') %>%
  filter(sex == 'F') %>%
  count(id)
unm = id_df %>%
  filter(age == 'U') %>%
  filter(sex == 'M') %>%
  count(id)
# nas = id_df %>%
#   filter(age == 'NA') %>%
#   filter(sex == 'NONE') %>%
#   count(id)

# merging sightings with number of jf, jm, af, am, calf, uns, una
df = merge(x = df, y = jf, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(juvenile_female = n)

df = merge(x = df, y = jm, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(juvenile_male = n)

df = merge(x = df, y = af, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(adult_female = n)

df = merge(x = df, y = am, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(adult_male = n)

df = merge(x = df, y = fcalf, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(calf_female = n)

df = merge(x = df, y = mcalf, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(calf_male = n)

df = merge(x = df, y = xcalf, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(sex_ukn_calf = n)

df = merge(x = df, y = auns, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(adult_unknown_sex = n)

df = merge(x = df, y = juns, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(juvenile_unknown_sex = n)

df = merge(x = df, y = un, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(unknown_agesex = n)

df = merge(x = df, y = unf, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(unknown_age_female = n)

df = merge(x = df, y = unm, by.x = 'id', by.y = 'id', all.x = TRUE)
df = df %>% rename(unknown_age_male = n)

# df = merge(x = df, y = nas, by.x = 'id', by.y = 'id', all.x = TRUE)
# df = df %>% rename(na_agesex = n)

# remove duplicate whale sightings
# make subset of only dups
df_dup = id_df %>%
  filter(dup == 'TRUE') %>%
  group_by(id) %>%
  filter(!duplicated(EGNO))

# add column of just 1 call it dummy
df_dup[,"dummy"] <- 1

# remove duplicates from num_sighting
# count the duplicates by id
testing = df_dup %>%
  group_by(id) %>%
  summarise(df_dup = sum(dummy))

# filter df for all the id's
testing2 = df %>%
  dplyr::select(id)

# after filtering df into testing2 dataframe, add column of 0's into testing2
testing2[,"dummy"] <- 0

# merge the dup counts dataframe testing) with dataframe that has all the deployment ids (testing2) to get testing3 dataframe
testing3 <- merge (testing, testing2 , all.x=T, all.y=T)

# change nas to 0 in testing3
testing3[is.na(testing3)] = 0

# drop the dummy column in testing3
drop <- c("dummy")
testing3 = testing3[,!(names(testing3) %in% drop)]

# add the dup counts dataframe (testing3) to the original dataframe in column called num_s_dups
df = merge(x = df, y = testing3, by.x = 'id', by.y = 'id', all.x = TRUE)

# subtract the sum of dups by id from the num_sighting by id
df[is.na(df)] = 0
df$num_sighting = df$num_sighting - df$df_dup

# remove the dups column
drop <- c("df_dup")
df = df[,!(names(df) %in% drop)]

# remove duplicates from demographic groups
# adult males
# count the duplicates by id
testing = df_dup %>%
  filter(age == 'A') %>%
  filter(sex == 'M') %>%
  group_by(id) %>%
  summarise(df_dup = sum(dummy))

# merge the dup counts dataframe testing) with dataframe that has all the deployment ids (testing2) to get testing3 dataframe
testing3 <- merge (testing, testing2 , all.x=T, all.y=T)

# change nas to 0 in testing3
testing3[is.na(testing3)] = 0

# drop the dummy column in testing3
drop <- c("dummy")
testing3 = testing3[,!(names(testing3) %in% drop)]

# add the dup counts dataframe (testing3) to the original dataframe in column called num_s_dups
df = merge(x = df, y = testing3, by.x = 'id', by.y = 'id', all.x = TRUE)

# subtract the sum of dups by id from the num_sighting by id
df$adult_male = df$adult_male - df$df_dup

# remove the dups column
drop <- c("df_dup")
df = df[,!(names(df) %in% drop)]

# adult females
# count the duplicates by id
testing = df_dup %>%
  filter(age == 'A') %>%
  filter(sex == 'F') %>%
  group_by(id) %>%
  summarise(df_dup = sum(dummy))

# merge the dup counts dataframe testing) with dataframe that has all the deployment ids (testing2) to get testing3 dataframe
testing3 <- merge (testing, testing2 , all.x=T, all.y=T)

# change nas to 0 in testing3
testing3[is.na(testing3)] = 0

# drop the dummy column in testing3
drop <- c("dummy")
testing3 = testing3[,!(names(testing3) %in% drop)]

# add the dup counts dataframe (testing3) to the original dataframe in column called num_s_dups
df = merge(x = df, y = testing3, by.x = 'id', by.y = 'id', all.x = TRUE)

# subtract the sum of dups by id from the num_sighting by id
df$adult_female = df$adult_female - df$df_dup

# remove the dups column
drop <- c("df_dup")
df = df[,!(names(df) %in% drop)]

# juvenile males
# count the duplicates by id
testing = df_dup %>%
  filter(age == 'J') %>%
  filter(sex == 'M') %>%
  group_by(id) %>%
  summarise(df_dup = sum(dummy))

# merge the dup counts dataframe testing) with dataframe that has all the deployment ids (testing2) to get testing3 dataframe
testing3 <- merge (testing, testing2 , all.x=T, all.y=T)

# change nas to 0 in testing3
testing3[is.na(testing3)] = 0

# drop the dummy column in testing3
drop <- c("dummy")
testing3 = testing3[,!(names(testing3) %in% drop)]

# add the dup counts dataframe (testing3) to the original dataframe in column called num_s_dups
df = merge(x = df, y = testing3, by.x = 'id', by.y = 'id', all.x = TRUE)

# subtract the sum of dups by id from the num_sighting by id
df$juvenile_male = df$juvenile_male - df$df_dup

# remove the dups column
drop <- c("df_dup")
df = df[,!(names(df) %in% drop)]

# juvenile females
# count the duplicates by id
testing = df_dup %>%
  filter(age == 'J') %>%
  filter(sex == 'F') %>%
  group_by(id) %>%
  summarise(df_dup = sum(dummy))

# merge the dup counts dataframe testing) with dataframe that has all the deployment ids (testing2) to get testing3 dataframe
testing3 <- merge (testing, testing2 , all.x=T, all.y=T)

# change nas to 0 in testing3
testing3[is.na(testing3)] = 0

# drop the dummy column in testing3
drop <- c("dummy")
testing3 = testing3[,!(names(testing3) %in% drop)]

# add the dup counts dataframe (testing3) to the original dataframe in column called num_s_dups
df = merge(x = df, y = testing3, by.x = 'id', by.y = 'id', all.x = TRUE)

# subtract the sum of dups by id from the num_sighting by id
df$juvenile_female = df$juvenile_female - df$df_dup

# remove the dups column
drop <- c("df_dup")
df = df[,!(names(df) %in% drop)]

# adult, unknown sex
# count the duplicates by id
testing = df_dup %>%
  filter(age == 'A') %>%
  filter(sex == 'X') %>%
  group_by(id) %>%
  summarise(df_dup = sum(dummy))

# merge the dup counts dataframe testing) with dataframe that has all the deployment ids (testing2) to get testing3 dataframe
testing3 <- merge (testing, testing2 , all.x=T, all.y=T)

# change nas to 0 in testing3
testing3[is.na(testing3)] = 0

# drop the dummy column in testing3
drop <- c("dummy")
testing3 = testing3[,!(names(testing3) %in% drop)]

# add the dup counts dataframe (testing3) to the original dataframe in column called num_s_dups
df = merge(x = df, y = testing3, by.x = 'id', by.y = 'id', all.x = TRUE)

# subtract the sum of dups by id from the num_sighting by id
df$adult_unknown_sex = df$adult_unknown_sex - df$df_dup

# remove the dups column
drop <- c("df_dup")
df = df[,!(names(df) %in% drop)]

# juvenile, unknown sex
# count the duplicates by id
testing = df_dup %>%
  filter(age == 'J') %>%
  filter(sex == 'X') %>%
  group_by(id) %>%
  summarise(df_dup = sum(dummy))

# merge the dup counts dataframe testing) with dataframe that has all the deployment ids (testing2) to get testing3 dataframe
testing3 <- merge (testing, testing2 , all.x=T, all.y=T)

# change nas to 0 in testing3
testing3[is.na(testing3)] = 0

# drop the dummy column in testing3
drop <- c("dummy")
testing3 = testing3[,!(names(testing3) %in% drop)]

# add the dup counts dataframe (testing3) to the original dataframe in column called num_s_dups
df = merge(x = df, y = testing3, by.x = 'id', by.y = 'id', all.x = TRUE)

# subtract the sum of dups by id from the num_sighting by id
df$juvenile_unknown_sex = df$ juvenile_unknown_sex- df$df_dup

# remove the dups column
drop <- c("df_dup")
df = df[,!(names(df) %in% drop)]

# unknown age, unknown sex
# count the duplicates by id
testing = df_dup %>%
  filter(age == 'U') %>%
  filter(sex == 'X') %>%
  group_by(id) %>%
  summarise(df_dup = sum(dummy))

# merge the dup counts dataframe testing) with dataframe that has all the deployment ids (testing2) to get testing3 dataframe
testing3 <- merge (testing, testing2 , all.x=T, all.y=T)

# change nas to 0 in testing3
testing3[is.na(testing3)] = 0

# drop the dummy column in testing3
drop <- c("dummy")
testing3 = testing3[,!(names(testing3) %in% drop)]

# add the dup counts dataframe (testing3) to the original dataframe in column called num_s_dups
df = merge(x = df, y = testing3, by.x = 'id', by.y = 'id', all.x = TRUE)

# subtract the sum of dups by id from the num_sighting by id
df$unknown_agesex = df$unknown_agesex - df$df_dup

# remove the dups column
drop <- c("df_dup")
df = df[,!(names(df) %in% drop)]

# unknown age, female
# count the duplicates by id
testing = df_dup %>%
  filter(age == 'U') %>%
  filter(sex == 'F') %>%
  group_by(id) %>%
  summarise(df_dup = sum(dummy))

# merge the dup counts dataframe testing) with dataframe that has all the deployment ids (testing2) to get testing3 dataframe
testing3 <- merge (testing, testing2 , all.x=T, all.y=T)

# change nas to 0 in testing3
testing3[is.na(testing3)] = 0

# drop the dummy column in testing3
drop <- c("dummy")
testing3 = testing3[,!(names(testing3) %in% drop)]

# add the dup counts dataframe (testing3) to the original dataframe in column called num_s_dups
df = merge(x = df, y = testing3, by.x = 'id', by.y = 'id', all.x = TRUE)

# subtract the sum of dups by id from the num_sighting by id
df$unknown_age_female = df$unknown_age_female - df$df_dup

# remove the dups column
drop <- c("df_dup")
df = df[,!(names(df) %in% drop)]

# unknown age, male
# count the duplicates by id
testing = df_dup %>%
  filter(age == 'U') %>%
  filter(sex == 'M') %>%
  group_by(id) %>%
  summarise(df_dup = sum(dummy))

# merge the dup counts dataframe testing) with dataframe that has all the deployment ids (testing2) to get testing3 dataframe
testing3 <- merge (testing, testing2 , all.x=T, all.y=T)

# change nas to 0 in testing3
testing3[is.na(testing3)] = 0

# drop the dummy column in testing3
drop <- c("dummy")
testing3 = testing3[,!(names(testing3) %in% drop)]

# add the dup counts dataframe (testing3) to the original dataframe in column called num_s_dups
df = merge(x = df, y = testing3, by.x = 'id', by.y = 'id', all.x = TRUE)

# subtract the sum of dups by id from the num_sighting by id
df$unknown_age_male = df$unknown_age_male - df$df_dup

# remove the dups column
drop <- c("df_dup")
df = df[,!(names(df) %in% drop)]

# # NA age, NONE sex
# # count the duplicates by id
# testing = df_dup %>%
#   filter(age == 'NA') %>%
#   filter(sex == 'NONE') %>%
#   group_by(id) %>%
#   summarise(df_dup = sum(dummy))
# 
# # merge the dup counts dataframe testing) with dataframe that has all the deployment ids (testing2) to get testing3 dataframe
# testing3 <- merge (testing, testing2 , all.x=T, all.y=T)
# 
# # change nas to 0 in testing3
# testing3[is.na(testing3)] = 0
# 
# # drop the dummy column in testing3
# drop <- c("dummy")
# testing3 = testing3[,!(names(testing3) %in% drop)]
# 
# # add the dup counts dataframe (testing3) to the original dataframe in column called num_s_dups
# df = merge(x = df, y = testing3, by.x = 'id', by.y = 'id', all.x = TRUE)
# 
# # subtract the sum of dups by id from the num_sighting by id
# df$na_agesex = df$na_agesex - df$df_dup
# 
# # remove the dups column
# drop <- c("df_dup")
# df = df[,!(names(df) %in% drop)]

# adding behaviour data to data frame 
# separate multiple behaviours into rows
id_dfs = separate_rows(id_df, behaviour, sep = ",")

# trim leading white space from split behaviours
id_dfs$behaviour = trimws(id_dfs$behaviour)

# # look to see which behaviour needs to be renamed 
# unique(id_dfs$behaviour)
# table(id_dfs$behaviour)

# names on left are now written as names on the right
id_dfs$behaviour = gsub('CALF W/ MOM', 'CALF W/MOM', id_dfs$behaviour)
id_dfs$behaviour = gsub('W/ CALF' , 'W/CALF', id_dfs$behaviour)
id_dfs$behaviour = gsub('MCSLG' , 'MCLSG', id_dfs$behaviour)
id_dfs$behaviour = gsub('AGG', 'AGG VSL', id_dfs$behaviour)
id_dfs$behaviour = gsub('AGG VSL VSL', 'AGG VSL', id_dfs$behaviour)
id_dfs$behaviour <- sub(" ", "_", id_dfs$behaviour)

# # double checking all the behaviours
# unique(id_dfs$behaviour)
# table(id_dfs$behaviour)

# count for each behaviour for each deployment - subset for id and behaviour only
df_test = subset(id_dfs, select = c("id","behaviour"))
# unique((df_test$behaviour))

# count for each behaviour for each deployment - count
df_test = count(df_test, vars = id, wt_var = behaviour)

# summarize behaviour counts in a 'mini' data frame - this is so the counts can be merged with the full data frame
df_test = df_test %>%
  group_by(vars, wt_var) %>% 
  summarise(sum = sum(n)) %>%
  spread(wt_var, sum)

# rename the first column id
colnames(df_test)[1] = "id"

# merge subset that counts behaviours with the full data frame
df = merge(x = df , y = df_test, by.x = 'id', by.y = 'id', all.x = TRUE)

# change all NAs to 0 
df[is.na(df)] = 0

# re-categorizing behaviours (lumping)
# create foraging column
df[,"foraging"] <- 0

# add foraging behaviours to foraging column
if("SUB_FD" %in% colnames(df)){
  df$SUB_FD + df$foraging -> df$foraging
}
if("MCLSG" %in% colnames(df)){
  df$MCLSG + df$foraging -> df$foraging
}
if("SKM_FD" %in% colnames(df)){
  df$SKM_FD + df$foraging -> df$foraging
}
if("CO_FD" %in% colnames(df)){
  df$CO_FD + df$foraging -> df$foraging
}
if("LEAD" %in% colnames(df)){
  df$LEAD + df$foraging -> df$foraging
}

# create social column
df[,"social"] <- 0

# add social behaviours to social column
if("SAG" %in% colnames(df)){
  df$SAG + df$social -> df$social
}
if("BOD_CNT" %in% colnames(df)){
  df$BOD_CNT + df$social -> df$social
}
if("ROLL" %in% colnames(df)){
  df$ROLL + df$social -> df$social
}
if("BEL_UP" %in% colnames(df)){
  df$BEL_UP + df$social -> df$social
}
if("APPR" %in% colnames(df)){
  df$APPR + df$social -> df$social
}
if("BEL/BEL" %in% colnames(df)){
  df$'BEL/BEL' + df$social -> df$social
}
if("LBTL" %in% colnames(df)){
  df$LBTL + df$social -> df$social
}
if("UW_EXH" %in% colnames(df)){
  df$UW_EXH + df$social -> df$social
}
if("BRCH" %in% colnames(df)){
  df$BRCH + df$social -> df$social
}
if("BUBLS" %in% colnames(df)){
  df$BUBLS + df$social -> df$social
}
if("TL_SLSH" %in% colnames(df)){
  df$TL_SLSH + df$social -> df$social
}
if("RACE" %in% colnames(df)){
  df$RACE + df$social -> df$social
}
if("FCL" %in% colnames(df)){
  df$FCL + df$social -> df$social
}

# add other behavior column
df[,"other_bhv"] <- 0

# add other bheaviours to other behaviour column
if("MUD" %in% colnames(df)){
  df$MUD + df$other_bhv -> df$other_bhv
}
if("MOPN" %in% colnames(df)){
  df$MOPN + df$other_bhv -> df$other_bhv
}
if("DFCN" %in% colnames(df)){
  df$DFCN + df$other_bhv -> df$other_bhv
}
if("HDLFT" %in% colnames(df)){
  df$HDLFT + df$other_bhv -> df$other_bhv
}
if("AVD" %in% colnames(df)){
  df$AVD + df$other_bhv -> df$other_bhv
}
if("FL" %in% colnames(df)){
  df$FL + df$other_bhv -> df$other_bhv
}
if("POST" %in% colnames(df)){
  df$POST + df$other_bhv -> df$other_bhv
}
if("FLIP" %in% colnames(df)){
  df$FLIP + df$other_bhv -> df$other_bhv
}
if("WIWO" %in% colnames(df)){
  df$WIWO + df$other_bhv -> df$other_bhv
}
if("UNUSUAL" %in% colnames(df)){
  df$UNUSUAL + df$other_bhv -> df$other_bhv
}

# since each behaviour was its own column we do not need them now that they
# are grouped into the 3 behaviour categories so remove individual behaviours
drop <- c("AGG_VSL","APPR","AVD","BEL_UP","BEL/BEL","BOD_CNT","BRCH","BUBLS",
          "CALF","CALF_W/MOM","CALF_W/OTHERS","CO_FD","DFCN","DSENTGL_ATT","ENTGL","FCL",
          "FL","FRST_ENTGL","HDLFT","LBTL","LEAD","LN_GONE","MCLSG","MOPN","MUD","NONE",
          "NOT_FL","NURS","POST","PRT_DSENTGL","RACE","ROLL","SAG","SATTG_GONE","SKM_FD",
          "SUB_FD","SUB_FD?","TL_SLSH","UNUSUAL_BEH","UW_EXH","W/CALF","W/CETACEAN","BLK_BEL",
          "BLK_CHN","FEM","MALE","SUCTG","WH_BEL","WH_CHN","WIWO","FLIP","UNUSUAL")
df = df[,!(names(df) %in% drop)]

# change behaviours so that they are rates
df$foraging_bhv_whale = round(df$foraging/df$num_sighting, 3)

df$social_bhv_whale = round(df$social/df$num_sighting, 3)

df$other_bhv_whale = round(df$other_bhv/df$num_sighting, 3)

# adding other variables of interest (based off of current variables) to data frame

# separate the year from the id to make it a new column
yr = str_split_fixed(df$id, "_", 2)
yr<-yr[,-2] # delete column 2 which is the other half of the id
df$year = yr
df$year = as.numeric(df$year) # change the year to numeric instead of character

#adding lat and lon and date and yday and datetime
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

df_test = subset(acou_df, select = c("id","datetime"))
df_test = df_test[!duplicated(df_test), ]
df = merge(x = df , y = df_test, by.x = 'id', by.y = 'id', all.x = TRUE)

# call rate (call/hour) - calls per hour for each deployment duration
df$rec_duration = as.numeric(df$rec_duration)
df$up_per_hr = round(df$up/(df$rec_duration/60/60), 3)
df$mf_per_hr = round(df$mf/(df$rec_duration/60/60), 3)
df$gs_per_hr = round(df$gs/(df$rec_duration/60/60), 3)

# call production rate (call/hour/whale) - calls per hour (deployment duration) per sighted whale
df$up_per_hr_per_whale = round(df$up/(df$rec_duration/60/60)/df$num_sighting, 3)
df$mf_per_hr_per_whale = round(df$mf/(df$rec_duration/60/60)/df$num_sighting, 3)
df$gs_per_hr_per_whale = round(df$gs/(df$rec_duration/60/60)/df$num_sighting, 3)

# other columns of interest added
df$sum_calls = (df$up+df$mf+df$gs)
df$sum_calls_rdur = (df$sum_calls/df$rec_duration)
df$rec_duration_h = df$rec_duration/60/60
df$sum_call_recdurh = df$sum_calls/df$rec_duration_h
df$sum_juvenile = (df$juvenile_female+df$juvenile_male+df$juvenile_unknown_sex)
df$sum_adult = (df$adult_female+df$adult_male+df$adult_unknown_sex)
df$sum_female = (df$juvenile_female+df$adult_female+df$unknown_age_female+df$calf_female)
df$sum_male = (df$juvenile_male+df$adult_male+df$unknown_age_male+df$calf_male)
df$unknown = (df$juvenile_unknown_sex+df$adult_unknown_sex+df$unknown_age_female+
                df$unknown_age_male+df$unknown_agesex+#df$na_agesex
                +df$sex_ukn_calf)
df$ratio_male_female = round((df$sum_male/df$sum_female), 3)
df$ratio_juvenile_adult = round((df$sum_juvenile/df$sum_adult), 3)
df$month = month(as.POSIXct(df$date, format = '%Y-%m-%d'))
df$hour = hour(df$datetime) 

# calculate distance from orpheline trough
DF = vector('list', length = nrow(df))
for (ii in 1:nrow(df)) {
  # get df lat and lon data
  ilat = df$lat[ii]
  ilon = df$lon[ii]
  
  # compute distance from sonobuoy to orpheline trough
  df$dist = geodDist(
    longitude2 = ilon,
    latitude2 = ilat,
    longitude1 = -63.90,
    latitude1 = 47.90,
    alongPath = FALSE
  )
  
  # store output
  DF[[ii]] = df
  
  message('Done ', ii)
}

# output of loop to calculate distance from orpheline trough
dfs = bind_rows(DF)

# remove dist variable in df that loop stored in df
drop <- c("dist")
df = df[,!(names(df) %in% drop)]

# find only the unique distances from loop and add to dataframe
distance = distinct(dfs, dist)
df = cbind(df, distance)

# editing data frame so these models can be made
# foraging
df[,"foraging_bi"] <- 0
df$foraging_bi[df$foraging != 0] <- 1

# social
df[,"social_bi"] <- 0
df$social_bi[df$social != 0] <- 1

# other bhv
df[,"other_bhv_bi"] <- 0
df$other_bhv_bi[df$other_bhv != 0] <- 1

# save file 
saveRDS(df, file = ofile)
