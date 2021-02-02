## tbl_vif_est_num_sight_est_callrate.R ##

# VIF tables for appendix for both types of models, the estimating number of 
# whales sighted and estimating the call rates for each call type (up, gs, mf)

# setup -------------------------------------------------------------------

# libraries 
library(MASS)
library(tidyverse)
library(car)
library(data.table)
library(dplyr)

# input -------------------------------------------------------------------

# data
df = readRDS("data/processed/proc_acou_photoid.rds")

# process - est num sighting ----------------------------------------------

# num whale VIF table
# model
sighta = lm(num_sighting ~ up_per_hr+gs_per_hr+mf_per_hr+yday
                  , data = df)

# VIF
a = vif(sighta)

# put vif in dataframe
a = as.data.frame(a)
a = data.table::transpose(a)

# rename column names
tmp_df = a %>% 
  dplyr::rename(
    up_per_hr = V1,
    gs_per_hr = V2,
    mf_per_hr = V3,
    yday = V4)

# add column for which model it is
tmp_df$model = c('num_sight ~ up_per_hr + gs_per_hr + mf_per_hr + yday')

# set dataframe to be a table 
setDT(tmp_df)

# save data table 
write.csv(tmp_df,"data/processed/tbl_est_num_sight_vif.csv")


# process - est call rate (all call rates) --------------------------------

# upcall call rate VIF table 
# model
sightf1 = glm.nb(up ~yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                   offset(log(rec_duration))
                 , data = df, control=glm.control(maxit=100))

# VIF
a = vif(sightf1)

# put vif in dataframe
a = as.data.frame(a)
a = data.table::transpose(a)

# gunshot call rate VIF table 
# model
sightf2 = glm.nb(gs ~yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                   offset(log(rec_duration))
                 , data = df, control=glm.control(maxit=100))

# VIF
b = vif(sightf2)

# put vif in dataframe
b = as.data.frame(b)
b = data.table::transpose(b)

# mid frq call rate VIF table 
# model
sightf3 = glm.nb(mf ~yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                   offset(log(rec_duration))
                 , data = df, control=glm.control(maxit=100))

# VIF
c = vif(sightf3)

# put vif in dataframe
c = as.data.frame(c)
c = data.table::transpose(c)

tmp_df <- rbind(a, b, c)

# rename column names
tmp_df = tmp_df %>% 
  dplyr::rename(
    yday = V1,
    ratio_male_female = V2,
    foraging_bhv_whale = V3,
    social_bhv_whale = V4)

# add column for which model it is
tmp_df$model = c('up ~ yday + ratio_male_female + social_bhv_whale + foraging_bhv_whale + offset(log(rec_duration))',
                 'gs ~ yday + ratio_male_female + social_bhv_whale + foraging_bhv_whale + offset(log(rec_duration))',
                 'mf ~ yday + ratio_male_female + social_bhv_whale + foraging_bhv_whale + offset(log(rec_duration))')


# set dataframe to be a table 
setDT(tmp_df)

# save data table 
write.csv(tmp_df,"data/processed/tbl_est_call_rate_vif.csv")
