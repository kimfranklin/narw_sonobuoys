# tbl_overdis_est_callrate.R #

# Overdispersion test results for full poisson GLM call rate models

# libraries ---------------------------------------------------------------

library(tidyverse)
library(data.table)
library(AER)

# input -------------------------------------------------------------------

df = readRDS("data/processed/proc_acou_photoid.rds")


# process -----------------------------------------------------------------

# estimating upcall call rate UPDATED
sightm = glm(up ~ yday+
               num_sighting+ratio_male_female+
               social_bhv_whale+foraging_bhv_whale+
               offset(log(rec_duration))
             , data = df, family = 'poisson')

up = dispersiontest(sightm,trafo=NULL)

up = do.call(rbind.data.frame, up)
up=data.table::transpose(up)

# estimating gunshot call rate UPDATED
sightm = glm(gs ~ yday+
               num_sighting+ratio_male_female+
               social_bhv_whale+foraging_bhv_whale+
               offset(log(rec_duration))
             , data = df, family = 'poisson')

gs = dispersiontest(sightm,trafo=NULL)

gs = do.call(rbind.data.frame, gs)
gs=data.table::transpose(gs)

# estimating mid-freq call rate UPDATED
sightm = glm(mf ~ yday+
               num_sighting+ratio_male_female+
               social_bhv_whale+foraging_bhv_whale+
               offset(log(rec_duration))
             , data = df, family = 'poisson')

mf = dispersiontest(sightm,trafo=NULL)

mf = do.call(rbind.data.frame, mf)
mf=data.table::transpose(mf)

# combine all rows into a dataframe
tmp_df <- rbind(up, gs, mf)

# rename column names
tmp_df = tmp_df %>% 
  rename(
    test_stat_z = V1,
    p_val = V2,
    dispersion = V3,
    deg_freedom= V4,
    hypothesis = V5,
    test_type = V6,
    model = V7
    
  )

tmp_df$model = c('up ~ ydady+num_sighting+ratio_male_female+social_bhv_whale+foraging_bhv_whale+offset(log(rec_duration))',
              'gs ~ yday+num_sighting+ratio_male_female+social_bhv_whale+foraging_bhv_whale+offset(log(rec_duration))',
              'mf ~ yday+num_sighting+ratio_male_female+social_bhv_whale+foraging_bhv_whale+offset(log(rec_duration))')


# set dataframe to be a table 
setDT(tmp_df)

# save data table 
write.csv(tmp_df,"data/processed/overdis_test_callrate.csv")
