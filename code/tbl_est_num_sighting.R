# tbl_est_num_sighting.R #

# tables for manuscript that estimate the number of whales

# libraries ---------------------------------------------------------------

library(tidyverse)
library(MASS)
library(data.table)

# input -------------------------------------------------------------------

# read in data
df = readRDS("data/processed/proc_acou_photoid_fliptest.rds")

# process -----------------------------------------------------------------

# summary output of each indiivdual regression
# upcall call rate
sighta = lm(num_sighting ~ up_per_hr
                  , data = df)
# put model in matrix/array
sighta <-summary(sighta)$coefficients

# gunshot call rate
sightb = lm(num_sighting ~ gs_per_hr
                  , data = df)
# put model in matrix/array
sightb <-summary(sightb)$coefficients

# mid-freq rate
sightc = lm(num_sighting ~ mf_per_hr
                  , data = df)
# put model in matrix/array
sightc <-summary(sightc)$coefficients

# yday (not doing month)
sightd = lm(num_sighting ~ yday
                  , data = df)
# put model in matrix/array
sightd <-summary(sightd)$coefficients

# combine all gs rows into a dataframe
ns_df <- rbind(sighta,sightb,sightc,sightd)

# round
ns_df = round(ns_df,2)

# add y variable column
ns_df <- cbind(ns_df, "whale count")

# save data table 
write.csv(ns_df,"data/processed/num_sighting_regression_table_fliptest.csv")



# individual anovas
# upcall call rate
sighta = anova(lm(num_sighting ~ up_per_hr
            , data = df))

# gunshot call rate
sightb = anova(lm(num_sighting ~ gs_per_hr
            , data = df))

# mid-freq rate
sightc = anova(lm(num_sighting ~ mf_per_hr
            , data = df))

# yday (not doing month)
sightg = anova(lm(num_sighting ~ yday
            , data = df))

# combine each model into one big table
ns_df <- rbind(sighta,sightb,sightc,sightg)

# round
ns_df = round(ns_df,3)

# add y variable column
ns_df <- cbind(ns_df, "whale count")

# save data table 
write.csv(ns_df,"data/processed/num_sighting_anova_table_fliptest.csv")
