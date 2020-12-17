# tbl_est_num_sighting.R #

# tables for manuscript that estimate the number of whales

# libraries
library(tidyverse)
library(MASS)
library(plyr)

# read in data
df = readRDS("data/processed/proc_acou_photoid.rds")

# indivdual models
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

# month
sightd = lm(num_sighting ~ month
             , data = df)

# put model in matrix/array
sightd <-summary(sightd)$coefficients

# distance
sighte = lm(num_sighting ~ dist
             , data = df)

# put model in matrix/array
sighte <-summary(sighte)$coefficients

# combine each model into one big table
ns_df <- rbind(sighta,sightb,sightc,sightd,sighte)

# save data table 
write.csv(ns_df,"data/processed/num_sighting_regression_table.csv")
