# tbl_est_num_sighting.R #

# tables for manuscript that estimate the number of whales

# libraries ---------------------------------------------------------------

library(tidyverse)
library(MASS)
# library(plyr)
library(data.table)

# input -------------------------------------------------------------------

# read in data
df = readRDS("data/processed/proc_acou_photoid.rds")

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

# save data table 
write.csv(ns_df,"data/processed/num_sighting_regression_table.csv")



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
# 
# # ratio
# sightd = anova(lm(num_sighting ~ ratio_male_female
#                   , data = df))
# 
# # foraging
# sighte = anova(lm(num_sighting ~ foraging_bhv_whale
#                   , data = df))
# 
# # social
# sightf = anova(lm(num_sighting ~ social_bhv_whale
#                   , data = df))

# yday (not doing month)
sightg = anova(lm(num_sighting ~ yday
            , data = df))

# combine each model into one big table
ns_df <- rbind(sighta,sightb,sightc,sightg)

# save data table 
write.csv(ns_df,"data/processed/num_sighting_anova_table.csv")

# # anova comparisons - when compared to null its exactly the same as the anova funtion above
# # null model
# #sightz = lm(num_sighting ~ up_per_hr+gs_per_hr+mf_per_hr+month, data = df)
# sightz = lm(num_sighting ~ 1)
# 
# # upcall call rate
# sighta = lm(num_sighting ~ up_per_hr
#                   , data = df)
# 
# # gunshot call rate
# sightb = lm(num_sighting ~ gs_per_hr
#                   , data = df)
# 
# # mid-freq rate
# sightc = lm(num_sighting ~ mf_per_hr
#                   , data = df)
# 
# # month
# sightg = lm(num_sighting ~ month
#                   , data = df)
# 
# # anovas
# # null vs. up
# anova(sightz,sighta)
# 
# # null vs. gs
# anova(sightz,sightb)
# 
# # null vs. mf
# anova(sightz,sightc)
# 
# # null vs. month
# anova(sightz,sightg)


# # kruskal-walis test 
# # number of whales comparison
# # Upcall
# a = kruskal.test(up_per_hr~num_sighting, data = df)
# a = do.call(rbind.data.frame, a)
# a=data.table::transpose(a)
# 
# # gunshot
# b = kruskal.test(gs_per_hr~num_sighting, data = df)
# b = do.call(rbind.data.frame, b)
# b=data.table::transpose(b)
# 
# # tonal
# c = kruskal.test(mf_per_hr~num_sighting, data = df)
# c = do.call(rbind.data.frame, c)
# c=data.table::transpose(c)
# 
# # month
# d = kruskal.test(month~num_sighting, data = df)
# d = do.call(rbind.data.frame, d)
# d=data.table::transpose(d)
# 
# # combine all rows into a dataframe
# kw_df <- rbind(a,b,c,d)
# 
# # rename column names
# kw_df = kw_df %>% 
#   dplyr::rename(
#     test_stat = V1,
#     degrees_freedom = V2,
#     p_val = V3,
#     test_type = V4,
#     variables_comp = V5
#   )
# 
# # set dataframe to be a table 
# setDT(kw_df)
# 
# # save data table 
# write.csv(kw_df,"data/processed/kw_est_num_sighting.csv")


# # individual models linear regression
# # upcall call rate
# sighta = lm(num_sighting ~ up_per_hr
#                 , data = df)
# 
# # put model in matrix/array
# sighta <-summary(sighta)$coefficients
# 
# # gunshot call rate
# sightb = lm(num_sighting ~ gs_per_hr
#              , data = df)
# 
# # put model in matrix/array
# sightb <-summary(sightb)$coefficients
# 
# # mid-freq rate
# sightc = lm(num_sighting ~ mf_per_hr
#              , data = df)
# 
# # put model in matrix/array
# sightc <-summary(sightc)$coefficients
# 
# # month
# sightd = lm(num_sighting ~ month
#              , data = df)
# 
# # put model in matrix/array
# sightd <-summary(sightd)$coefficients
# 
# # distance
# sighte = lm(num_sighting ~ dist
#              , data = df)
# 
# # put model in matrix/array
# sighte <-summary(sighte)$coefficients
# 
# # combine each model into one big table
# ns_df <- rbind(sighta,sightb,sightc,sightd,sighte)
# 
# # save data table 
# write.csv(ns_df,"data/processed/num_sighting_regression_table.csv")
