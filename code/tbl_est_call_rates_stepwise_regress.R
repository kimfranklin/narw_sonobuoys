# tbl_est_call_rates_stepwise_regress.R #

# tables for manuscript that estimate the call rates stepwise regression

# libraries ---------------------------------------------------------------

library(tidyverse)
library(MASS)
library(plyr)
#library(sjPlot)
# library(kimisc)
# library(AICcmodavg)
library(broom)

# input -------------------------------------------------------------------

# read in data
df = readRDS("data/processed/proc_acou_photoid.rds")

# process -----------------------------------------------------------------

# upcall
# model
sightf1 = glm.nb(up ~yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# step model of the previous model to see what models should have in table
step.modelf1 <- stepAIC(sightf1, direction = "both", 
                       trace = TRUE)

# model - foraging
sightf2 = glm.nb(up ~yday+ratio_male_female+social_bhv_whale+
                 offset(log(rec_duration))
               , data = df, control=glm.control(maxit=100))

# model - foraging and ratio
sightf3 = glm.nb(up ~yday+social_bhv_whale+
                   offset(log(rec_duration))
                 , data = df, control=glm.control(maxit=100))

# model - foraging and ratio and yday
sightf4 = glm.nb(up ~social_bhv_whale+
                   offset(log(rec_duration))
                 , data = df, control=glm.control(maxit=100))

sumf1 = glance(sightf1)
sumf2 = glance(sightf2)
sumf3 = glance(sightf3)
sumf4 = glance(sightf4)

# combine all models into one large dataframe/table
smf = rbind(sumf1,sumf2,sumf3,sumf4)

smf$model = c('up=yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+offset(log(rec_duration))',
'up=yday+ratio_male_female+social_bhv_whale+offset(log(rec_duration))',
'up=yday+social_bhv_whale+offset(log(rec_duration))',
'up=social_bhv_whale+offset(log(rec_duration))')

# gunshot
# model
sightg1 = glm.nb(gs ~yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=10000))

# step model of the previous model
step.modelg1 <- stepAIC(sightg1, direction = "both", 
                       trace = TRUE)

# model - ratio
sightg2 = glm.nb(gs ~yday++foraging_bhv_whale+social_bhv_whale+
                   offset(log(rec_duration))
                 , data = df, control=glm.control(maxit=10000))

# model - ratio and social 
sightg3 = glm.nb(gs ~yday+foraging_bhv_whale+
                   offset(log(rec_duration))
                 , data = df, control=glm.control(maxit=10000))

# model - ratio and social and foraging
sightg4 = glm.nb(gs ~yday+
                   offset(log(rec_duration))
                 , data = df, control=glm.control(maxit=10000))

sumg1 = glance(sightg1)
sumg2 = glance(sightg2)
sumg3 = glance(sightg3)
sumg4 = glance(sightg4)

# combine all models into one large dataframe/table
smg = rbind(sumg1,sumg2,sumg3,sumg4)

smg$model = c('gs=yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+offset(log(rec_duration))',
              'gs=yday+ratio_male_female+foraging_bhv_whale+offset(log(rec_duration))',
              'gs=yday+foraging_bhv_whale+offset(log(rec_duration))',
              'gs=yday+offset(log(rec_duration))')


# mid-freq
# model
sighth1 = glm.nb(mf ~yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# step model of the previous model
step.modelh1 <- stepAIC(sighth1, direction = "both", 
                       trace = TRUE)

# model - foraging
sighth2 = glm.nb(mf ~yday+ratio_male_female+social_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# model - foraging and social
sighth3 = glm.nb(mf ~yday+ratio_male_female+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))
summary(sighth3)

sumh1 = glance(sighth1)
sumh2 = glance(sighth2)
sumh3 = glance(sighth3)

# combine all models into one large dataframe/table
smh = rbind(sumh1,sumh2,sumh3)

smh$model = c('mf=yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+offset(log(rec_duration))',
              'mf=yday+ratio_male_female+foraging_bhv_whale+offset(log(rec_duration))',
              'mf=yday+ratio_male_female+offset(log(rec_duration))')


# combine all models into one large dataframe/table
tmp_df = rbind(smf,smg,smh)

# save data table 
write.csv(tmp_df,"data/processed/call_rate_stepwise_regression_table.csv")
