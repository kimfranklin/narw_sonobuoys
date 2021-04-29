# tbl_est_call_rates_stepwise_process.R #

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
                  num_sighting+offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# step model of the previous model to see what models should have in table
step.modelf1 <- stepAIC(sightf1, direction = "both", 
                       trace = TRUE)

# model - yday
sightf2 = glm.nb(up ~ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                   num_sighting+offset(log(rec_duration))
               , data = df, control=glm.control(maxit=100))

# model - yday and ratio
sightf3 = glm.nb(up ~foraging_bhv_whale+social_bhv_whale+
                   num_sighting+offset(log(rec_duration))
                 , data = df, control=glm.control(maxit=100))

# model -  yday and ratio and foraging
sightf4 = glm.nb(up ~social_bhv_whale+
                   num_sighting+offset(log(rec_duration))
                 , data = df, control=glm.control(maxit=100))

sumf1 = glance(sightf1)
sumf2 = glance(sightf2)
sumf3 = glance(sightf3)
sumf4 = glance(sightf4)

# combine all models into one large dataframe/table
smf = rbind(sumf1,sumf2,sumf3,sumf4)

smf$model = c('upcall = day of year + male/female ratio + foraging rate + socializing rate + whale count + offset(log(duration))',
              'upcall = male/female ratio + foraging rate + socializing rate + whale count + offset(log(duration))',
              'upcall = foraging rate + socializing rate + whale count + offset(log(duration))',
              'upcall = socializing rate + whale count + offset(log(duration))')

# gunshot
# model
sightg1 = glm.nb(gs ~yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                  num_sighting+offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# step model of the previous model
step.modelg1 <- stepAIC(sightg1, direction = "both", 
                       trace = TRUE)

# model - ratio
sightg2 = glm.nb(gs ~yday+foraging_bhv_whale+social_bhv_whale+
                   num_sighting+offset(log(rec_duration))
                 , data = df, control=glm.control(maxit=100))

# model - ratio and num sight
sightg3 = glm.nb(gs ~yday+foraging_bhv_whale+social_bhv_whale+
                   offset(log(rec_duration))
                 , data = df, control=glm.control(maxit=100))

# model - ratio and num sight and social 
sightg4 = glm.nb(gs ~yday+foraging_bhv_whale+
                   offset(log(rec_duration))
                 , data = df, control=glm.control(maxit=100))

# model - ratio and num sight and social and foraging
sightg5 = glm.nb(gs ~yday+
                   offset(log(rec_duration))
                 , data = df, control=glm.control(maxit=100))

sumg1 = glance(sightg1)
sumg2 = glance(sightg2)
sumg3 = glance(sightg3)
sumg4 = glance(sightg4)
sumg5 = glance(sightg5)

# combine all models into one large dataframe/table
smg = rbind(sumg1,sumg2,sumg3,sumg4,sumg5)

smg$model = c('gunshot = day of year + male/female ratio + foraging rate + socializing rate + whale count + offset(log(duration))',
              'gunshot = day of year + foraging rate + socializing rate + whale count + offset(log(duration))',
              'gunshot = day of year + foraging rate + socializing rate + offset(log(duration))',
              'gunshot = day of year + foraging rate + offset(log(duration))',
              'gunshot = day of year + offset(log(duration))')


# mid-freq
# model
sighth1 = glm.nb(mf ~yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                  num_sighting+offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# step model of the previous model
step.modelh1 <- stepAIC(sighth1, direction = "both", 
                       trace = TRUE)

# model - foraging
sighth2 = glm.nb(mf ~yday+ratio_male_female+social_bhv_whale+
                   num_sighting+offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# model - foraging and social
sighth3 = glm.nb(mf ~yday+ratio_male_female+
                   num_sighting+offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# model - foraging and socialand num sight
sighth4 = glm.nb(mf ~yday+ratio_male_female+
                   offset(log(rec_duration))
                 , data = df, control=glm.control(maxit=100))


sumh1 = glance(sighth1)
sumh2 = glance(sighth2)
sumh3 = glance(sighth3)
sumh4 = glance(sighth4)

# combine all models into one large dataframe/table
smh = rbind(sumh1,sumh2,sumh3,sumh4)

smh$model = c('tonal = day of year + male/female ratio + foraging rate + socializing rate + whale count + offset(log(duration))',
              'tonal = day of year + male/female ratio + socializing rate + whale count + offset(log(duration))',
              'tonal = day of year + male/female ratio + whale count + offset(log(duration))',
              'tonal = day of year + male/female ratio + offset(log(duration))')


# combine all models into one large dataframe/table
tmp_df = rbind(smf,smg,smh)

# round
tmp_df$null.deviance = round(as.numeric(tmp_df$null.deviance), 2)
tmp_df$logLik = round(as.numeric(tmp_df$logLik), 2)
tmp_df$AIC = round(as.numeric(tmp_df$AIC), 2)
tmp_df$BIC = round(as.numeric(tmp_df$BIC), 2)
tmp_df$deviance = round(as.numeric(tmp_df$deviance), 2)

# save data table 
write.csv(tmp_df,"data/processed/call_rate_stepwise_regression_table.csv")
