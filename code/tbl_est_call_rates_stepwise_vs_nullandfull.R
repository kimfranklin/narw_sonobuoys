# tbl_est_call_rates_stepwise_vs_nullandfull.R #

# Stepwise regression of full call rate models are compared to null call rate 
# models and full call rate models (respectively).
# Full models have month, ration male to female, foraging rate and social rate.

# libraries
library(tidyverse)
library(MASS)
library(plyr)

# read in data
df = readRDS("data/processed/proc_acou_photoid.rds")

# anovas comparing null and stepwise results ------------------------------

# upcall
# null model
sightfnull = glm.nb(up ~ offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# full model
sightf = glm.nb(up ~month+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                   offset(log(rec_duration))
                 , data = df, control=glm.control(maxit=100))

# step model of the previous model
step.modelf <- stepAIC(sightf, direction = "both",
                       trace = TRUE)

null_up_anova = anova(sightfnull,step.modelf)

# gunshot
# null model
sightgnull = glm.nb(gs ~ offset(log(rec_duration))
                    , data = df, control=glm.control(maxit=100))

# full model
sightg = glm.nb(gs ~month+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# step model of the previous model
step.modelg <- stepAIC(sightg, direction = "both",
                       trace = TRUE)

null_gs_anova = anova(sightgnull,step.modelg)

# mid-freq
# null model
sighthnull = glm.nb(mf ~ offset(log(rec_duration))
                    , data = df, control=glm.control(maxit=100))

# full model
sighth = glm.nb(mf ~month+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# step model of the previous model
step.modelh <- stepAIC(sighth, direction = "both",
                       trace = TRUE)

null_mf_anova = anova(sighthnull,step.modelh)

# combine all models into one large dataframe/table
null_anova_bind = rbind(null_up_anova,null_gs_anova,null_mf_anova)

# save data table
write.csv(null_anova_bind,"data/processed/call_rate_stepwise_vs_null_table.csv")


# anovas comparing full and stepwise results ------------------------------

# upcall
# model
sightf = glm.nb(up ~month+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# step model of the previous model
step.modelf <- stepAIC(sightf, direction = "both",
                       trace = TRUE)

full_up_anova = anova(sightf,step.modelf)

# gunshot
# model
sightg = glm.nb(gs ~month+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# step model of the previous model
step.modelg <- stepAIC(sightg, direction = "both",
                       trace = TRUE)

full_gs_anova = anova(sightg,step.modelg)

# mid-freq
# model
sighth = glm.nb(mf ~month+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# step model of the previous model
step.modelh <- stepAIC(sighth, direction = "both",
                       trace = TRUE)

full_mf_anova = anova(sighth,step.modelh)

# combine all models into one large dataframe/table
full_anova_bind = rbind(full_up_anova,full_gs_anova,full_mf_anova)

# save data table
write.csv(full_anova_bind,"data/processed/call_rate_stepwise_vs_full_table.csv")
