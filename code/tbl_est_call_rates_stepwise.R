## tbl_est_call_rates_stepwise.R ##

# computing the coefficients of the stepwise produced models for call rates and
# stepwise regression of full call rate models are compared to null call rate 
# models.

# libraries ---------------------------------------------------------------

library(tidyverse)
library(MASS)
library(plyr)
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

# put model in matrix/array
sighta <-summary(step.modelf1)$coefficients

# round
sighta = round(sighta, 3)

# add call type column
sighta <- cbind(sighta, "upcall") 

# gunshot
# model
sightf2 = glm.nb(gs ~yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                   num_sighting+offset(log(rec_duration))
                 , data = df, control=glm.control(maxit=100))

# step model of the previous model to see what models should have in table
step.modelf2 <- stepAIC(sightf2, direction = "both", 
                        trace = TRUE)

# put model in matrix/array
sightb <-summary(step.modelf2)$coefficients

# round
sightb = round(sightb, 3)

# add call type column
sightb <- cbind(sightb, "gunshot") 

# mid-freq
# model
sightf3 = glm.nb(mf ~yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                   num_sighting+offset(log(rec_duration))
                 , data = df, control=glm.control(maxit=100))

# step model of the previous model to see what models should have in table
step.modelf3 <- stepAIC(sightf3, direction = "both", 
                        trace = TRUE)

# put model in matrix/array
sightc <-summary(step.modelf3)$coefficients

# round
sightc = round(sightc, 3)

# add call type column
sightc <- cbind(sightc, "tonal") 


# combine all mf rows into a dataframe
crm_df <- rbind(sighta,sightb,sightc)

# save data table 
write.csv(crm_df,"data/processed/call_rate_stepregression_coeff_table.csv")



# anovas comparing null and stepwise results ------------------------------

# upcall
# null model
sightfnull = glm.nb(up ~ offset(log(rec_duration))
                    , data = df, control=glm.control(maxit=100))

# full model
sightf = glm.nb(up ~yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                  num_sighting+offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# step model of the previous model
step.modelf <- stepAIC(sightf, direction = "both",
                       trace = TRUE)

null_up_anova = anova(sightfnull,step.modelf)

# round
null_up_anova$theta = round(as.numeric(null_up_anova$theta), 3)
null_up_anova$`LR stat.` = round(as.numeric(null_up_anova$`LR stat.`), 3)
null_up_anova$`Pr(Chi)` = round(as.numeric(null_up_anova$`Pr(Chi)`), 3)
null_up_anova$`   2 x log-lik.` = round(as.numeric(null_up_anova$`   2 x log-lik.`), 3)

# add call type column
null_up_anova <- cbind(null_up_anova, 'upcall')

# rename column names
null_up_anova = null_up_anova %>% 
  dplyr::rename(
    call_type = `"upcall"`
  )

# gunshot
# null model
sightgnull = glm.nb(gs ~ offset(log(rec_duration))
                    , data = df, control=glm.control(maxit=100))

# full model
sightg = glm.nb(gs ~yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                  num_sighting+offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# step model of the previous model
step.modelg <- stepAIC(sightg, direction = "both",
                       trace = TRUE)

null_gs_anova = anova(sightgnull,step.modelg)

# round
null_gs_anova$theta = round(as.numeric(null_gs_anova$theta), 3)
null_gs_anova$`LR stat.` = round(as.numeric(null_gs_anova$`LR stat.`), 3)
null_gs_anova$`Pr(Chi)` = round(as.numeric(null_gs_anova$`Pr(Chi)`), 3)
null_gs_anova$`   2 x log-lik.` = round(as.numeric(null_gs_anova$`   2 x log-lik.`), 3)

# add call type column
null_gs_anova <- cbind(null_gs_anova, 'gunshot')

# rename column names
null_gs_anova = null_gs_anova %>% 
  dplyr::rename(
    call_type = `"gunshot"`
  )

# mid-freq
# null model
sighthnull = glm.nb(mf ~ offset(log(rec_duration))
                    , data = df, control=glm.control(maxit=100))

# full model
sighth = glm.nb(mf ~yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                  num_sighting+offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# step model of the previous model
step.modelh <- stepAIC(sighth, direction = "both",
                       trace = TRUE)

null_mf_anova = anova(sighthnull,step.modelh)

# round
null_mf_anova$theta = round(as.numeric(null_mf_anova$theta), 3)
null_mf_anova$`LR stat.` = round(as.numeric(null_mf_anova$`LR stat.`), 3)
null_mf_anova$`Pr(Chi)` = round(as.numeric(null_mf_anova$`Pr(Chi)`), 3)
null_mf_anova$`   2 x log-lik.` = round(as.numeric(null_mf_anova$`   2 x log-lik.`), 3)

# add call type column
null_mf_anova <- cbind(null_mf_anova, 'tonal')

# rename column names
null_mf_anova = null_mf_anova %>% 
  dplyr::rename(
    call_type = `"tonal"`
  )


# combine all models into one large dataframe/table
null_anova_bind = rbind(null_up_anova,null_gs_anova,null_mf_anova)

# save data table
write.csv(null_anova_bind,"data/processed/call_rate_stepwise_vs_null_table.csv")
