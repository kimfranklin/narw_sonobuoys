
# libraries
library(data.table)
library(tidyverse)

# input 
# read in data 
df = readRDS("data/processed/proc_acou_photoid.rds")

# add column in df for entangled and not entangled
df$entangled = c('False','False','True','True','False','False','False','False','False','False',
                 'True','False','False','False','False','False','False','False','False','False',
                 'False','False','False','False','False','False','False','False','True','True',
                 'False','False','False','False','False','False','False')

colnames(df)

df2<-df[!(df$entangled=="True"),]

# double check the number of rows to make sure they were actually removed

# tbl_est_num_sighting.R #

# tables for manuscript that estimate the number of whales

# libraries ---------------------------------------------------------------

library(tidyverse)
library(MASS)
library(data.table)

# process -----------------------------------------------------------------

# summary output of each indiivdual regression
# upcall call rate
sighta = lm(num_sighting ~ up_per_hr
            , data = df2)
# put model in matrix/array
sighta <-summary(sighta)$coefficients

# gunshot call rate
sightb = lm(num_sighting ~ gs_per_hr
            , data = df2)
# put model in matrix/array
sightb <-summary(sightb)$coefficients

# mid-freq rate
sightc = lm(num_sighting ~ mf_per_hr
            , data = df2)
# put model in matrix/array
sightc <-summary(sightc)$coefficients

# yday (not doing month)
sightd = lm(num_sighting ~ yday
            , data = df2)
# put model in matrix/array
sightd <-summary(sightd)$coefficients

# combine all gs rows into a dataframe
ns_df <- rbind(sighta,sightb,sightc,sightd)

# round
ns_df = round(ns_df,2)

# add y variable column
ns_df <- cbind(ns_df, "whale count")

# save data table 
write.csv(ns_df,"data/processed/num_sighting_regression_table_noentangledwhales.csv")



# individual anovas
# upcall call rate
sighta = anova(lm(num_sighting ~ up_per_hr
                  , data = df2))

# gunshot call rate
sightb = anova(lm(num_sighting ~ gs_per_hr
                  , data = df2))

# mid-freq rate
sightc = anova(lm(num_sighting ~ mf_per_hr
                  , data = df2))

# yday (not doing month)
sightg = anova(lm(num_sighting ~ yday
                  , data = df2))

# combine each model into one big table
ns_df <- rbind(sighta,sightb,sightc,sightg)

# round
ns_df = round(ns_df,3)

# add y variable column
ns_df <- cbind(ns_df, "whale count")

# save data table 
write.csv(ns_df,"data/processed/num_sighting_anova_table_noentangledwhales.csv")


# tbl_est_call_rates.R #

# tables for manuscript that estimate the call rates and their likelihood ratio
# tests (rather than reporting p-values from summary table (see first half of 
# script) MASS fxn anova is used to get p-values)

# libraries ---------------------------------------------------------------

library(tidyverse)
library(MASS)
library(plyr)
#library(sjPlot)

# process -----------------------------------------------------------------
# Upcall ------------------------------------------------------------------
# yday
sighta = glm.nb(up ~ yday+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# put model in matrix/array
sighta <-summary(sighta)$coefficients

# ratio
sightb = glm.nb(up ~ ratio_male_female+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# put model in matrix/array
sightb <-summary(sightb)$coefficients

# number of whales
sightc = glm.nb(up ~ num_sighting+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# put model in matrix/array
sightc <-summary(sightc)$coefficients

# foraging bhv
sightd = glm.nb(up ~ foraging_bhv_whale+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# put model in matrix/array
sightd <-summary(sightd)$coefficients

# social bhv
sighte = glm.nb(up ~social_bhv_whale+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# put model in matrix/array
sighte <-summary(sighte)$coefficients

# combine all upcall rows into a dataframe
cru_df <- rbind(sighta,sightb,sightc,sightd,sighte)

# round 
cru_df = round(cru_df,3)

# add call type column
cru_df <- cbind(cru_df, "upcall") 

# Gunshot -----------------------------------------------------------------
# yday
sighta = glm.nb(gs ~ yday+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# put model in matrix/array
sighta <-summary(sighta)$coefficients

# ratio
sightb = glm.nb(gs ~ ratio_male_female+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# put model in matrix/array
sightb <-summary(sightb)$coefficients

# number of whales
sightc = glm.nb(gs ~ num_sighting+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# put model in matrix/array
sightc <-summary(sightc)$coefficients

# foraging bhv
sightd = glm.nb(gs ~ foraging_bhv_whale+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# put model in matrix/array
sightd <-summary(sightd)$coefficients

# social rate
sighte = glm.nb(gs ~social_bhv_whale+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# put model in matrix/array
sighte <-summary(sighte)$coefficients

# combine all gs rows into a dataframe
crg_df <- rbind(sighta,sightb,sightc,sightd,sighte)

# round 
crg_df = round(crg_df,3)

# add call type column
crg_df <- cbind(crg_df, "gunshot") 

# Mid-freq ----------------------------------------------------------------
# yday
sighta = glm.nb(mf ~ yday+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# put model in matrix/array
sighta <-summary(sighta)$coefficients

# ratio
sightb = glm.nb(mf ~ ratio_male_female+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# put model in matrix/array
sightb <-summary(sightb)$coefficients

# number of whales sighted
sightc = glm.nb(mf ~ num_sighting+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# put model in matrix/array
sightc <-summary(sightc)$coefficients

# foraging bhv
sightd = glm.nb(mf ~ foraging_bhv_whale+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# put model in matrix/array
sightd <-summary(sightd)$coefficients

# social bhv
sighte = glm.nb(mf ~social_bhv_whale+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# put model in matrix/array
sighte <-summary(sighte)$coefficients

# combine all mf rows into a dataframe
crm_df <- rbind(sighta,sightb,sightc,sightd,sighte)

# round 
crm_df = round(crm_df,3)

# add call type column
crm_df <- cbind(crm_df, "tonal") 


# combine all call types into single dataframe
cr_df = rbind(cru_df,crg_df,crm_df)

# save data table 
write.csv(cr_df,"data/processed/call_rate_regression_table_noentangledwhales.csv")


# LIKELIHOOD RATIO TESTS
# process -----------------------------------------------------------------
# Upcall ------------------------------------------------------------------
# null model
sightunull = glm.nb(up ~ offset(log(rec_duration))
                    , data = df2, control=glm.control(maxit=100))

# yday
sighta = glm.nb(up ~ yday+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# likelihood ratio test
sighta_anova = anova(sightunull,sighta)

# ratio
sightb = glm.nb(up ~ ratio_male_female+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# likelihood ratio test
sightb_anova = anova(sightunull,sightb)

# number of whales
sightc = glm.nb(up ~ num_sighting+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# likelihood ratio test
sightc_anova = anova(sightunull,sightc)

# foraging bhv
sightd = glm.nb(up ~ foraging_bhv_whale+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# likelihood ratio test
sightd_anova = anova(sightunull,sightd)

# social bhv
sighte = glm.nb(up ~social_bhv_whale+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# likelihood ratio test
sighte_anova = anova(sightunull,sighte)

# combine all upcall rows into a dataframe
cru_df <- rbind(sighta_anova,sightb_anova,sightc_anova,sightd_anova,sighte_anova)

# round
cru_df$theta = round(as.numeric(cru_df$theta), 2)
cru_df$`LR stat.` = round(as.numeric(cru_df$`LR stat.`), 2)
cru_df$`Pr(Chi)` = round(as.numeric(cru_df$`Pr(Chi)`), 3)
cru_df$`   2 x log-lik.` = round(as.numeric(cru_df$`   2 x log-lik.`), 2)

# add call type column
cru_df <- cbind(cru_df, as.character('upcall'))

# rename column names
cru_df = cru_df %>% 
  dplyr::rename(
    call_type = `as.character("upcall")`
  )

# Gunshot -----------------------------------------------------------------
# null model
sightgnull = glm.nb(gs ~ offset(log(rec_duration))
                    , data = df2, control=glm.control(maxit=100))

# yday
sighta = glm.nb(gs ~ yday+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# likelihood ratio test
sighta_anova = anova(sightgnull,sighta)

# ratio
sightb = glm.nb(gs ~ ratio_male_female+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# likelihood ratio test
sightb_anova = anova(sightgnull,sightb)

# number of whales
sightc = glm.nb(gs ~ num_sighting+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# likelihood ratio test
sightc_anova = anova(sightgnull,sightc)

# foraging bhv
sightd = glm.nb(gs ~ foraging_bhv_whale+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# likelihood ratio test
sightd_anova = anova(sightgnull,sightd)

# social rate
sighte = glm.nb(gs ~social_bhv_whale+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# likelihood ratio test
sighte_anova = anova(sightgnull,sighte)

# combine all gs rows into a dataframe
crg_df <- rbind(sighta_anova,sightb_anova,sightc_anova,sightd_anova,sighte_anova)

# round
crg_df$theta = round(as.numeric(crg_df$theta), 2)
crg_df$`LR stat.` = round(as.numeric(crg_df$`LR stat.`), 2)
crg_df$`Pr(Chi)` = round(as.numeric(crg_df$`Pr(Chi)`), 3)
crg_df$`   2 x log-lik.` = round(as.numeric(crg_df$`   2 x log-lik.`), 2)

# add call type column
crg_df <- cbind(crg_df, "gunshot") 

# rename column names
crg_df = crg_df %>% 
  dplyr::rename(
    call_type = `"gunshot"`
  )

# Mid-freq ----------------------------------------------------------------
# null model
sightmnull = glm.nb(mf ~ offset(log(rec_duration))
                    , data = df2, control=glm.control(maxit=100))

# yday
sighta = glm.nb(mf ~ yday+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# likelihood ratio test
sighta_anova = anova(sightmnull,sighta)

# ratio
sightb = glm.nb(mf ~ ratio_male_female+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# likelihood ratio test
sightb_anova = anova(sightmnull,sightb)

# number of whales sighted
sightc = glm.nb(mf ~ num_sighting+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# likelihood ratio test
sightc_anova = anova(sightmnull,sightc)

# foraging bhv
sightd = glm.nb(mf ~ foraging_bhv_whale+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# likelihood ratio test
sightd_anova = anova(sightmnull,sightd)

# social bhv
sighte = glm.nb(mf ~social_bhv_whale+
                  offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# likelihood ratio test
sighte_anova = anova(sightmnull,sighte)

# combine all mf rows into a dataframe
crm_df <- rbind(sighta_anova,sightb_anova,sightc_anova,sightd_anova,sighte_anova)

# round
crm_df$theta = round(as.numeric(crm_df$theta), 2)
crm_df$`LR stat.` = round(as.numeric(crm_df$`LR stat.`), 2)
crm_df$`Pr(Chi)` = round(as.numeric(crm_df$`Pr(Chi)`), 3)
crm_df$`   2 x log-lik.` = round(as.numeric(crm_df$`   2 x log-lik.`), 2)

# add call type column
crm_df <- cbind(crm_df, "tonal") 

# rename column names
crm_df = crm_df %>% 
  dplyr::rename(
    call_type = `"tonal"`
  )


# combine all call types into single dataframe
cr_df = rbind(cru_df,crg_df,crm_df)

# save data table 
write.csv(cr_df,"data/processed/call_rate_likelihood_table_noentangledwhales.csv")



## tbl_est_call_rates_stepwise.R ##

# computing the coefficients of the stepwise produced models for call rates and
# stepwise regression of full call rate models are compared to null call rate 
# models.

# libraries ---------------------------------------------------------------

library(tidyverse)
library(MASS)
library(plyr)
library(broom)

# process -----------------------------------------------------------------

# upcall
# model
sightf1 = glm.nb(up ~yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                   num_sighting+offset(log(rec_duration))
                 , data = df2, control=glm.control(maxit=100))

# step model of the previous model to see what models should have in table
step.modelf1 <- stepAIC(sightf1, direction = "both", 
                        trace = TRUE)

# put model in matrix/array
sighta <-summary(step.modelf1)$coefficients

# round
sighta = round(sighta, 2)

# add call type column
sighta <- cbind(sighta, "upcall") 

# gunshot
# model
sightf2 = glm.nb(gs ~yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                   num_sighting+offset(log(rec_duration))
                 , data = df2, control=glm.control(maxit=100))

# step model of the previous model to see what models should have in table
step.modelf2 <- stepAIC(sightf2, direction = "both", 
                        trace = TRUE)

# put model in matrix/array
sightb <-summary(step.modelf2)$coefficients

# round
sightb = round(sightb, 2)

# add call type column
sightb <- cbind(sightb, "gunshot") 

# mid-freq
# model
sightf3 = glm.nb(mf ~yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                   num_sighting+offset(log(rec_duration))
                 , data = df2, control=glm.control(maxit=100))

# step model of the previous model to see what models should have in table
step.modelf3 <- stepAIC(sightf3, direction = "both", 
                        trace = TRUE)

# put model in matrix/array
sightc <-summary(step.modelf3)$coefficients

# round
sightc = round(sightc, 2)

# add call type column
sightc <- cbind(sightc, "tonal") 


# combine all mf rows into a dataframe
crm_df <- rbind(sighta,sightb,sightc)

# save data table 
write.csv(crm_df,"data/processed/call_rate_stepregression_coeff_table_noentangledwhales.csv")



# anovas comparing null and stepwise results ------------------------------

# upcall
# null model
sightfnull = glm.nb(up ~ offset(log(rec_duration))
                    , data = df2, control=glm.control(maxit=100))

# full model
sightf = glm.nb(up ~yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                  num_sighting+offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# step model of the previous model
step.modelf <- stepAIC(sightf, direction = "both",
                       trace = TRUE)

null_up_anova = anova(sightfnull,step.modelf)

# round
null_up_anova$theta = round(as.numeric(null_up_anova$theta), 2)
null_up_anova$`LR stat.` = round(as.numeric(null_up_anova$`LR stat.`), 2)
null_up_anova$`Pr(Chi)` = round(as.numeric(null_up_anova$`Pr(Chi)`), 3)
null_up_anova$`   2 x log-lik.` = round(as.numeric(null_up_anova$`   2 x log-lik.`), 2)

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
                    , data = df2, control=glm.control(maxit=100))

# full model
sightg = glm.nb(gs ~yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                  num_sighting+offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# step model of the previous model
step.modelg <- stepAIC(sightg, direction = "both",
                       trace = TRUE)

null_gs_anova = anova(sightgnull,step.modelg)

# round
null_gs_anova$theta = round(as.numeric(null_gs_anova$theta), 2)
null_gs_anova$`LR stat.` = round(as.numeric(null_gs_anova$`LR stat.`), 2)
null_gs_anova$`Pr(Chi)` = round(as.numeric(null_gs_anova$`Pr(Chi)`), 3)
null_gs_anova$`   2 x log-lik.` = round(as.numeric(null_gs_anova$`   2 x log-lik.`), 2)

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
                    , data = df2, control=glm.control(maxit=100))

# full model
sighth = glm.nb(mf ~yday+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
                  num_sighting+offset(log(rec_duration))
                , data = df2, control=glm.control(maxit=100))

# step model of the previous model
step.modelh <- stepAIC(sighth, direction = "both",
                       trace = TRUE)

null_mf_anova = anova(sighthnull,step.modelh)

# round
null_mf_anova$theta = round(as.numeric(null_mf_anova$theta), 2)
null_mf_anova$`LR stat.` = round(as.numeric(null_mf_anova$`LR stat.`), 2)
null_mf_anova$`Pr(Chi)` = round(as.numeric(null_mf_anova$`Pr(Chi)`), 3)
null_mf_anova$`   2 x log-lik.` = round(as.numeric(null_mf_anova$`   2 x log-lik.`), 2)

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
write.csv(null_anova_bind,"data/processed/call_rate_stepwise_vs_null_table_noentangledwhales.csv")
