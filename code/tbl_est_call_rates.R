# tbl_est_call_rates.R #

# tables for manuscript that estimate the call rates and their likelihood ratio
# tests (rather than reporting p-values from summary table (see first half of 
# script) MASS fxn anova is used to get p-values)

# libraries ---------------------------------------------------------------

library(tidyverse)
library(MASS)
library(plyr)
#library(sjPlot)

# input -------------------------------------------------------------------

# read in data
df = readRDS("data/processed/proc_acou_photoid.rds")

# process -----------------------------------------------------------------
# Upcall ------------------------------------------------------------------
# yday
sighta = glm.nb(up ~ yday+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))
#summary(sighta)
#tab_model(sighta)

# put model in matrix/array
sighta <-summary(sighta)$coefficients

# test = unlist(sighta)
# test = as.data.frame(matrix(c(3, rep(NA, length(sighta) %% 3)), 3))
# test = do.call(rbind, lapply(names(sighta[[1]]), function(u) transform(sighta[[1]][[u]])))
# test = as.data.frame(summary(sighta))
# sighta=data.table::transpose(sighta)

# ratio
sightb = glm.nb(up ~ ratio_male_female+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# put model in matrix/array
sightb <-summary(sightb)$coefficients

# number of whales
sightc = glm.nb(up ~ num_sighting+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# put model in matrix/array
sightc <-summary(sightc)$coefficients

# foraging bhv
sightd = glm.nb(up ~ foraging_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# put model in matrix/array
sightd <-summary(sightd)$coefficients

# social bhv
sighte = glm.nb(up ~social_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# put model in matrix/array
sighte <-summary(sighte)$coefficients

#tab_model(sighta,sightb,sightc,sightd,sightd)

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
                , data = df, control=glm.control(maxit=100))

# put model in matrix/array
sighta <-summary(sighta)$coefficients

# ratio
sightb = glm.nb(gs ~ ratio_male_female+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# put model in matrix/array
sightb <-summary(sightb)$coefficients

# number of whales
sightc = glm.nb(gs ~ num_sighting+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# put model in matrix/array
sightc <-summary(sightc)$coefficients

# foraging bhv
sightd = glm.nb(gs ~ foraging_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# put model in matrix/array
sightd <-summary(sightd)$coefficients

# social rate
sighte = glm.nb(gs ~social_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

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
                , data = df, control=glm.control(maxit=100))

# put model in matrix/array
sighta <-summary(sighta)$coefficients

# ratio
sightb = glm.nb(mf ~ ratio_male_female+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# put model in matrix/array
sightb <-summary(sightb)$coefficients

# number of whales sighted
sightc = glm.nb(mf ~ num_sighting+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# put model in matrix/array
sightc <-summary(sightc)$coefficients

# foraging bhv
sightd = glm.nb(mf ~ foraging_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# put model in matrix/array
sightd <-summary(sightd)$coefficients

# social bhv
sighte = glm.nb(mf ~social_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# put model in matrix/array
sighte <-summary(sighte)$coefficients

# combine all mf rows into a dataframe
crm_df <- rbind(sighta,sightb,sightc,sightd,sighte)

# round 
crm_df = round(crm_df,3)

# add call type column
crm_df <- cbind(crm_df, "tonal") 


# # combine all call types into single dataframe
# cr_df = cbind(cru_df,crg_df,crm_df)

# combine all call types into single dataframe
cr_df = rbind(cru_df,crg_df,crm_df)

# save data table 
write.csv(cr_df,"data/processed/call_rate_regression_table.csv")


# LIKELIHOOD RATIO TESTS
# process -----------------------------------------------------------------
# Upcall ------------------------------------------------------------------
# null model
sightunull = glm.nb(up ~ offset(log(rec_duration))
                    , data = df, control=glm.control(maxit=100))

# yday
sighta = glm.nb(up ~ yday+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# likelihood ratio test
sighta_anova = anova(sightunull,sighta)

# ratio
sightb = glm.nb(up ~ ratio_male_female+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# likelihood ratio test
sightb_anova = anova(sightunull,sightb)

# number of whales
sightc = glm.nb(up ~ num_sighting+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# likelihood ratio test
sightc_anova = anova(sightunull,sightc)

# foraging bhv
sightd = glm.nb(up ~ foraging_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# likelihood ratio test
sightd_anova = anova(sightunull,sightd)

# social bhv
sighte = glm.nb(up ~social_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# likelihood ratio test
sighte_anova = anova(sightunull,sighte)

# combine all upcall rows into a dataframe
cru_df <- rbind(sighta_anova,sightb_anova,sightc_anova,sightd_anova,sighte_anova)

# round
cru_df$theta = round(as.numeric(cru_df$theta), 3)
cru_df$`LR stat.` = round(as.numeric(cru_df$`LR stat.`), 3)
cru_df$`Pr(Chi)` = round(as.numeric(cru_df$`Pr(Chi)`), 3)
cru_df$`   2 x log-lik.` = round(as.numeric(cru_df$`   2 x log-lik.`), 3)

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
                    , data = df, control=glm.control(maxit=100))

# yday
sighta = glm.nb(gs ~ yday+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# likelihood ratio test
sighta_anova = anova(sightgnull,sighta)

# ratio
sightb = glm.nb(gs ~ ratio_male_female+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# likelihood ratio test
sightb_anova = anova(sightgnull,sightb)

# number of whales
sightc = glm.nb(gs ~ num_sighting+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# likelihood ratio test
sightc_anova = anova(sightgnull,sightc)

# foraging bhv
sightd = glm.nb(gs ~ foraging_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# likelihood ratio test
sightd_anova = anova(sightgnull,sightd)

# social rate
sighte = glm.nb(gs ~social_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# likelihood ratio test
sighte_anova = anova(sightgnull,sighte)

# combine all gs rows into a dataframe
crg_df <- rbind(sighta_anova,sightb_anova,sightc_anova,sightd_anova,sighte_anova)

# round
crg_df$theta = round(as.numeric(crg_df$theta), 3)
crg_df$`LR stat.` = round(as.numeric(crg_df$`LR stat.`), 3)
crg_df$`Pr(Chi)` = round(as.numeric(crg_df$`Pr(Chi)`), 3)
crg_df$`   2 x log-lik.` = round(as.numeric(crg_df$`   2 x log-lik.`), 3)

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
                    , data = df, control=glm.control(maxit=100))

# yday
sighta = glm.nb(mf ~ yday+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# likelihood ratio test
sighta_anova = anova(sightmnull,sighta)

# ratio
sightb = glm.nb(mf ~ ratio_male_female+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# likelihood ratio test
sightb_anova = anova(sightmnull,sightb)

# number of whales sighted
sightc = glm.nb(mf ~ num_sighting+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# likelihood ratio test
sightc_anova = anova(sightmnull,sightc)

# foraging bhv
sightd = glm.nb(mf ~ foraging_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# likelihood ratio test
sightd_anova = anova(sightmnull,sightd)

# social bhv
sighte = glm.nb(mf ~social_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))

# likelihood ratio test
sighte_anova = anova(sightmnull,sighte)

# combine all mf rows into a dataframe
crm_df <- rbind(sighta_anova,sightb_anova,sightc_anova,sightd_anova,sighte_anova)

# round
crm_df$theta = round(as.numeric(crm_df$theta), 3)
crm_df$`LR stat.` = round(as.numeric(crm_df$`LR stat.`), 3)
crm_df$`Pr(Chi)` = round(as.numeric(crm_df$`Pr(Chi)`), 3)
crm_df$`   2 x log-lik.` = round(as.numeric(crm_df$`   2 x log-lik.`), 3)

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
write.csv(cr_df,"data/processed/call_rate_likelihood_table.csv")





# stepwise regressions ----------------------------------------------------
# all significant terms from the above models are put into a single regression
# from that larger regression a stepwise comparison is done to see the 'ultimate'
# significant terms for estimating that call type

# # upcall
# # model
# sightf = glm.nb(up ~month+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
#                   offset(log(rec_duration))
#                 , data = df, control=glm.control(maxit=100))
# 
# # put model in matrix/array
# sightf1 <-summary(sightf)$coefficients
# 
# # convert matirx/array to dataframe
# sightf1 = as.data.frame(sightf1)
# 
# # add name row so we know what variable is for what row
# sightf1$names = rownames(sightf1)
# 
# # step model of the previous model
# step.modelf <- stepAIC(sightf, direction = "both", 
#                       trace = TRUE)
# 
# # put model in matrix/array
# step.modelf1 <- summary(step.modelf)$coefficients
# 
# # convert matirx/array to dataframe
# step.modelf1 = as.data.frame(step.modelf1)
# 
# # add name row so we know what variable is for what row
# step.modelf1$names = rownames(step.modelf1)
# 
# # combine both models into one dataframe by adding columns for each model
# smf_df <- merge(sightf1,step.modelf1,by = 'names', all = T)
# 
# # gunshot
# # model
# sightg = glm.nb(gs ~month+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
#                   offset(log(rec_duration))
#                 , data = df, control=glm.control(maxit=100))
# 
# # put model in matrix/array
# sightg1 <-summary(sightg)$coefficients
# 
# # convert matirx/array to dataframe
# sightg1 = as.data.frame(sightg1)
# 
# # add name row so we know what variable is for what row
# sightg1$names = rownames(sightg1)
# 
# # step model of the previous model
# step.modelg <- stepAIC(sightg, direction = "both", 
#                        trace = TRUE)
# 
# # put model in matrix/array
# step.modelg1 <- summary(step.modelg)$coefficients
# 
# # convert matirx/array to dataframe
# step.modelg1 = as.data.frame(step.modelg1)
# 
# # add name row so we know what variable is for what row
# step.modelg1$names = rownames(step.modelg1)
# 
# # combine both models into one dataframe by adding columns for each model
# smg_df <- merge(sightg1,step.modelg1,by = 'names', all = T)
# 
# # mid-freq
# # model
# sighth = glm.nb(mf ~month+ratio_male_female+foraging_bhv_whale+social_bhv_whale+
#                   offset(log(rec_duration))
#                 , data = df, control=glm.control(maxit=100))
# 
# # put model in matrix/array
# sighth1 <-summary(sighth)$coefficients
# 
# # convert matirx/array to dataframe
# sighth1 = as.data.frame(sighth1)
# 
# # add name row so we know what variable is for what row
# sighth1$names = rownames(sighth1)
# 
# # step model of the previous model
# step.modelh <- stepAIC(sighth, direction = "both", 
#                        trace = TRUE)
# 
# # put model in matrix/array
# step.modelh1 <- summary(step.modelh)$coefficients
# 
# # convert matirx/array to dataframe
# step.modelh1 = as.data.frame(step.modelh1)
# 
# # add name row so we know what variable is for what row
# step.modelh1$names = rownames(step.modelh1)
# 
# # combine both models into one dataframe by adding columns for each model
# smh_df <- merge(sighth1,step.modelh1,by = 'names', all = T)
# 
# 
# # combine all models into one large dataframe/table
# tmp_df = rbind(smf_df,smg_df,smh_df)
# 
# # save data table 
# write.csv(tmp_df,"data/processed/call_rate_stepwise_regression_table.csv")
