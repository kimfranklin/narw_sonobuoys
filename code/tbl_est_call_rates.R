# tbl_est_call_rates.R #

# tables for manuscript that estimate the call rates

# libraries
library(tidyverse)
library(MASS)
library(plyr)
#library(sjPlot)

# read in data
df = readRDS("data/processed/proc_acou_photoid.rds")

# Upcall ------------------------------------------------------------------
# month
sighta = glm.nb(up ~ month+
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


# Gunshot -----------------------------------------------------------------
# month
sighta = glm.nb(gs ~ month+
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


# Mid-freq ----------------------------------------------------------------
# month
sighta = glm.nb(mf ~ month+
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


# combine all call types into single dataframe
cr_df = cbind(cru_df,crg_df,crm_df)

# save data table 
write.csv(cr_df,"data/processed/call_rate_regression_table.csv")


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
