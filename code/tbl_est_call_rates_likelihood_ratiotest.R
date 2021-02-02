## tbl_est_call_rates_likelihood_ratiotest.R ##

# rather than reporting p-values from summary table (see tbl_est_call_rates.R)
# MASS fxn anova is used to get p-values

# libraries ---------------------------------------------------------------

library(tidyverse)
library(MASS)
library(plyr)

# input -------------------------------------------------------------------

# read in data
df = readRDS("data/processed/proc_acou_photoid.rds")

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


# combine all call types into single dataframe
cr_df = cbind(cru_df,crg_df,crm_df)

# save data table 
write.csv(cr_df,"data/processed/call_rate_likelihood_table.csv")
