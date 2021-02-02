# tbl_normality_test.R #

# shapiro-wilk normality test for varaibles, est num sight models and all call 
# rate models 

# libraries ---------------------------------------------------------------

library(MASS)
library(tidyverse)
library(data.table)
# library(dgof)
# library(lmtest)
# library(tseries)
# library(AER)
# library(performance)
# library(car)

# input -------------------------------------------------------------------

df = readRDS("data/processed/proc_acou_photoid.rds")


# process -----------------------------------------------------------------
# individual --------------------------------------------------------------

# Are the variables normal shaped? Only num_sight is and adult males, everything else fails the test suggesting that everything else cannot be modeled by a normal distribution. (Alpha = 0.05)

# number of whales
nw = shapiro.test(df$num_sighting)
nw = do.call(rbind.data.frame, nw)
nw=data.table::transpose(nw)

# call rates
# up call
up = shapiro.test(df$up_per_hr)
up = do.call(rbind.data.frame, up)
up=data.table::transpose(up)

# gunshot
gs = shapiro.test(df$gs_per_hr)
gs = do.call(rbind.data.frame, gs)
gs=data.table::transpose(gs)

# mid-freq
mf = shapiro.test(df$mf_per_hr)
mf = do.call(rbind.data.frame, mf)
mf=data.table::transpose(mf)

# yday
mo = shapiro.test(df$yday)
mo = do.call(rbind.data.frame, mo)
mo=data.table::transpose(mo)


# demographics
# ratio
ra = shapiro.test(df$ratio_male_female)
ra = do.call(rbind.data.frame, ra)
ra=data.table::transpose(ra)

# adult females
af = shapiro.test(df$adult_female)
af = do.call(rbind.data.frame, af)
af=data.table::transpose(af)

# adult males
am = shapiro.test(df$adult_male)
am = do.call(rbind.data.frame, am)
am=data.table::transpose(am)

# juvenile females
jf = shapiro.test(df$juvenile_female)
jf = do.call(rbind.data.frame, jf)
jf=data.table::transpose(jf)

# juvenile males
jm = shapiro.test(df$juvenile_male)
jm = do.call(rbind.data.frame, jm)
jm=data.table::transpose(jm)

# unknown indiivduals
uk = shapiro.test(df$unknown)
uk = do.call(rbind.data.frame, uk)
uk=data.table::transpose(uk)

# behaviour rates
# foraging 
fr = shapiro.test(df$foraging_bhv_whale)
fr = do.call(rbind.data.frame, fr)
fr=data.table::transpose(fr)

# social 
sr = shapiro.test(df$social_bhv_whale)
sr = do.call(rbind.data.frame, sr)
sr=data.table::transpose(sr)

# other
or = shapiro.test(df$other_bhv_whale)
or = do.call(rbind.data.frame, or)
or=data.table::transpose(or)

# combine all rows into a dataframe
shap_df <- rbind(nw, up, gs, mf, mo, ra, af, am, jf, jm, uk, fr, sr, or)

# rename column names
shap_df = shap_df %>% 
  rename(
    test_stat_W = V1,
    p_val = V2,
    test_type = V3,
    test_variable = V4
  )

# set dataframe to be a table 
setDT(shap_df)

# save data table 
write.csv(shap_df,"data/processed/shap_normality_test_ind.csv")


# models ------------------------------------------------------------------

# number of whales and upcall rate
res = residuals(lm(num_sighting~up_per_hr, data = df))

nwup = shapiro.test(res)

nwup = do.call(rbind.data.frame, nwup)
nwup=data.table::transpose(nwup)


# number of whales and gunshot rate
res = residuals(lm(num_sighting~gs_per_hr, data = df))

nwgs = shapiro.test(res)

nwgs = do.call(rbind.data.frame, nwgs)
nwgs=data.table::transpose(nwgs)

# number of whales and mid-freq rate
res = residuals(lm(num_sighting~mf_per_hr, data = df))

nwmf = shapiro.test(res)

nwmf = do.call(rbind.data.frame, nwmf)
nwmf=data.table::transpose(nwmf)

# number of whales and yday
res = residuals(lm(num_sighting~yday, data = df))

nwmo = shapiro.test(res)

nwmo = do.call(rbind.data.frame, nwmo)
nwmo=data.table::transpose(nwmo)


# upcall rate vs yday
res = residuals(lm(up_per_hr~yday, data = df))

upmo = shapiro.test(res)

upmo = do.call(rbind.data.frame, upmo)
upmo=data.table::transpose(upmo)

# upcall rate vs ratio
res = residuals(lm(up_per_hr~ratio_male_female, data = df))

upra = shapiro.test(res)

upra = do.call(rbind.data.frame, upra)
upra=data.table::transpose(upra)

# upcall rate vs sighting
res = residuals(lm(up_per_hr~num_sighting, data = df))

upnw = shapiro.test(res)

upnw = do.call(rbind.data.frame, upnw)
upnw=data.table::transpose(upnw)

# upcall rate vs foragign rate
res = residuals(lm(up_per_hr~foraging_bhv_whale, data = df))

upfr = shapiro.test(res)

upfr = do.call(rbind.data.frame, upfr)
upfr=data.table::transpose(upfr)

# upcall rate vs social
res = residuals(lm(up_per_hr~social_bhv_whale, data = df))

upsr = shapiro.test(res)

upsr = do.call(rbind.data.frame, upsr)
upsr=data.table::transpose(upsr)


# gunshot rate vs yday
res = residuals(lm(gs_per_hr~yday, data = df))

gsmo = shapiro.test(res)

gsmo = do.call(rbind.data.frame, gsmo)
gsmo=data.table::transpose(gsmo)

# gunshot rate vs ratio
res = residuals(lm(gs_per_hr~ratio_male_female, data = df))

gsra = shapiro.test(res)

gsra = do.call(rbind.data.frame, gsra)
gsra=data.table::transpose(gsra)

# gunshot rate vs sighting
res = residuals(lm(gs_per_hr~num_sighting, data = df))

gsnw = shapiro.test(res)

gsnw = do.call(rbind.data.frame, gsnw)
gsnw=data.table::transpose(gsnw)

# gunshot rate vs foragign rate
res = residuals(lm(gs_per_hr~foraging_bhv_whale, data = df))

gsfr = shapiro.test(res)

gsfr = do.call(rbind.data.frame, gsfr)
gsfr=data.table::transpose(gsfr)

# gunshot rate vs social
res = residuals(lm(gs_per_hr~social_bhv_whale, data = df))

gssr = shapiro.test(res)

gssr = do.call(rbind.data.frame, gssr)
gssr=data.table::transpose(gssr)


# mf rate vs yday
res = residuals(lm(mf_per_hr~yday, data = df))

mfmo = shapiro.test(res)

mfmo = do.call(rbind.data.frame, mfmo)
mfmo=data.table::transpose(mfmo)

# mf rate vs ratio
res = residuals(lm(mf_per_hr~ratio_male_female, data = df))

mfra = shapiro.test(res)

mfra = do.call(rbind.data.frame, mfra)
mfra=data.table::transpose(mfra)

# mf rate vs sighting
res = residuals(lm(mf_per_hr~num_sighting, data = df))

mfnw = shapiro.test(res)

mfnw = do.call(rbind.data.frame, mfnw)
mfnw=data.table::transpose(mfnw)

# mf rate vs foragign rate
res = residuals(lm(mf_per_hr~foraging_bhv_whale, data = df))

mffr = shapiro.test(res)

mffr = do.call(rbind.data.frame, mffr)
mffr=data.table::transpose(mffr)

# mf rate vs social
res = residuals(lm(mf_per_hr~social_bhv_whale, data = df))

mfsr = shapiro.test(res)

mfsr = do.call(rbind.data.frame, mfsr)
mfsr=data.table::transpose(mfsr)



# combine all rows into a dataframe
shap_df <- rbind(nwup, nwgs, nwmf, nwmo,
               upmo, upra, upnw, upfr, upsr,
               gsmo, gsra, gsnw, gsfr, gssr,
               mfmo, mfra, mfnw, mffr, mfsr)

# rename column names
shap_df = shap_df %>% 
  rename(
    test_stat_W = V1,
    p_val = V2,
    test_type = V3,
    test_variable = V4
  )

shap_df$model = c('num_sighting~up_per_hr',
                 'num_sighting~gs_per_hr',
                 'num_sighting~mf_per_hr',
                 'num_sighting~yday',
                 'up_per_hr~yday',
                 'up_per_hr~ratio_male_female',
                 'up_per_hr~num_sighting',
                 'up_per_hr~foraging_bhv_whale',
                 'up_per_hr~social_bhv_whale',
                 'gs_per_hr~yday',
                 'gs_per_hr~ratio_male_female',
                 'gs_per_hr~num_sighting',
                 'gs_per_hr~foraging_bhv_whale',
                 'gs_per_hr~social_bhv_whale',
                 'mf_per_hr~yday',
                 'mf_per_hr~ratio_male_female',
                 'mf_per_hr~num_sighting',
                 'mf_per_hr~foraging_bhv_whale',
                 'mf_per_hr~social_bhv_whale')


# set dataframe to be a table 
setDT(shap_df)

# save data table 
write.csv(shap_df,"data/processed/shap_normality_test_model.csv")
