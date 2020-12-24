# tbl_normality_test.R #


library(MASS)
library(tidyverse)
library(data.table)
# library(dgof)
# library(lmtest)
# library(tseries)
# library(AER)
# library(performance)
# library(car)

df = readRDS("data/processed/proc_acou_photoid.rds")




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

# month
mo = shapiro.test(df$month)
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


# models!
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

# number of whales and month
res = residuals(lm(num_sighting~month, data = df))

nwmo = shapiro.test(res)

nwmo = do.call(rbind.data.frame, nwmo)
nwmo=data.table::transpose(nwmo)


# upcall rate vs month
res = residuals(lm(up_per_hr~month, data = df))

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


# combine all rows into a dataframe
shap_df <- rbind(nwup, nwgs, nwmf, nwmo,
               upmo, upra, upnw, upfr, upsr)

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
                 'num_sighting~month',
                 'up_per_hr~month',
                 'up_per_hr~ratio_male_female',
                 'up_per_hr~num_sighting',
                 'up_per_hr~foraging_bhv_whale',
                 'up_per_hr~social_bhv_whale')


# set dataframe to be a table 
setDT(shap_df)

# save data table 
write.csv(shap_df,"data/processed/shap_normality_test_model.csv")



