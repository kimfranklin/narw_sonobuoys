# tbl_zero_infla_est_callrate.R #



library(tidyverse)
library(data.table)
library(MASS)
library(performance)


df = readRDS("data/processed/proc_acou_photoid.rds")



# estimating upcall call rate
# poisson
sightm = glm(up ~ month+
               num_sighting+ratio_male_female+
               social_bhv_whale+foraging_bhv_whale+
               offset(log(rec_duration))
             , data = df, family = 'poisson')
uppo = check_zeroinflation(sightm)

uppo = do.call(rbind.data.frame, uppo)
uppo=data.table::transpose(uppo)

# quasi poisson
sightm = glm(up ~ month+
               num_sighting+ratio_male_female+
               social_bhv_whale+foraging_bhv_whale+
               offset(log(rec_duration))
             , data = df, family = 'quasipoisson')
upqp = check_zeroinflation(sightm)

upqp = do.call(rbind.data.frame, upqp)
upqp=data.table::transpose(upqp)

# negative binomial
sightm = glm.nb(up ~ month+
                  num_sighting+ratio_male_female+
                  social_bhv_whale+foraging_bhv_whale+
                  offset(log(rec_duration))
                , data = df,control=glm.control(maxit=1000))
upnb = check_zeroinflation(sightm)

upnb = do.call(rbind.data.frame, upnb)
upnb=data.table::transpose(upnb)

# estimating gunshot call rate
# poisson
sightm = glm(gs ~ month+
               num_sighting+ratio_male_female+
               social_bhv_whale+foraging_bhv_whale+
               offset(log(rec_duration))
             , data = df, family = 'poisson')
gspo = check_zeroinflation(sightm)

gspo = do.call(rbind.data.frame, gspo)
gspo=data.table::transpose(gspo)

# quasi poisson
sightm = glm(gs ~ month+
               num_sighting+ratio_male_female+
               social_bhv_whale+foraging_bhv_whale+
               offset(log(rec_duration))
             , data = df, family = 'quasipoisson')
gsqp = check_zeroinflation(sightm)

gsqp = do.call(rbind.data.frame, gsqp)
gsqp=data.table::transpose(gsqp)

# negative binomial
sightm = glm.nb(gs ~ month+
                  num_sighting+ratio_male_female+
                  social_bhv_whale+foraging_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))
gsnb = check_zeroinflation(sightm)

gsnb = do.call(rbind.data.frame, gsnb)
gsnb=data.table::transpose(gsnb)

# estimating mid-freq call rate 
# poisson
sightm = glm(mf ~ month+
               num_sighting+ratio_male_female+
               social_bhv_whale+foraging_bhv_whale+
               offset(log(rec_duration))
             , data = df, family = 'poisson')
mfpo = check_zeroinflation(sightm)

mfpo = do.call(rbind.data.frame, mfpo)
mfpo=data.table::transpose(mfpo)

# quasi poisson
sightm = glm(mf ~ month+
               num_sighting+ratio_male_female+
               social_bhv_whale+foraging_bhv_whale+
               offset(log(rec_duration))
             , data = df, family = 'quasipoisson')
mfqp = check_zeroinflation(sightm)

mfqp = do.call(rbind.data.frame, mfqp)
mfqp=data.table::transpose(mfqp)

# negative binomial
sightm = glm.nb(mf ~ month+
                  num_sighting+ratio_male_female+
                  social_bhv_whale+foraging_bhv_whale+
                  offset(log(rec_duration))
                , data = df, control=glm.control(maxit=100))
mfnb = check_zeroinflation(sightm)

mfnb = do.call(rbind.data.frame, mfnb)
mfnb=data.table::transpose(mfnb)


# combine all rows into a dataframe
tmp_df <- rbind(uppo, upqp, upnb, gspo, gsqp, gsnb, mfpo, mfqp, mfnb)

# rename column names
tmp_df = tmp_df %>% 
  rename(
    zeros_model_est = V1,
    zeros_data_has = V2,
    ratio = V3,
    alpha = V4
  )

tmp_df$model = c('poisson up ~ month+ num_sighting+ratio_male_female+social_bhv_whale+foraging_bhv_whale+offset(log(rec_duration))',
                 'quasi poisson up ~ month+ num_sighting+ratio_male_female+social_bhv_whale+foraging_bhv_whale+offset(log(rec_duration))',
                 'neg bin up ~ month+ num_sighting+ratio_male_female+social_bhv_whale+foraging_bhv_whale+offset(log(rec_duration))',
                 'poisson gs ~ month+num_sighting+ratio_male_female+social_bhv_whale+foraging_bhv_whale+offset(log(rec_duration))',
                 'quasi poisson gs ~ month+num_sighting+ratio_male_female+social_bhv_whale+foraging_bhv_whale+offset(log(rec_duration))',
                 'neg bin gs ~ month+num_sighting+ratio_male_female+social_bhv_whale+foraging_bhv_whale+offset(log(rec_duration))',
                 'poisson mf ~ month+num_sighting+ratio_male_female+social_bhv_whale+foraging_bhv_whale+offset(log(rec_duration))',
                 'quasi poissonmf ~ month+num_sighting+ratio_male_female+social_bhv_whale+foraging_bhv_whale+offset(log(rec_duration))',
                 'neg bin mf ~ month+num_sighting+ratio_male_female+social_bhv_whale+foraging_bhv_whale+offset(log(rec_duration))')


# set dataframe to be a table 
setDT(tmp_df)

# save data table 
write.csv(tmp_df,"data/processed/zero_infla_est_callrate.csv")
 