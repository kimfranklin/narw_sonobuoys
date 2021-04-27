# 5_num_summary.R #

# 5 number summary for each variable of interest in the dataframe 
# includes: max, min, median, first quartiile, third quartile

# libraries
library(tidyverse)
library(data.table)

# read in the data
df= readRDS("data/processed/proc_acou_photoid.rds")

# subset data so only variables of interest are shown
tmp = df %>%
   dplyr::select(up,up_per_hr,up_per_hr_per_whale,
          gs,gs_per_hr,gs_per_hr_per_whale,
          mf,mf_per_hr,mf_per_hr_per_whale,
          num_sighting, ratio_male_female,
          adult_male,adult_female,juvenile_male,juvenile_female,calf_male,calf_female,unknown,
          foraging_bhv_whale,social_bhv_whale,other_bhv_whale,
          rec_duration_h)

# make matrix/array of 5 number summary stats of variables of interest
tmps = do.call(cbind, lapply(tmp, summary))

# transpose that matrix/array
tmpss = t(tmps)

# add coefficient of variance term to table
cv = c((sd(df$up)/mean(df$up)*100),
        (sd(df$up_per_hr)/mean(df$up_per_hr)*100),
        (sd(df$up_per_hr_per_whale)/mean(df$up_per_hr_per_whale)*100),
        (sd(df$gs)/mean(df$gs)*100),
        (sd(df$gs_per_hr)/mean(df$gs_per_hr)*100),
        (sd(df$gs_per_hr_per_whale)/mean(df$gs_per_hr_per_whale)*100),
        (sd(df$mf)/mean(df$mf)*100),
        (sd(df$mf_per_hr)/mean(df$mf_per_hr)*100),
        (sd(df$mf_per_hr_per_whale)/mean(df$mf_per_hr_per_whale)*100),
        (sd(df$num_sighting)/mean(df$num_sighting)*100),
        (sd(df$ratio_male_female)/mean(df$ratio_male_female)*100),
        (sd(df$adult_male)/mean(df$adult_male)*100),
        (sd(df$adult_female)/mean(df$adult_female)*100),
        (sd(df$juvenile_male)/mean(df$juvenile_male)*100),
        (sd(df$juvenile_female)/mean(df$juvenile_female)*100),
       (sd(df$calf_male)/mean(df$calf_male)*100),
       (sd(df$calf_female)/mean(df$calf_female)*100),
        (sd(df$unknown)/mean(df$unknown)*100),
        (sd(df$foraging_bhv_whale)/mean(df$foraging_bhv_whale)*100),
        (sd(df$social_bhv_whale)/mean(df$social_bhv_whale)*100),
        (sd(df$other_bhv_whale)/mean(df$other_bhv_whale)*100),
        (sd(df$rec_duration_h)/mean(df$rec_duration_h)*100))

# round
cv = round(cv,2)
tmpss = round(tmpss,2)

# combine cv with summary table
tmpss = cbind(tmpss,cv)

# save the matrix/array
write.csv(tmpss,"data/processed/5_num_summary.csv")
