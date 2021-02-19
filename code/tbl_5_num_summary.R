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
          adult_male,adult_female,juvenile_male,juvenile_female,unknown,
          foraging_bhv_whale,social_bhv_whale,other_bhv_whale,
          rec_duration,dep_duration)

# make matrix/array of 5 number summary stats of variables of interest
tmps = do.call(cbind, lapply(tmp, summary))

# transpose that matrix/array
tmpss = t(tmps)

# round
tmpss = round(tmpss,3)

# save the matrix/array
write.csv(tmpss,"data/processed/5_num_summary.csv")
