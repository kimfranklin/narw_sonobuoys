# tbl_homoscedasticity_est_num_whales.R #


library(tidyverse)
library(data.table)
library(lmtest)


df = readRDS("data/processed/proc_acou_photoid.rds")

# upcall rate
up = bptest(df$num_sighting~df$up_per_hr)
up = do.call(rbind.data.frame, up)
up=data.table::transpose(up)

# gs rate
gs = bptest(df$num_sighting~df$gs_per_hr)
gs = do.call(rbind.data.frame, gs)
gs=data.table::transpose(gs)

# mf rate
mf = bptest(df$num_sighting~df$mf_per_hr)
mf = do.call(rbind.data.frame, mf)
mf=data.table::transpose(mf)

# month
mo = bptest(df$num_sighting~df$month)
mo = do.call(rbind.data.frame, mo)
mo=data.table::transpose(mo)


# combine all rows into a dataframe
tmp_df <- rbind(up, gs, mf, mo)

# rename column names
tmp_df = tmp_df %>% 
  rename(
    test_stat_bp = V1,
    deg_freedom = V2,
    test_type = V3,
    p_val= V4,
    model = V5
  )

# set dataframe to be a table 
setDT(tmp_df)

# save data table 
write.csv(tmp_df,"data/processed/homoscedasticity_test_num_whales.csv")