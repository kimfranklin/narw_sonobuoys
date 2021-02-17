## master ##
# conduct narw_sonobuoys analysis

# process -----------------------------------------------------------------

# 1. combine all selection tables and merge sonobuoy log with selection tables
# (produces `data/interim/all_noaa_selections.rds`and `data/processed/all_noaa_acoustic.rds`)
source('code/wrg_acoustic.R')

# 2. combine all photo-id data and subset photo-id data (produces `data/interim/all_noaa_photoid_comb.rds`
# and `data/processed/all_noaa_photoid.rds`)
source('code/wrg_photo_id.R')

# 3. combine acoustic and photo-id data (produces `data/processed/proc_acou_photoid.rds`)
source('code/wrg_comb_acou_photoid.R')

# basic info from data set ------------------------------------------------

# plot deployment map
source('code/proc_map_gsl.R')
source('code/fig_map_OFFICIALOFFICIAL.R')

# produce summary table 
source('code/tbl_5_num_summary.R')

# plot correlation matrix (this script includes the one in the app/supp)
source('code/fig_corr_matrix_manuscript.R')

# statistical analyses ----------------------------------------------------

# characterize whale abundance using single variable linear regressions
source('code/tbl_est_num_sighting.R')

# characterize calling rates using single variable neg. binomial regressions
source('code/tbl_est_call_rates')

# characterize calling rates using stepwise multivariate neg. binomial regression
source('code/tbl_est_call_rates_stepwise.R')

# appendix/supplementary --------------------------------------------------

# stepwise process for each call type
source('code/tbl_est_call_rates_stepwise_process.R')

# compare month and year for monthly/yearly variation in data
source('code/tbl_year_month_comparisons.R')

# checking assumptions for linear and neg. binomial regressions
source('code/tbl_normality_test.R')
source('code/tbl_homoscedasticity_est_num_whales.R')
source('code/tbl_overdis_est_callrate.R')
source('code/tbl_zero_infla_est_callrate.R')
source('code/tbl_vif_est_num_sight_est_callrate.R')
