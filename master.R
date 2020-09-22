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

# plot --------------------------------------------------------------------

# plot deployment map
source('code/fig_map.R')

# plot histogram...
# source('code/f_histogram.R')
