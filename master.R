## master ##
# conduct narw_sonobuoys analysis

# process -----------------------------------------------------------------

# 1. combine all selection tables (produces `data/interim/all_noaa_selections.rds`)
source('code/wrg_proc_comb_allselection_tables.R')

# 2. merge sonobuoy log with selection tables (produces `data/processed/all_noaa_acoustic.rds`)
source('code/wrg_proc_comb_log_selection.R')

# 3. combine all photo-id data (produces `data/interim/all_noaa_photoid_comb.rds`)
source('code/wrg_comb_photoid.R')

# 4. subset photo-id data (produces `data/processed/all_noaa_photoid.rds`)
source('code/wrg_photoid.R')

# 5. combine acoustic and photo-id data (produces `data/processed/proc_acou_photoid.rds`)
source('code/wrg_comb_acou_photoid.R')

# plot --------------------------------------------------------------------

# plot histogram...
# source('code/f_histogram.R')
