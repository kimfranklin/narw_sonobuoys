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


# test markdowns/scripts --------------------------------------------------
# this section is not necessary to run unless a bunch of new data is added

# these markdown files are for seeing what kind of data we have and how we want 
# to represent it/show the trends, some of the Rmd files are testing models
# assumptions to see if the models are valid
source('tests/spearman_matrix_pval_matrix.Rmd')
source('tests/glm_testing.Rmd')
source('tests/est_call_rate_stepwise.Rmd')
source('tests/overdis_0inf.Rmd')
source('tests/year_comparison.Rmd')
source('tests/est_num_whales_models.Rmd')
source('tests/est_call_rate_models.Rmd')
source('tests/negbin_vs_zinb_vs_zip.Rmd')

# these scripts are sanity checks/double checking what we are doing is valid or can be done
source('tests/pca.R')
source('tests/check_call_rate.R')
source('tests/logistic_models_plots.R')

# these are testing the time and space distributions
source('tests/fig_target_plot_photoid.R')
source('tests/fig_track_maps.R') # this might not work because it uses processed 
# sightings data which is a different script not listed here (only photo id data 
# is processed in this master script)

# these maps are faceted by year and month to see in depth trends
source('tests/fig_sono_map_facet_month_yr.R')
source('tests/space_dist_of_variables_17-19.Rmd')
source('tests/space_map_variables.R')

# basic info from data set ------------------------------------------------

# plot deployment map (Figure 1)
source('code/proc_map_data.R')
source('code/fig_map_official_for_ms.R')

# produce summary table (Table 2 and Table S2)
source('code/tbl_5_num_summary.R')

# plot correlation matrix (Figure 2; this script includes the one in the app/supp which its Figure S1)
source('code/fig_corr_matrix_manuscript.R')

# plot scatter plots (Figure 3)
source('code/fig_scatterplots_new.R')

# statistical analyses ----------------------------------------------------

# characterize whale count using single variable linear regressions (Table S10)
source('code/tbl_est_num_sighting.R')

# characterize calling rates using single variable neg. binomial regressions (Table S11)
source('code/tbl_est_call_rates.R')

# characterize calling rates using stepwise multivariate neg. binomial regression (Table S13)
source('code/tbl_est_call_rates_stepwise.R')

# all models produced above in a single table (note this table is half formatted, 
# rest of formatting done in excel) (Table 3)
source('code/tbl_all_manuscript_models_combined.R')

# appendix/supplementary --------------------------------------------------

# stepwise process for each call type (Table S12)
source('code/tbl_est_call_rates_stepwise_process.R')

# compare month and year for monthly/yearly variation in data (Table S3)
source('code/tbl_year_month_comparisons.R')

# checking assumptions for linear and neg. binomial regressions
source('code/tbl_normality_test.R') # Table S5 and Table S6
source('code/tbl_homoscedasticity_est_num_whales.R') # Table S7
source('code/tbl_overdis_est_callrate.R') # Table S8
source('code/tbl_zero_infla_est_callrate.R') # Table S9
source('code/tbl_vif_est_num_sight_est_callrate.R') # Table S4
