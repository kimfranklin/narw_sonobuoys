# narw_sonobuoys
this is a readme file

## overview
North Atlantic right whale sonobuoy project. This project combines visual and 
acoustic data collected from aerial/vessel surveys and sonobuoys, respectively. 
The data were collected simultaneously which provides a unique perspective to 
see and hear what the whales are doing at the same time. This project will be 
able to associate behaviours with call types with hopes that collecting acoustic 
data will be sufficient in the future to derive right whale behaviours with out 
visual aid. 

NOTE: 
- wrg = wrangling scripts, these produce data and saved as csv
- fig = figure producing scripts, these produce figures and the figures are saved
as either png or jpeg
- tbl = table producing scripts, these produce tables and saved as csv

## wrangling scripts order
run the scripts in order staring with #1 to #3 
1. wrg_acoustic.R
  - the first half of the script combines all the selection tables for each
  deployment (was script wrg_proc_comb_allselection_tables.R)
    - it produces 'data/interim/all_noaa_selections.rds' (saved in interim)
  - the second half of of the script adds log information to the collection of
  selection tables (was script wrg_proc_comb_log_selection.R); it also calculates 
  the deployment durations and saves the log information as its own rds
    - it produces 'data/processed/log.rds' (saved in processed) and
    'data/processed/all_noaa_acoustic.rds' (saved in processed)
2. wrg_photo_id.R 
  - the first half of the script combines all photo-id data from NOAA and NARWC
  (was script wrg_comb_photoid.R)
    - it produces 'data/interim/all_noaa_photoid_comb.rds' (saved in interim)
  - the second half of the script subsets the data in time and space (was script 
  wrg_photoid.R)
    - it produces 'data/processed/all_noaa_photoid.rds' (saved in processed)
3. wrg_comb_acou_photoid.R
  - combines 'data/processed/all_noaa_acoustic.rds' and 
  'data/processed/all_noaa_photoid.rds'; it combines counts all the data (i.e., calls,
  behaviours, demographics) and has some rate calculations 
    - it produces 'data/processed/proc_acou_photoid.rds' (saved in processed)

Two data files in interim and four data files in processed should be produced

## maps
- generic deployment map has all locations for all years and month that NOAA deployed a sonobuoy (used in MS)
- time and space range maps
- sightings data maps with track lines
- faceted map by year and month
- faceted map by year and month for each behaviour and call rate

## working data/the markdown files
The R markdown files were created to help see patterns and decide what statistical analyses we wanted to do on the data. The are a stepping stone before creating the tbl and fig scripts

## make figures to visualize the data
1. fig_corr_matrix_manuscript.R
  - this produces two spearman correlation matrices, a short one and full one
  (the short one has only few variables where as the full one has nearly all the 
  variables in the dataset)
2. fig_scatterplots.R
  - this produces two scatter plots in one image (call rates in one plot and 
  behaviour rates in the other plot) and another image of a bargraph that is 
  grouped by demographic class with each total bar being the whale abundance

## statistical analysis
1. tbl_5_num_summary.R
- this script produces a table of all variables' 1st and 3rd quartile, mean, 
median, max and min to give idea of variables' distribution
2. tbl_est_num_sighting.R
- this script produces all the models where num_sighting (or whale abundance)
is the y variable, the estimates are produced and saved in one csv file and the 
anova tables are saved as another (separate) csv file
3. tbl_est_call_rates.R
- this script produces all the models where call rates (upcall, gunshot, tonal)
are the y variable, the estimates are produced and saved in one csv file and the 
negative binomial likelihood ratio test tables are saved as another (separate) csv file
(the negative binomial likelihood ratio test compare the model to a null which is just the 
intercept and offset)
4. tbl_est_call_rates_stepwise.R
- this script produces all the call rate stepwise models where the call rates
(upcall, gunshot, tonal) are the y variable, the estimates are produced and 
saved in one csv file and the negative binomial likelihood ratio test tables are
saved as another (separate) csv file (the negative binomial likelihood ratio test 
compare the model to a null which is just the intercept and offset)

## extra stat analyses/justification for stat anaylses
1. tbl_est_call_rates_stepwise_process.R
2. tbl_year_month_comparisons.R
3. tbl_normality_test.R
4. tbl_homoscedasticity_est_num_whales.R
5. tbl_overdis_est_callrate.R
6. tbl_vif_est_num_sight_est_callrate.R


