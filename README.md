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

For the current (2021) manuscript being made only aerial surveys from 2017-2019 are being used, the vessel data is not!

NOTE: 
- All scripts in 'code' folder are necessary to reproduce the data in the manuscript along with its figures and tables. All scripts in 'tests' were used for testing data assumptions, models, and time/space ranges, these scripts are not necessary to run.
- wrg = wrangling scripts, these produce data and saved as csv
- fig = figure producing scripts, these produce figures and the figures are saved
as either png or jpeg
- tbl = table producing scripts, these produce tables and saved as csv
- terminology discrepancies used in code and used in manuscript:
  - num_sighting = whale count = the number of whales sighted/counted with in the time and     space range
  - up = upcall count; gs = gunshot count; mf = tonal count
  - dep_duration = includes hour before recording, hour after recording and time length of     recording (includes weak signals) (not mentioned in manuscript)
  - rec_duration = duration = duration of recording, weak signals are removed
  - foraging_bhv_whale (same for social and other) = foraging behaviour rate
  - ratio_male_female = male/female ratio = all males/all females

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
From here on out these csv files produced will be data will be used in the rest of the scripts

## maps
1. fig_map_official_for_ms.R
  - needs proc_map_data.R to run properly (in code folder)
  - generic deployment map has all locations for all years and month that NOAA deployed a sonobuoy (used in MS)
2. fig_target_plot_photoid.R
  - needs functions.R to run properly (in tests folder)
  - this produces a suite of time and space range maps, it was used to figure out what time and space range is best for figuring out what whales were inlcuded or not with different ranges
3. fig_track_maps.R (in test folder)
  - sightings data maps with track lines
  - needs map_data.rda (in data/processed folder), whale map track lines, processed photo id
  - theres a version of this script floating around (probably work folder) that uses processed sightings data which is a different script not listed here (only photo id data is processed as described in this readme file)
4. fig_sono_map_facet_month_yr.R (in test folder)
  - needs map_data.rda (in data/processed folder)
  - faceted map by year and month to see deployment trends in each month for each year
5. tests/space_map_variables.R (in test folder)
  - needs map_data.rda (in data/processed folder)
  - faceted map by year and month for each behaviour and call rate to see acoustic and behaviour trends in each month for each year

## testing the data/the markdown files
The R markdown files were created to help see patterns and decide what statistical analyses we wanted to do on the data. The are a stepping stone before creating the tbl and fig scripts. See master script for main scripts used in this process. This process also inlcudes all the map scripts not inlcuding the first map script listed above (fig_map_official_for_ms.R)

## make figures to visualize the data
1. fig_corr_matrix_manuscript.R
  - this produces two spearman correlation matrices, a short one and full one (the short one has only few variables where as the full one has nearly all the variables in the dataset)
2. fig_scatterplots_new.R
  - this produces two scatter plots in one image (call rates in one plot and behaviour rates in the other plot) and another image of a bargraph that is grouped by demographic class with each total bar being the whale count

## statistical analysis
1. tbl_5_num_summary.R
  - this script produces a table of all variables' 1st and 3rd quartile, mean, median, max and min to give idea of variables' distribution
2. tbl_est_num_sighting.R
  - this script produces all the models where num_sighting (or whale count) is the y variable, the estimates are produced and saved in one csv file and the anova tables are saved as another (separate) csv file
3. tbl_est_call_rates.R
  - this script produces all the models where call rates (upcall, gunshot, tonal) are the y variable, the estimates are produced and saved in one csv file and the negative binomial likelihood ratio test tables are saved as another (separate) csv file (the negative binomial likelihood ratio test compare the model to a null which is just the intercept and offset)
4. tbl_est_call_rates_stepwise.R
  - this script produces all the call rate stepwise models where the call rates (upcall, gunshot, tonal) are the y variable, the estimates are produced and saved in one csv file and the negative binomial likelihood ratio test tables are saved as another (separate) csv file (the negative binomial likelihood ratio test compare the model to a null which is just the intercept and offset)

## extra stat analyses/justification for stat anaylses
THESE PRODUCE TABLES 
1. tbl_est_call_rates_stepwise_process.R
  - this is the backwards stepwise process for all the 3 call type full models
2. tbl_year_month_comparisons.R
  - this is KW test for comparing variables by year and month to see if there is variation in month and year
3. tbl_normality_test.R
  - test normality of single variables and single vairable models
4. tbl_homoscedasticity_est_num_whales.R
  - this is to test whale count models because whale count is normally distributed
5. tbl_overdis_est_callrate.R
  - testing overdispersion in the call rate models
6. tbl_zero_infla_est_callrate.R
  - this compares the zeros in the data to the zeros the models estimate, from there we can decide if the model is appropriate or not 
7. tbl_vif_est_num_sight_est_callrate.R
  - obtaining variance inflation factors (VIF) for all full models


