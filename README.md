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