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
  - it is wrg_proc_comb_allselection_tables.R combined with 
  wrg_proc_comb_log_selection.R
    - it produces 'data/interim/all_noaa_selections.rds', 
    'data/processed/log.rds' and,
    'data/processed/all_noaa_acoustic.rds'
2. wrg_photo_id.R 
  - it is wrg_comb_photoid.R combined with wrg_photoid.R
    - it produces 'data/interim/all_noaa_photoid_comb.rds' and 
    'data/processed/all_noaa_photoid.rds'
3. wrg_comb_acou_photoid.R
  - produces 'data/processed/proc_acou_photoid.rds'

Two data files in interim and four data files in processed should be produced