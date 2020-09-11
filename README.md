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
run the scripts in order astaring with #1 to #5
1. wrg_proc_comb_allselection_tables.R
  - produces 'data/interim/all_noaa_selections.rds'
2. wrg_proc_comb_log_selection.R
  - produces 'data/processed/all_noaa_acoustic.rds'
3. wrg_comb_photoid.R
  - produces 'data/interim/all_noaa_photoid_comb.rds'
4. wrg_photoid.R
  - produces 'data/processed/all_noaa_photoid.rds'
5. wrg_comb_acou_photoid.R
  - produces 'data/processed/proc_acou_photoid.rds'

Two data files in interim and three data files in processed should be produced