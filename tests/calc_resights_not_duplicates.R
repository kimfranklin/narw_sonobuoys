
# libraries ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(stringr)
library(oce)
library(lubridate)

# input -------------------------------------------------------------------

# read in processed photo-id data file
id_df = readRDS("data/processed/all_noaa_photoid.rds")


# process -----------------------------------------------------------------

# remove duplicate whale sightings (remove the rows with true in dup column)
id_df = id_df[!(id_df$dup=='TRUE'),]

# check to see how many whales are duplicates (143)
unique(id_df$EGNO)

# identify duplicates
id_df$dup = duplicated(id_df$EGNO)

# count duplicates per EGNO NOTE THERE IS AN NA WHALE THE REPEAT DOES NOT COUNT 
# THIS WHALE, WE REMOVE THIS WHALE IN PROCESSING SEE WRG_COMB_ACOU_PHOTOID.R
df_repeat = data.frame(table(id_df$EGNO))

# add column identifying seeing a whale more than once
df_repeat[,"greater_than _1"] = df_repeat$Freq>1

# see how many indivudal whales were resighted in the whole study
table(df_repeat$`greater_than _1`)
