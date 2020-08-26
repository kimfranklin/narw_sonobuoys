##test_spliting_rows##
# trying to split behaviours intow own rows

# read data
df = readRDS("data/processed/wrg_all_noaa_photoid.rds")

# libraries
library(tidyverse)
library(stringr)
library(lubridate)
library(tidyr)
library(oce)
library(ggplot2)
library(splitstackshape)
library(dplyr)

# process

# trying to separate behaviours so each behaviour has its own row
#attempt 1
tmp = separate_rows(df, behaviour, sep = ", ")

#attempt 2
tmp = df %>%
  separate_rows(behaviour, sep = ", ")

#attempt 3- this one did yield more rows but now i can't get it to work even though i didnt change anything
tmp =
  df %>%
  mutate(unpacked = str_split(behaviour, ", ")) %>%
  unnest %>%
  mutate(behaviour = str_trim(unpacked))

#attempt 4
tmp = separate_rows(df, behaviour, sep = ",\\s+")

unique(tmp$behaviour)
unique(df$behaviour)

# replace tmp with df
df = tmp