# descriptive_stats

# read in the data
ifile = readRDS("data/processed/sight_dfs.rds")
df= ifile

library(tidyr)
library(tidyverse)


# acoustic stats ----------------------------------------------------------
#average deployment duration  
dur_stats = summary(df$duration) 
dur_stats
dur_stdev = sd(df$duration)
dur_stdev

#number of calls total
total_calls_stats = summary(df$sum_calls)
total_calls_stats
total_calls_stdev = sd(df$sum_calls)
total_calls_stdev

#number of calls by category total 
gs_stats = summary(df$num_gs)
gs_stats 
gs_stdev = sd(df$num_gs)
gs_stdev 

mf_stats = summary(df$num_mf)
mf_stats 
mf_stdev = sd(df$num_mf)
mf_stdev

up_stats = summary(df$num_up)
up_stats
up_stdev = sd(df$num_up)
up_stdev


# visual data -------------------------------------------------------------
#number of whales sighted total
sight_stats = summary(df$num_sighting)
sight_stats
sight_stdev = sd(df$num_sighting)
sight_stdev

#number of whales by demographics total
#adult vs juv
df2=df%>%
  mutate(sum_adult=adult_female+adult_male)
adult_stats = summary(df2$sum_adult)
adult_stats
adult_stdev = sd(df2$sum_adult)
adult_stdev

juv_stats = summary(df$sum_juvenile)
juv_stats
juv_stdev = sd(df$sum_juvenile)
juv_stdev

#male vs female
male_stats = summary(df$sum_male)
male_stats
male_stdev = sd(df$sum_male)
male_stdev

female_stats = summary(df$sum_female)
female_stats
female_stdev = sd(df$sum_female)
female_stdev

#adult male, juv male, adult female, juv female
juv_male_stats = summary(df$juvenile_male)
juv_male_stats
juv_male_stdev = sd(df$juvenile_male)
juv_male_stdev

juv_female_stats = summary(df$juvenile_female)
juv_female_stats
juv_female_stdev = sd(df$juvenile_female)
juv_female_stdev

adult_male_stats = summary(df$adult_male)
adult_male_stats
adult_male_stdev = sd(df$adult_male)
adult_male_stdev

adult_female_stats = summary(df$adult_female)
adult_female_stats
adult_female_stdev = sd(df$adult_female)
adult_female_stdev

#number of whales by behaviour total
#feed
feed_stats = summary(df$feed)
feed_stats
feed_stdev = sd(df$feed)
feed_stdev

#social behaviour
social_stats = summary(df$social)
social_stats
social_stdev = sd(df$social)
social_stdev

#other behaviour
other_behv_stats = summary(df$other_behv)
other_behv_stats
other_behv_stdev = sd(df$other_behv)
other_behv_stdev
