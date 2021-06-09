## check_yday_hour_correlation ##
# check for pattern in day of year versus deployment time

library(tidyverse)

# read in data
df = readRDS('data/processed/proc_acou_photoid.rds')

# plot
ggplot(df)+
  geom_point(aes(x = yday, y = hour, fill = as.factor(year)), shape = 21)+
  scale_fill_viridis_d()+
  labs(x = 'Day of year', y = 'Hour of day', fill = 'Year')

# quick spearman's correlation test
cor.test(x = df$yday, y = df$hour, method = 'spearman')

# conclusion: there is a non-significant negative relationship between day of year and deployment hour