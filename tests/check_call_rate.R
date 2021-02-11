## check_call_rate ##
# quick check for change in call rate within deployment

library(tidyverse)

# read and format call data
df = readRDS('data/processed/all_noaa_acoustic.rds') %>%
  filter(score == 1 & call_type %in% c('mf', 'gs', 'up')) %>%
  transmute(
    id,
    time = Begin.Time..s./60/60,
    call_type,
    yday
  ) %>%
  arrange(time) %>%
  as_tibble() 

# calculate cumulative call counts by deployment
cbd = df %>%
  group_by(id) %>%
  summarize(
    time,
    call_n = seq(from = 1, to = length(call_type), by = 1),
    yday,
    .groups = 'drop'
  )

# plot
ggplot()+
  geom_path(data = cbd, aes(x = time, y = call_n, group = id, color = yday), size = 1) +
  scale_color_viridis_c() +
  labs(x = 'Time (h)', y = 'Cumulative call count', color = 'Day of year')+
  theme_bw()

# assign calls to first or second half of each deployment
fvs = df %>%
  group_by(id) %>%
  summarize(
    grp = ifelse(time <= max(time,na.rm = T)/2, yes = 'First half', no = 'Second half'),
    .groups = 'drop'
  ) %>%
  group_by(id,grp) %>%
  count()

# plot
ggplot()+
  geom_boxplot(data = fvs, aes(x = grp, y = n), fill = 'grey') +
  labs(x = 'Call time within deployment', y = 'Call count') +
  theme_bw()
