## check_call_rate ##
# quick check for change in call rate within deployment

library(tidyverse)
library(patchwork)
library(data.table)

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
p = ggplot()+
  geom_path(data = cbd, aes(x = time, y = call_n, group = id, color = yday), size = 1) +
  scale_color_viridis_c() +
  labs(x = 'Time (h)', y = 'Cumulative call count', color = 'Day of year')+
  theme_bw()+ 
  theme(text = element_text(size = 15, family = "serif"))

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
g = ggplot()+
  geom_boxplot(data = fvs, aes(x = grp, y = n), fill = 'grey') +
  labs(x = 'Deployment time available for calling', y = 'Call count') +
  theme_bw()+ 
  theme(text = element_text(size = 15, family = "serif"))

# SAVE
# output file 
ofile = 'calls_firsthalf_vs_secondhalf.png'

# figure directory
fig_dir = 'figures/'

stacked = wrap_plots(p,g, ncol = 1)+
  plot_annotation(tag_levels = 'a', tag_suffix = ')')

# save
ggsave(filename = paste0(fig_dir, ofile), plot = stacked,
       height = 9, width = 7, dpi = 300)



# rank sum test to compare first half to second half 
a = kruskal.test(n~grp, data = fvs)
a = do.call(rbind.data.frame, a)
a=data.table::transpose(a)

# round to 3 decimal places
a$V1 = round(as.numeric(a$V1), 3)
a$V3 = round(as.numeric(a$V3), 3)

# rename column names
a = a %>% 
  dplyr::rename(
    test_stat = V1,
    degrees_freedom = V2,
    p_val = V3,
    test_type = V4,
    variables_comp = V5
  )

# save rank test results 
# set dataframe to be a table 
setDT(a)

# save data table 
write.csv(a,"data/processed/tbl_calls_firsthalf_vs_secondhalf.csv")
