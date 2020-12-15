## pca ##
# principle components analysis
# helpful for visualizing patterns (and diagnosing non-independence)

library(tidyverse)
library(ggfortify)

# read in processed data
all = readRDS('data/processed/proc_acou_photoid.rds') %>% as_tibble()

# select variables of interest
df = all %>%
  select(
    year,
    date,
    up_per_hr,
    gs_per_hr,
    mf_per_hr,
    social_bhv_whale,
    foraging_bhv_whale,
    num_sighting,
    ratio_male_female
  ) 

# check for call outliers
# df %>% pivot_longer(up_per_hr:mf_per_hr, names_to = 'call_type', values_to = 'calls_per_hr') %>%
#   ggplot()+
#   geom_histogram(aes(x=calls_per_hr), bins = 30, size=0.2, color = 'black', fill = 'grey')+
#   facet_wrap(.~call_type,scales = 'free', ncol = 1)+
#   theme_bw()

# remove outliers 
df = filter(df, up_per_hr < 60)
# df = filter(df, !(date %in% as.Date(c('2019-08-16','2019-08-26'))))

# select numeric variables for pca
dfs = select(df, -year,-date)

# principal components analysis
pca = prcomp(dfs, scale. = T)

# plot pca
autoplot(object = pca, loadings = TRUE, loadings.label = TRUE,data = df)+
  geom_point(aes(fill=as.factor(year)),size=3,shape=21)+
  scale_fill_viridis_d()+
  geom_text(aes(label=format(date,'%b%d')), nudge_y = 0.02,size = 2.5)+
  labs(fill = 'Year')+
  theme_bw()
