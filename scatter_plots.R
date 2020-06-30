## scatter_plots
# scatterplots for acoustic and visual data, note that the sight_dfs.rds file 
# was made in this script, if something doesn't work see sight_dfs.R script,
# this script was cut out right here, paste here to hopefully fix.

# open file for scatter plots 
sight_dfs = readRDS('data/processed/sight_dfs.rds')
library(ggplot2)
library(ggpubr)
library(viridis)

ggsave(plt, filename = paste0('figures/scatter_gs2.png'), 
       height = 5, width = 7, units = 'in', dpi = 300) 

# scatter plot for up calls per hour vs sightings
plt = ggplot(sight_dfs)+
  geom_point(aes(x = up_dur_hr, y = num_sighting, col=year), size = 3)+
  scale_color_manual(values=c("black", "red"))+
  labs(col = 'Year', x = 'Calls per Hour', y = 'Number of NARW Sighted')+
  theme(text = element_text(size=15))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))
plt

# scatter plot for mid-freq calls per hour vs sightings
plt = ggplot(sight_dfs)+
  geom_point(aes(x = mf_dur_hr, y = num_sighting, col = year), size = 3)+
  scale_color_manual(values=c("black", "red"))+
  labs(col = 'Year', x = 'Calls per Hour', y = 'Number of NARW Sighted')+
  theme(text = element_text(size=15))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))
plt

# scatter plot for gunshot calls per hour vs sightings
plt = ggplot(sight_dfs)+
  geom_point(aes(x = gs_dur_hr, y = num_sighting, col = year), size = 3)+
  scale_color_manual(values=c("black", "red"))+
  labs(col = 'Year', x = 'Calls per Hour', y = 'Number of NARW Sighted')+
  theme(text = element_text(size=15))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))
plt



# trying to do a scater plot with stats
scat = ggscatter(sight_dfs, x = "up_dur_hr", y = "num_sighting",
                 color = "year", shape = 16, size = 3, # Points color, shape and size
                 add = "reg.line",  # Add regressin line
                 add.params = list(color = "black",  fill = "lightgray"), # Customize reg. line
                 conf.int = FALSE, # Add confidence interval
                 cor.coef = TRUE, # Add correlation coefficient. see ?stat_cor
                 cor.coeff.args = list(method = "pearson", label.x = 3, label.sep = "\n")
)
scat

# extra codes 
# to keep in mindo n how to filter
tmp = df %>%
  filter(call_type == 'up')
unique(df$id)

# getting time
tmp = sight_df %>% 
count(Date1 = as.Date(date), Hour = hour(date)) %>%
  group_by(Date1) %>% 
  complete(Date1, Hour = min(Hour):24, fill = list(n = 0)) %>%
  arrange(Date1, Hour)