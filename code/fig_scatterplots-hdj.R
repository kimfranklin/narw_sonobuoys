## fig_scatterplots.R ##

# libraries 
library(tidyverse)
library(patchwork)

# data
df = readRDS("data/processed/proc_acou_photoid.rds")

# whale abundance
p1 = df %>%
  ggplot(aes(x = as.Date(yday,'1970-01-01'), y = num_sighting, fill = ratio_male_female))+
  geom_point(size = 3, shape = 21)+
  scale_fill_gradient2(low = 'blue', high = 'red', mid = 'white', midpoint = 1)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(limits = c(0,NA))+
  labs(x = NULL,
       y = 'Number of Whales',
       fill = "Male:Female")+
  coord_cartesian(clip = 'off', expand = FALSE)+
  theme_classic() +
  theme(axis.text.x = element_blank())

# call rate
p2 = df %>%
  select("yday", 'year', "up_per_hr", "gs_per_hr", "mf_per_hr") %>%
  gather(key = "call" , value = "cval", up_per_hr, gs_per_hr, mf_per_hr) %>%
  ggplot(aes(x = as.Date(yday,'1970-01-01'), y = cval))+
  geom_point(aes(fill = call), shape = 21, alpha = 0.7, size = 3)+
  scale_fill_manual(values = c("white", "steelblue3", "tan"),
                      breaks=c("up_per_hr", "gs_per_hr", "mf_per_hr"),
                      labels=c("Upcall", "Gunshot","Tonal"))+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  labs(x = NULL,
       y = 'Rate (call/h)',
       fill = "Call Type")+
  coord_cartesian(clip = 'off', expand = FALSE)+
  theme_classic() +
  theme(axis.text.x = element_blank())

# behaviour rate
p3 = df %>%
  select("yday", 'year', "foraging_bhv_whale", "social_bhv_whale") %>%
  gather(key = "bhv" , value = "cval", foraging_bhv_whale, social_bhv_whale) %>%
  ggplot(aes(x = as.Date(yday,'1970-01-01'), y = cval))+
  geom_point(aes(fill = bhv), shape = 21, alpha = 0.7, size = 3)+
  scale_fill_viridis_d(labels=c("Foraging", "Socalizing"))+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  labs(x = NULL,
       y = 'Behavior rate (behavior/whale)',
       fill = "Behavior")+
  theme_bw()+
  coord_cartesian(clip = 'off', expand = FALSE)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

# combine and annotate
p = wrap_plots(p1,p2,p3, ncol = 1)+
  plot_annotation(tag_levels = 'a', tag_suffix = ')')

# save
ggsave(filename = 'figures/scatterplot.png', plot = p, width = 8, height = 10, units = 'in', dpi = 300)
