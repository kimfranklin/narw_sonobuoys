## fig_scatterplots.R ##

# libraries 
library(tidyverse)
library(patchwork)

# data
df = readRDS("data/processed/proc_acou_photoid.rds")

df[nrow(df)+1,] <- NA

df$yday[is.na(df$yday)] <- 154

df[nrow(df)+1,] <- NA

df$yday[is.na(df$yday)] <- 240

# whale count
p1 = df %>%
  ggplot(aes(x = as.Date(yday,'1970-01-01'), 
             y = num_sighting))+#, 
             #fill = ratio_male_female
             #fill = cut(ratio_male_female,c(0, 1, 2, 3, 4))))+
  geom_point(size = 3, alpha = 0.7, shape = 19)+
  #scale_fill_gradient2(low = 'blue', high = 'red', mid = 'white', midpoint = 1)+
  scale_fill_brewer(palette = "Reds")+
  coord_cartesian(clip = 'off', expand = FALSE, ylim = c(0,60))+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+ #, limits = as.Date(c('2017-07-27', '2019-08-26')))+
  scale_y_continuous(limits = c(0,NA))+
  labs(x = "Day of year",
       y = 'Number of Whales',
       #fill = "Male/Female",
       fill = "Male/Female Ratio")+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, colour = "black"))+
  theme(text = element_text(size=12, family = "sans"))+
  theme(panel.border = element_rect(colour = "black", fill=NA))+
  #theme(plot.background = element_rect(color = "black"))+
  theme(axis.text.y = element_text(colour = "black"))
p1

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
       y = (bquote('Rate (calls'~h^-1~')')),
       fill = "Call Type")+
  coord_cartesian(clip = 'off', expand = FALSE, ylim = c(0, 414))+
  theme_classic() +
  theme(axis.text.x = element_blank(),
        legend.key = element_rect(color = 'black', fill = NA),
        text = element_text(size=12, family = "sans"),
        panel.border = element_rect(colour = "black", fill=NA),
        #plot.background = element_rect(color = "black"),
        axis.text.y = element_text(colour = "black"))
p2

# behaviour rate
p3 = df %>%
  select("yday", 'year', "foraging_bhv_whale", "social_bhv_whale") %>%
  gather(key = "bhv" , value = "cval", foraging_bhv_whale, social_bhv_whale) %>%
  ggplot(aes(x = as.Date(yday,'1970-01-01'), y = cval))+
  geom_point(aes(fill = bhv), shape = 21, alpha = 0.7, size = 3)+
  scale_fill_viridis_d(labels=c("Foraging", "Socalizing"))+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  labs(x = NULL,
       y = (bquote('Behavior rate (behavior'~whale^-1~')')),
       fill = "Behavior")+
  theme_bw()+
  coord_cartesian(clip = 'off', expand = FALSE, ylim = c(0, 0.939))+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        legend.key = element_rect(color = 'black', fill = NA),
        text = element_text(size=12, family = "sans"),
        panel.border = element_rect(colour = "black", fill=NA),
        #plot.background = element_rect(color = "black"),
        axis.text.y = element_text(colour = "black"))
p3

# combine and annotate
p = wrap_plots(p2,p3,p1, ncol = 1)+
  plot_annotation(tag_levels = 'a', tag_suffix = ')')

p
# save
ggsave(filename = 'figures/scatterplot_new_updated_updated_withboarderfixed_sansfont.png', plot = p, width = 7, height = 9, units = 'in', dpi = 300)
