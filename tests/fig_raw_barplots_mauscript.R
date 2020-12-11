## figures.R ##
# making repeat of figures from thesis, but including 2019 NOAA data

# figure directory
fig_dir = 'figures/'

# define input data file
ifile = readRDS("data/processed/proc_acou_photoid.rds")
df= ifile

# libraries 
library(tidyverse)
library(viridis)
library(ggplot2)
library(grid)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(oce)

# call rate by week
dfp = df %>%
  select("id", "up", "mf", "gs") %>%
  #mutate(date = as.Date(yday, origin = '2018-01-01')) %>% 
  gather(key = "call_type" , value = "cval", up, mf, gs)
# plot
plt = ggplot(data = dfp, aes(x=id,y=cval,fill=call_type))+
  geom_col(position = "stack")+
  
  # remove white space around plot
  scale_y_continuous(expand = c(0,0))+
  
  # specify weekly axis labels
  #scale_x_date(breaks = '1 week', date_labels = '%b-%d')+
  
  # make pretty
  scale_fill_grey(start = 0.1, end = 0.7,  
                  breaks=c("up", "mf", "gs"),
                  labels=c("Upcall", "Mid-frequency","Gunshot"))+
  labs(fill = "Call Type",
       y = 'Call Count',
       x = NULL,
       title = 'a)')+
  theme_bw()+
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45, hjust = 1, colour = 'black'),
        axis.text.y = element_text(colour = 'black'),
        text = element_text(size=14, family = "serif"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

plt

# ggsave(plt, filename = paste0('figures/raw_call_category.png'), 
#        height = 5, width = 8, units = 'in', dpi = 300)


# demographics bar graph
dfp = df %>%
  select("id", "juvenile_male", "juvenile_female", 
         "adult_male", "adult_female", "unknown") %>%
  #mutate(date = as.Date(yday, origin = '2018-01-01')) %>% 
  gather(key = "age_sex" , value = "cval", juvenile_male, juvenile_female, adult_male, adult_female, unknown)

# plot
pltd = ggplot(data = dfp, aes(x=id,y=cval,fill=age_sex))+
  geom_col(position = "stack")+
  
  # remove white space around plot
  scale_y_continuous(expand = c(0,0))+
  
  # specify weekly axis labels
  #scale_x_date(breaks = '1 week', date_labels = '%b-%d')+
  
  # make pretty
  scale_fill_grey(start = 0.1, end = 0.7,  
                  breaks=c("juvenile_male", "juvenile_female", "adult_male", "adult_female", "unknown"),
                  labels=c("Juvenile, Male", "Juvenile, Female","Adult, Male", "Adult, Female", "Unknown"))+
  labs(fill = "Age Class, Sex",
       y = 'Sighted Whales',
       x = NULL,
       title = 'b)')+
  theme_bw()+
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 45, hjust = 1, colour = 'black'),
        axis.text.y = element_text(colour = 'black'),
        text = element_text(size=14, family = "serif")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

pltd

# ggsave(pltd, filename = paste0('figures/raw_demogrpahics_category.png'), 
#        height = 5, width = 8, units = 'in', dpi = 300)


# behaviour stacked bar graph
# wrangle plot data (coercing all data to the same year makes plotting much easier)
dfp = df %>%
  select("id", "foraging", "social", "other_bhv") %>%
  #mutate(date = as.Date(yday, origin = '2018-01-01')) %>% 
  gather(key = "behaviour" , value = "cval", foraging, social, other_bhv)

# plot
pltb = ggplot(data = dfp, aes(x=id,y=cval,fill=behaviour))+
  geom_col(position = "stack")+
  
  # remove white space around plot
  scale_y_continuous(expand = c(0,0))+
  
  # specify weekly axis labels
  #scale_x_date(breaks = '1 week', date_labels = '%b-%d')+
  
  # make pretty
  scale_fill_grey(start = 0.1, end = 0.7,  
                  breaks=c("foraging", "social", "other_bhv"),
                  labels=c("Foraging", "Social", "Other Behaviour"))+
  labs(fill = "Behaviour",
       y = 'Sighted Whale Behaviour Count',
       x = 'Deployment ID',
       title = 'c)')+
  theme_bw()+
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, colour= 'black'),
        axis.text.y = element_text(colour = 'black'),
        text = element_text(size=14, family = "serif"))

pltb

# ggsave(pltb, filename = paste0('figures/raw_behaviour_category.png'), 
#        height = 5, width = 8, units = 'in', dpi = 300)


png(paste0(fig_dir, 'raw_call_demo_bhv_category.png'), height = 8, width = 7,
    units = 'in', res = 300)

grid.newpage()
grid.draw(rbind(ggplotGrob(plt), 
                ggplotGrob(pltd), 
                ggplotGrob(pltb), size = "last"))

dev.off()

