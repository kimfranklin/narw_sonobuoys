## fig_scatterplots.R ##

# libraries 
library(tidyverse)
library(viridis)
library(ggplot2)
library(grid)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(oce)

# figure directory
fig_dir = 'figures/'

# data
df = readRDS("data/processed/proc_acou_photoid.rds")

# call rate scatterplot
plta = df %>%
  select("yday", 'year', "up_per_hr", "gs_per_hr", "mf_per_hr") %>%
  gather(key = "call" , value = "cval", up_per_hr, gs_per_hr, mf_per_hr) %>%
  filter(call %in% c("up_per_hr", "gs_per_hr", "mf_per_hr")) %>%
  ggplot(aes(x = as.Date(yday,'1970-01-01'), y = cval))+
  geom_point(aes(colour = call), alpha = 0.5, size = 3)+
  # geom_smooth(aes(color = sex, fill = sex), method = "lm",
  #             se = TRUE, fullrange = TRUE, show.legend = FALSE)+
  scale_colour_manual(values = c("red", "blue","black"),
                      breaks=c("up_per_hr", "gs_per_hr", "mf_per_hr"),
                      labels=c("Upcall", "Gunshot","Tonal"))+
  labs(colour = "Call Type",
       x = NULL,
       y = NULL)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,1))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  theme(text = element_text(size=14, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  # theme(axis.title.x=element_blank(),
  #       axis.text.x=element_blank())+
  labs(x = 'Week',
       y = 'Rate (call/h)',
       title = (expression(paste('a) Call rates'))))
plta

# behaviour rate scatterplot
pltb = df %>%
  select("yday", 'year', "foraging_bhv_whale", "social_bhv_whale") %>%
  gather(key = "bhv" , value = "cval", foraging_bhv_whale, social_bhv_whale) %>%
  filter(bhv %in% c("foraging_bhv_whale", "social_bhv_whale")) %>%
  ggplot(aes(x = as.Date(yday,'1970-01-01'), y = cval))+
  geom_point(aes(colour = bhv), alpha = 0.5, size = 3)+
  # geom_smooth(aes(color = sex, fill = sex), method = "lm",
  #             se = TRUE, fullrange = TRUE, show.legend = FALSE)+
  scale_colour_manual(values = c("red", "blue"),
                      breaks=c("foraging_bhv_whale", "social_bhv_whale"),
                      labels=c("Foraging", "Socalizing"))+
  labs(colour = "Behavior",
       x = NULL,
       y = NULL)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,0.0025))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  theme(text = element_text(size=14, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  # theme(axis.title.x=element_blank(),
  #       axis.text.x=element_blank())+
  labs(x = 'Week',
       y = 'Behavior rate (behavior/whale)',
       title = (expression(paste('b) Behavior rates'))))
pltb

png(paste0(fig_dir, 'manuscript_scatterplots.png'), height = 8, width = 8,
    units = 'in', res = 300)

grid.newpage()
grid.draw(rbind(ggplotGrob(plta), 
                ggplotGrob(pltb), size = "last"))

dev.off()

# whale abundance scatterplot
pltc = df %>%
  select("yday", 'year', "num_sighting") %>%
  # gather(key = "dem" , value = "cval", num_sighting) %>%
  # filter(dem %in% c("num_sighting")) %>%
  ggplot(aes(x = as.Date(yday,'1970-01-01'), y = num_sighting))+
  geom_point(#aes(colour = 'red'), 
    color = 'red', alpha = 0.5, size = 3)+
  # geom_smooth(aes(color = sex, fill = sex), method = "lm",
  #             se = TRUE, fullrange = TRUE, show.legend = FALSE)+
  scale_colour_manual(values = c("red"),
                      breaks=c("num_sighting"),
                      labels=c("Whale abundance"))+
  labs(#colour = "",
       x = NULL,
       y = NULL)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,1))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  theme(text = element_text(size=14, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  # theme(axis.title.x=element_blank(),
  #       axis.text.x=element_blank())+
  labs(x = 'Week',
       y = 'Number of Whales',
       title = (expression(paste('c) Whale Abundance'))))
pltc

ggsave(pltc, filename = paste0('figures/manuscript_abundance.png'), 
        height = 5, width = 7, units = 'in', dpi = 300)

# demographics bar graph
df$date_discrete = c('2017-06-27', '2017-06-29', '2017-07-05', '2017-07-08', '2017-07-20', 
                    '2017-07-21', '2017-07-24', '2017-07-26', '2018-06-06', '2018-06-07', 
                    '2018-06-11', '2018-06-15', '2018-06-17', '2018-06-26', '2018-06-29', 
                    '2018-07-19', '2018-07-21', '2018-07-27', '2018-08-03', '2018-08-06', 
                    '2018-08-12', '2019-06-04', '2019-06-05', '2019-06-07', '2019-06-09', 
                    '2019-06-10', '2019-06-13', '2019-07-11', '2019-07-16', '2019-07-19 a', 
                    '2019-07-19 b', '2019-08-14', '2019-08-15', '2019-08-16', '2019-08-21',
                    '2019-08-25', '2019-08-26')

dfp = df %>%
  select("date_discrete", "adult_female","adult_male","calf_female","calf_male",
          "juvenile_female","juvenile_male","unknown") %>%
  #mutate(date = as.Date(yday, origin = '2018-01-01')) %>% 
  gather(key = "age_sex" , value = "cval", adult_female,adult_male,calf_female,calf_male,
         juvenile_female,juvenile_male,unknown)

# plot
pltd = ggplot(data = dfp, aes(x=date_discrete,y=cval,fill=age_sex))+
  geom_col(position = "stack")+
  
  # remove white space around plot
  scale_y_continuous(expand = c(0,0))+
  
  # specify weekly axis labels
  #scale_x_date(breaks = '1 week', date_labels = '%b-%d')+
  
  # make pretty
  scale_fill_grey(start = 0.1, end = 0.9,  
                  breaks=c("adult_female","adult_male","calf_female","calf_male",
                           "juvenile_female","juvenile_male","unknown"),
                  labels=c("Adult, Female","Adult, Male","Calf, Female", "Calf, Male", "Juvenile, Female","Juvenile, Male", "Unknown"))+
  labs(fill = "Age Class, Sex",
       y = 'Number of Whales',
       x = 'Date')+
  theme_bw()+
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, colour = 'black'),
        axis.text.y = element_text(colour = 'black'),
        text = element_text(size=14, family = "serif"))

pltd

ggsave(pltd, filename = paste0('figures/manuscript_demographics.png'), 
       height = 5, width = 8, units = 'in', dpi = 300)
