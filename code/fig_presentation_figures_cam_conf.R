## presenation_figures_cam_conf
# cameron confernece figures for presentation 

# figure directory
fig_dir = 'figures/'

# read in data
ifile = readRDS("data/processed/sight_dfs.rds")
df= ifile

# libraries
library(tidyverse)
library(viridis)
library(ggplot2)
library(dplyr)
library(lubridate)
library(grid)
library(plotly)
library(RColorBrewer)
library(oce)
library(xtable)
library(mapproj)
source('D:/narw_sonobuoys/functions.R') 


# Raw plots ---------------------------------------------------------------
# behaviour stacked plot
pltb = df %>%
  select(as.numeric("yday"), "feed", "social", "other_behv") %>%
  gather(key = "behaviour" , value = "cval", feed, social, other_behv) %>%
  filter(behaviour %in% c('feed', 'social', 'other_behv')) %>%
  ggplot(aes(x = as.Date(yday(date),'1970-01-01'), y = cval, fill = behaviour))+
  geom_bar(position = 'stack', stat = 'identity', width = 2)+
  scale_fill_viridis(discrete = TRUE, option = "E", 
                     breaks=c("feed", "social", "other_behv"),
                     labels=c("Feeding", "Socializing","Other Behaviour"))+
  labs(fill = "Behaviour",
       x = NULL,
       y = NULL,
       title = 'Sighted behaviour within 15 km plus an hour before and after recording')+
  # facet by year
  #facet_wrap(~year)+
  ylab('Sighted Whale Behaviour')+
  xlab('Week')+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
       legend.position="bottom")+
  theme(axis.text.x = element_text(), 
       legend.position="right")+
  #theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  theme(text = element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))
pltb

# calls stacked plot
pltc = df %>%
  select(as.numeric("yday"), "year", "up_dur_hr", "mf_dur_hr", "gs_dur_hr") %>%
  gather(key = "call_type" , value = "cval", up_dur_hr, mf_dur_hr, gs_dur_hr) %>%
  filter(call_type %in% c('up_dur_hr', 'mf_dur_hr', 'gs_dur_hr')) %>%
  ggplot(aes(x = as.Date(yday(date),'1970-01-01'), y = cval, fill = call_type))+
  geom_bar(position="stack", stat = 'identity', width = 2)+
  scale_fill_viridis(discrete = TRUE, option = "C", 
                     breaks=c("up_dur_hr", "mf_dur_hr", "gs_dur_hr"),
                     labels=c("Up Call", "Mid-frequency","Gunshot"))+
  labs(fill = "Call Type",
       x = NULL,
       y = NULL,
       title = 'Calls detected per duration of deployment')+
  # facet by year
  #facet_wrap(~year)+
  ylab('Call Rate (call/hour)')+
  xlab('Week')+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(breaks = seq(0,275,50))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  #theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  theme(text = element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))
pltc

# demographics stacked plot
pltd = df %>%
  select(as.numeric("yday"), "year", "juvenile_male", "juvenile_female", "adult_male", "adult_female") %>%
  gather(key = "age_sex" , value = "cval", juvenile_male, juvenile_female, adult_male, adult_female) %>%
  #add to make sure character or factor to fix legend and graph colour?
  filter(age_sex %in% c("juvenile_male", "juvenile_female", "adult_male", "adult_female")) %>%
  ggplot(aes(x = as.Date(yday(date),'1970-01-01'), y = cval, fill = age_sex))+
  geom_bar(position="stack", stat = 'identity', width = 2)+
  scale_fill_viridis(discrete = TRUE, option = "D", 
                     breaks=c("juvenile_male", "juvenile_female", "adult_male", "adult_female"),
                     labels=c("Juvenile, Male", "Juvenile, Female","Adult, Male", "Adult, Female"))+
  labs(fill = "Age Class, Sex",
       x = NULL,
       y = NULL,
       title = 'Sightings within 15 km plus an hour before and after recording')+
  # facet by year
  #facet_wrap(~year)+
  ylab('Sighted Whales')+
  xlab('Week')+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  #theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  theme(text = element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))
pltd

png(paste0(fig_dir, 'calls_behaviour_raw.png'), height = 10, width = 12,
    units = 'in', res = 300)

grid.newpage()
grid.draw(rbind(ggplotGrob(pltc), ggplotGrob(pltb), size = "last"))

dev.off()

# Seasonality plots -------------------------------------------------------
# Demographic vs seasonality
df %>%
  ggplot()+
  geom_point(aes(x = yday, y = 1/ratio_female_male))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  ylab('Female to Male ratio inverse')+
  xlab('Year Day')

# Abundance vs seasonality
df %>%
  ggplot()+
  geom_point(aes(x = yday, y = num_sighting))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  ylab('Number of individuals sighted')+
  xlab('Year Day')

# Seasonality in calling
plt = df %>%
  mutate(up_call_rate = up_dur_hr/num_sighting) %>%
  mutate(mf_call_rate = mf_dur_hr/num_sighting) %>%
  mutate(gs_call_rate = gs_dur_hr/num_sighting) %>%
  select(as.numeric("yday"), "year", 
         "up_call_rate", "mf_call_rate", "gs_call_rate") %>%
  gather(key = "call_type" , value = "cval", 
         up_call_rate, mf_call_rate, gs_call_rate) %>%
  filter(call_type %in% c('up_call_rate', 'mf_call_rate', 'gs_call_rate')) %>%
  ggplot(aes(x = as.Date(yday(date),'1970-01-01'), y = cval))+
  geom_point(aes(colour = call_type), size = 3)+
  scale_colour_manual(up_call_rate='red',mf_call_rate='blue',
                      gs_call_rate='black',
                      breaks=c("up_call_rate", "mf_call_rate", "gs_call_rate"),
                      labels=c("Up call", "Mid-frequency","Gunshot"))+
  # scale_colour_viridis(discrete = TRUE, option = "D",
  #                   breaks=c("up_call_rate", "mf_call_rate", "gs_call_rate"),
  #                   labels=c("Up call", "Mid-frequency","Gunshot"))+
  # #facet_wrap(~call_type)+
  labs(colour = "Call Type",
       x = NULL,
       y = NULL)+
  theme(text = element_text(size=20))+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  #scale_y_log10()+
  #scale_y_continuous(trans = 'log10')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  ylab('Call Rate (call/hour/whale)')+
  xlab('Week')
plt

ggsave(plt, filename = paste0('figures/call_season_updated.png'), 
       height = 7, width = 10, units = 'in', dpi = 300) 

# Seasonality in behaviour
plt = df %>%
  mutate(feed_perc = (feed/num_sighting)*100) %>%
  mutate(social_perc = (social/num_sighting)*100) %>%
  select(as.numeric("yday"), "year", "feed_perc", "social_perc") %>%
  gather(key = "behaviour" , value = "cval", feed_perc, social_perc) %>%
  filter(behaviour %in% c('feed_perc', 'social_perc')) %>%
  ggplot(aes(x = as.Date(yday(date),'1970-01-01'), y = cval))+
  geom_point(aes(colour = behaviour), size = 3)+
  geom_smooth(aes(color = behaviour, fill = behaviour), method = "lm",
              se = TRUE, fullrange = TRUE, show.legend = FALSE)+
  scale_colour_viridis(discrete = TRUE, option = "D",
                       breaks=c("feed_perc", "social_perc"),
                       labels=c("Feeding", "Socalizing"))+
  labs(colour = "Behaviour Type",
       x = NULL,
       y = NULL)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="bottom")+
  theme(axis.text.x = element_text(),
        legend.position="right")+
  theme(text = element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  ylab('Behaviour Occurance (%)')+
  xlab('Week')
plt  

ggsave(plt, filename = paste0('figures/behaviour_season_updated_noarcsin.png'), 
       height = 7, width = 10, units = 'in', dpi = 300) 


lm=lm(df$social~df$yday)
anova(lm)
summary(lm)

# up calls over season
plt = df %>%
  mutate(call_rate = up_dur_hr/num_sighting) %>%
  ggplot()+
  geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = call_rate), color = 'black', size = 3)+
  #geom_smooth(aes(x = yday, y = call_rate), method = "lm", color='black',
              #se = FALSE, fullrange = TRUE)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  theme(text = element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  ylab('Call Rate (up calls/hour/whale)')+
  xlab('Week')

plt

lm=lm((df$up_dur_hr/df$num_sighting)~df$yday)
anova(lm)
summary(lm)

ggsave(plt, filename = paste0('figures/up_call_season.png'), 
       height = 7, width = 10, units = 'in', dpi = 300) 

# mf calls over season
plt = df %>%
  mutate(call_rate = mf_dur_hr/num_sighting) %>%
  ggplot()+
  geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = call_rate), color = 'black', size = 3)+
  #geom_smooth(aes(x = yday, y = call_rate), method = "lm", color='black',
  #se = FALSE, fullrange = TRUE)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  theme(text = element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  ylab('Call Rate (mid-frequency calls/hour/whale)')+
  xlab('Week')
plt

ggsave(plt, filename = paste0('figures/mf_call_season.png'), 
       height = 7, width = 10, units = 'in', dpi = 300) 

# gs calls over season
plt = df %>%
  mutate(call_rate = gs_dur_hr/num_sighting) %>%
  ggplot()+
  geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = call_rate), color = 'black', size = 3)+
  #geom_smooth(aes(x = yday, y = call_rate), method = "lm", color='black',
  #se = FALSE, fullrange = TRUE)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  theme(text = element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  ylab('Call Rate (gunshot calls/hour/whale)')+
  xlab('Week')
plt

ggsave(plt, filename = paste0('figures/gs_call_season.png'), 
       height = 7, width = 10, units = 'in', dpi = 300) 

#Sightings vs call counts
plt = df %>%
  mutate(call_rate = sum_calls_dur*60*60/num_sighting) %>%
  ggplot()+
  geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = call_rate), color = 'black', size = 3)+
  geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = call_rate), method = "lm", color='black',
              se = TRUE, fullrange = TRUE)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(text = element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  ylab('Call Rate (call/hour/whale)')+
  xlab('Week')

plt
lm=lm((df$sum_calls_dur*60*60/df$num_sighting)~df$yday)
anova(lm)
summary(lm)
ggsave(plt, filename = paste0('figures/call_rate_season.png'), 
       height = 7, width = 10, units = 'in', dpi = 300) 

# ratio between mf and upcalls
df %>%
  mutate(gm_ratio = num_mf / num_up) %>%
  ggplot()+
  geom_point(aes(x = yday, y = gm_ratio, color = social), size = 3)+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  ylab('Mid-frequency calls per Up calls')+
  xlab('Year Day')

# combining behaviour and call rate scatter plots 
plt = 
  ggplot()+
  geom_point(aes(x = plt1$yday, y = plt1$call_rate), color = 'black', size = 3)+
  geom_smooth(aes(x = plt1$yday, y = plt1$call_rate), method = "lm", color='black',
              se = TRUE, fullrange = TRUE)+
  geom_point(aes(x = plt1$yday, y = df$social), colour = "blue", size = 3)+
  geom_smooth(aes(x = plt1$yday, y = df$social), method = "lm", color='blue',
              se = TRUE, fullrange = TRUE)+
  geom_point(aes(x = plt1$yday, y = df$feed), colour = "red", size = 3)+
  geom_smooth(aes(x = plt1$yday, y = df$feed), method = "lm", color='red',
              se = TRUE, fullrange = TRUE)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous("plt2$cval", sec.axis = sec_axis(~. *2.25, name = "Behaviour Occurances"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(text = element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  ylab('Call Rate per Sighting (call/hour/whale)')+
  xlab('Week')
plt

# bar graph of behaviour and regression of call rate ontop
#this is the call rate data and is for plotting it ontop of the bargraph
#and for the regression it makes
plt1 =df %>%
  transmute(call_rate = sum_calls_dur*60*60/num_sighting,
            yday=yday)
plt1 = plt1[rep(seq_len(nrow(plt1)), 3), ]
#this is the bar graph
pltb = df %>%
  select(as.numeric("yday"), "feed", "social", "other_behv") %>%
  gather(key = "behaviour" , value = "cval", feed, social, other_behv) %>%
  filter(behaviour %in% c('feed', 'social', 'other_behv')) %>%
  ggplot(aes(x = as.Date(yday(date),'1970-01-01'), y = cval, fill = behaviour))+
  geom_bar(position = 'stack', stat = 'identity', width = 2)+
  scale_fill_manual('Behaviour', 
                    values = c('feed' = 'darkblue',
                               'social' = ' yellow',
                               'other_behv' = 'darkgrey'), 
                     breaks=c("feed", "social", "other_behv"),
                     labels=c("Feeding", "Socializing","Other Behaviour"))+
  #labs(#fill = "Behaviour",
       #x = NULL,
       #y = NULL,
       #title = 'Sighted behaviour within 15 km plus an hour before and after recording')+
  ylab('Sighted Whale Behaviour')+
  xlab('Week')+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  theme(text = element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))
pltb
#this is the call rate and regression added ontop
pltb2 = pltb+ 
  geom_point(aes(x = as.Date(yday(date),'1970-01-01'), 
               y = plt1$call_rate, 
               colour = 'Call Rate'), 
             fill = plt1$call_rate,
           color = 'black', size = 3)+
  scale_colour_manual("Call Rate", 
                      values = c('plt1$call_rate' = 'black'),
                      breaks=c("plt1$call_rate"),
                      labels=c("Call Rate"),
                      guide = guide_legend(override.aes = list(fill = NA)),
                      drop = F)+
  geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = plt1$call_rate), 
              colour = 'black', 
              method = "lm",
              se = TRUE, 
              fullrange = TRUE,
              alpha = 0.1,
              show.legend = FALSE)+
  scale_y_continuous("Sighted Whale Behaviour", 
                     sec.axis = sec_axis(~. , name = "Call Rate (call/hour/whale)"))
pltb2

ggsave(pltb2, filename = paste0('figures/behaviour_bar_callrate_regres_updated.png'), 
       height = 7, width = 10, units = 'in', dpi = 300) 


