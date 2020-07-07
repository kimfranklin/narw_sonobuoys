thesis_results


# Start up ----------------------------------------------------------------
# figure directory
fig_dir = 'figures/'

# data
ifile = readRDS("data/processed/sight_dfs.rds")
df= ifile

# libraries
library(tidyverse)
library(viridis)
library(ggplot2)
library(dplyr)
library(lubridate)
library(grid)
library(RColorBrewer)
library(oce)
library(xtable)

df2=df%>%
  mutate(sum_adult=adult_female+adult_male)

# Results -----------------------------------------------------------------

# Question 2 Do the number of whale (sightings) change with time
lm = lm(df$num_sighting~df$yday)
anova(lm)
summary(lm)

plt = df %>%
  ggplot(aes(x=as.Date(yday(date),'1970-01-01'), y=num_sighting))+
  geom_point(size = 3)+
  geom_smooth(method = 'lm')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  ylab('Number of Sighted Whales')+
  xlab('Week')+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(text = element_text(size=20))
plt

ggsave(plt, filename = paste0('figures/question_2.png'), 
       height = 7, width = 10, units = 'in', dpi = 300) 

# Question 3 Do the sighted whales demographics change with time
# Male/Female
lm = lm(df$sum_female~df$yday)
anova(lm)
summary(lm)

lm = lm(df$sum_male~df$yday)
anova(lm)
summary(lm)

plt = df %>%
  select(as.numeric("yday"), "year", "sum_female", "sum_male") %>%
  gather(key = "sex" , value = "cval", sum_female, sum_male) %>%
  filter(sex %in% c('sum_female', 'sum_male')) %>%
  ggplot(aes(x = as.Date(yday(date),'1970-01-01'), y = cval))+
  geom_point(aes(colour = sex), size = 3)+
  geom_smooth(aes(color = sex, fill = sex), method = "lm",
              se = TRUE, fullrange = TRUE, show.legend = FALSE)+
  scale_colour_viridis(discrete = TRUE, option = "D",
                       breaks=c("sum_female", "sum_male"),
                       labels=c("Female", "Male"))+
  labs(colour = "Sex",
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
  ylab('Number of individuals')+
  xlab('Week')

plt

ggsave(plt, filename = paste0('figures/question_3a.png'), 
       height = 7, width = 10, units = 'in', dpi = 300)

# Juvenile/Adult
lm = lm(df2$sum_adult~df$yday)
anova(lm)
summary(lm)

lm = lm(df2$sum_juvenile~df$yday)
anova(lm)
summary(lm)

plt = df2 %>%
  select(as.numeric("yday"), "year", "sum_juvenile", "sum_adult") %>%
  gather(key = "age" , value = "cval", sum_juvenile, sum_adult) %>%
  filter(age %in% c('sum_juvenile', 'sum_adult')) %>%
  ggplot(aes(x = as.Date(yday(date),'1970-01-01'), y = cval))+
  geom_point(aes(colour = age), size = 3)+
  geom_smooth(aes(color = age, fill = age), method = "lm",
              se = TRUE, fullrange = TRUE, show.legend = FALSE)+
  scale_colour_viridis(discrete = TRUE, option = "D",
                       breaks=c("sum_juvenile", "sum_adult"),
                       labels=c("Juvenile", "Adult"))+
  labs(colour = "Age",
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
  ylab('Number of individuals')+
  xlab('Week')

plt

ggsave(plt, filename = paste0('figures/question_3b.png'), 
       height = 7, width = 10, units = 'in', dpi = 300)

# Question 4 Does behaviour change with time?
lm = lm(df$feed~df$yday)
anova(lm)
summary(lm)

lm = lm(df$social~df$yday)
anova(lm)
summary(lm)

lm = lm(df$other_behv~df$yday)
anova(lm)
summary(lm)

plt = df %>%
  select(as.numeric("yday"), "year", "feed", "social", "other_behv") %>%
  gather(key = "behaviour" , value = "cval", feed, social, other_behv) %>%
  filter(behaviour %in% c('feed', 'social', 'other_behv')) %>%
  ggplot(aes(x = as.Date(yday(date),'1970-01-01'), y = cval))+
  geom_point(aes(colour = behaviour), size = 3)+
  geom_smooth(aes(color = behaviour, fill = behaviour), method = "lm",
              se = TRUE, fullrange = TRUE, show.legend = FALSE)+
  scale_colour_viridis(discrete = TRUE, option = "D",
                       breaks=c("feed", "social", "other_behv"),
                       labels=c("Feeding", "Socalizing", "Other Behaviour"))+
  labs(colour = "Behaviour Type",
       x = NULL,
       y = NULL)+
  facet_wrap(~behaviour)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  theme(text = element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  ylab('Behaviour Occurance')+
  xlab('Week')

plt

ggsave(plt, filename = paste0('figures/question_4.png'), 
       height = 7, width = 10, units = 'in', dpi = 300)

# Question 5 Do the call rate and call-type rates change with time
#CALL RATE
lm = lm(df$sum_calls_dur*60*60/df$num_sighting~df$yday)
anova(lm)
summary(lm)



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
  ylab('Call Rate per Sighting (call/hour/whale)')+
  xlab('Week')
plt

ggsave(plt, filename = paste0('figures/question_5_call_rate.png'), 
       height = 7, width = 10, units = 'in', dpi = 300)

#CALL-RATE TYPES
lm = lm(df$gs_dur_hr/df$num_sighting~df$yday)
anova(lm)
summary(lm)

lm = lm(df$mf_dur_hr/df$num_sighting~df$yday)
anova(lm)
summary(lm)

lm = lm(df$up_dur_hr/df$num_sighting~df$yday)
anova(lm)
summary(lm)

plt = df %>%
  select(as.numeric("yday"), "year", "up_dur_hr", "mf_dur_hr", "gs_dur_hr") %>%
  gather(key = "call_type" , value = "cval", up_dur_hr, mf_dur_hr, gs_dur_hr) %>%
  filter(call_type %in% c('up_dur_hr', 'mf_dur_hr', 'gs_dur_hr')) %>%
  ggplot(aes(x = as.Date(yday(date),'1970-01-01'), y = cval/num_sighting))+
  geom_point(aes(colour = call_type), size = 3)+
  scale_colour_viridis(discrete = TRUE, option = "D",
                       breaks=c("up_dur_hr", "mf_dur_hr", "gs_dur_hr"),
                       labels=c("Up call", "Mid-frequency","Gunshot"))+
  facet_wrap(~call_type)+
  geom_smooth(aes(color = call_type, fill = call_type), method = "lm",
              se = TRUE, fullrange = TRUE, show.legend = FALSE)+
  labs(colour = "Call Type",
       x = NULL,
       y = NULL)+
  theme(text = element_text(size=20))+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  ylab('Call Rate (call/hour/whale)')+
  xlab('Week')
plt

ggsave(plt, filename = paste0('figures/question_5_calltype_rate.png'), 
       height = 7, width = 10, units = 'in', dpi = 300)

# Question 6 do the call rate and call-type change with the observed sightings 
#CALL RATE
lm = lm(df$sum_calls_dur*60*60~df$num_sighting)
anova(lm)
summary(lm)

plt = df %>%
  mutate(call_rate=sum_calls_dur*60*60)%>%
  ggplot(aes(x=num_sighting, y=call_rate))+
  geom_point(size = 3)+
  geom_smooth(method = 'lm')+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1), 
  #legend.position="bottom")+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(text = element_text(size=20))+
  ylab('Call Rate (call/hour)')+
  xlab('Number of whales sighted')

plt

ggsave(plt, filename = paste0('figures/question_6_call_rate.png'), 
       height = 7, width = 10, units = 'in', dpi = 300)

#UP CALLS
lm = lm(df$up_dur_hr~df$num_sighting)
anova(lm)
summary(lm)

plt = df %>%
  ggplot(aes(x=num_sighting, y=up_dur_hr))+
  geom_point(size = 3)+
  geom_smooth(method = 'lm')+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1), 
  #legend.position="bottom")+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(text = element_text(size=20))+
  ylab('Up Call Rate (call/hour)')+
  xlab('Number of whales sighted')

plt

ggsave(plt, filename = paste0('figures/question_6_upcall_rate.png'), 
       height = 7, width = 10, units = 'in', dpi = 300)

#GUNSHOT CALLS
lm = lm(df$gs_dur_hr~df$num_sighting)
anova(lm)
summary(lm)

plt = df %>%
  ggplot(aes(x=num_sighting, y=gs_dur_hr))+
  geom_point(size = 3)+
  geom_smooth(method = 'lm')+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1), 
  #legend.position="bottom")+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(text = element_text(size=20))+
  ylab('Gunshot Call Rate (call/hour)')+
  xlab('Number of whales sighted')

plt

ggsave(plt, filename = paste0('figures/question_6_gscall_rate.png'), 
       height = 7, width = 10, units = 'in', dpi = 300)

#MIDFREQ CALLS
lm = lm(df$mf_dur_hr~df$num_sighting)
anova(lm)
summary(lm)

plt = df %>%
  ggplot(aes(x=num_sighting, y=mf_dur_hr))+
  geom_point(size = 3)+
  geom_smooth(method = 'lm')+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1), 
  #legend.position="bottom")+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(text = element_text(size=20))+
  ylab('Mid-frequency Call Rate (call/hour)')+
  xlab('Number of whales sighted')

plt

ggsave(plt, filename = paste0('figures/question_6_mfcall_rate.png'), 
       height = 7, width = 10, units = 'in', dpi = 300)

# Feeding behaviour with sightings 
lm = lm(df$feed~df$num_sighting)
anova(lm)
summary(lm)

plt = df %>%
  ggplot(aes(x=num_sighting, y=feed))+
  geom_point(size = 3)+
  geom_smooth(method = 'lm')+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1), 
  #legend.position="bottom")+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(text = element_text(size=20))+
  ylab('Number of times feeding was observed')+
  xlab('Number of whales sighted')

plt

# social behaviour with sightings 
lm = lm(df$social~df$num_sighting)
anova(lm)
summary(lm)

plt = df %>%
  ggplot(aes(x=num_sighting, y=social))+
  geom_point(size = 3)+
  geom_smooth(method = 'lm')+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1), 
  #legend.position="bottom")+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(text = element_text(size=20))+
  ylab('Number of times socaliing was observed')+
  xlab('Number of whales sighted')

plt

# other behaviour with sightings 
lm = lm(df$other_behv~df$num_sighting)
anova(lm)
summary(lm)

plt = df %>%
  ggplot(aes(x=num_sighting, y=other_behv))+
  geom_point(size = 3)+
  geom_smooth(method = 'lm')+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1), 
  #legend.position="bottom")+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(text = element_text(size=20))+
  ylab('Number of times other behaviour was observed')+
  xlab('Number of whales sighted')

plt

# male with sightings 
lm = lm(df$sum_male~df$num_sighting)
anova(lm)
summary(lm)

plt = df %>%
  ggplot(aes(x=num_sighting, y=sum_male))+
  geom_point(size = 3)+
  geom_smooth(method = 'lm')+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1), 
  #legend.position="bottom")+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(text = element_text(size=20))+
  ylab('Number of times males were observed')+
  xlab('Number of whales sighted')

plt

# female with sightings 
lm = lm(df$sum_female~df$num_sighting)
anova(lm)
summary(lm)

plt = df %>%
  ggplot(aes(x=num_sighting, y=sum_female))+
  geom_point(size = 3)+
  geom_smooth(method = 'lm')+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1), 
  #legend.position="bottom")+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(text = element_text(size=20))+
  ylab('Number of times females were observed')+
  xlab('Number of whales sighted')

plt

# adult with sightings 
lm = lm(df2$sum_adult~df$num_sighting)
anova(lm)
summary(lm)

plt = df2 %>%
  ggplot(aes(x=num_sighting, y=sum_adult))+
  geom_point(size = 3)+
  geom_smooth(method = 'lm')+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1), 
  #legend.position="bottom")+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(text = element_text(size=20))+
  ylab('Number of times adults were observed')+
  xlab('Number of whales sighted')

plt

# juvenile with sightings
lm = lm(df$sum_juvenile~df$num_sighting)
anova(lm)
summary(lm)

plt = df %>%
  ggplot(aes(x=num_sighting, y=sum_juvenile))+
  geom_point(size = 3)+
  geom_smooth(method = 'lm')+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1), 
  #legend.position="bottom")+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(text = element_text(size=20))+
  ylab('Number of times juveniles were observed')+
  xlab('Number of whales sighted')

plt

# change with male/female
lm = lm(df$sum_calls_dur*60*60/df$num_sighting~df$sum_male)
anova(lm)
summary(lm)
lm = lm(df$gs_dur_hr/df$num_sighting~df$sum_male)
anova(lm)
summary(lm)
lm = lm(df$mf_dur_hr/df$num_sighting~df$sum_male)
anova(lm)
summary(lm)
lm = lm(df$up_dur_hr/df$num_sighting~df$sum_male)
anova(lm)
summary(lm)
lm = lm(df$feed~df$sum_male)
anova(lm)
summary(lm)
lm = lm(df$social~df$sum_male)
anova(lm)
summary(lm)
lm = lm(df$other_behv~df$sum_male)
anova(lm)
summary(lm)

lm = lm(df$sum_calls_dur*60*60/df$num_sighting~df$sum_female)
anova(lm)
summary(lm)
lm = lm(df$gs_dur_hr/df$num_sighting~df$sum_female)
anova(lm)
summary(lm)
lm = lm(df$mf_dur_hr/df$num_sighting~df$sum_female)
anova(lm)
summary(lm)
lm = lm(df$up_dur_hr/df$num_sighting~df$sum_female)
anova(lm)
summary(lm)
lm = lm(df$feed~df$sum_female)
anova(lm)
summary(lm)
lm = lm(df$social~df$sum_female)
anova(lm)
summary(lm)
lm = lm(df$other_behv~df$sum_female)
anova(lm)
summary(lm)

# change with adult/juvenile
lm = lm(df2$sum_calls_dur*60*60/df$num_sighting~df2$sum_adult)
anova(lm)
summary(lm)
lm = lm(df2$gs_dur_hr/df$num_sighting~df2$sum_adult)
anova(lm)
summary(lm)
lm = lm(df2$mf_dur_hr/df$num_sighting~df2$sum_adult)
anova(lm)
summary(lm)
lm = lm(df2$up_dur_hr/df$num_sighting~df2$sum_adult)
anova(lm)
summary(lm)
lm = lm(df2$feed~df2$sum_adult)
anova(lm)
summary(lm)
lm = lm(df2$social~df2$sum_adult)
anova(lm)
summary(lm)
lm = lm(df2$other_behv~df2$sum_adult)
anova(lm)
summary(lm)

lm = lm(df$sum_calls_dur*60*60/df$num_sighting~df$sum_juvenile)
anova(lm)
summary(lm)
lm = lm(df$gs_dur_hr/df$num_sighting~df$sum_juvenile)
anova(lm)
summary(lm)
lm = lm(df$mf_dur_hr/df$num_sighting~df$sum_juvenile)
anova(lm)
summary(lm)
lm = lm(df$up_dur_hr/df$num_sighting~df$sum_juvenile)
anova(lm)
summary(lm)
lm = lm(df$feed~df$sum_juvenile)
anova(lm)
summary(lm)
lm = lm(df$social~df$sum_juvenile)
anova(lm)
summary(lm)
lm = lm(df$other_behv~df$sum_juvenile)
anova(lm)
summary(lm)

# change calls with behaviour 
lm = lm(df$sum_calls_dur*60*60/df$num_sighting~df$feed)
anova(lm)
summary(lm)
lm = lm(df$gs_dur_hr/df$num_sighting~df$feed)
anova(lm)
summary(lm)
lm = lm(df$mf_dur_hr/df$num_sighting~df$feed)
anova(lm)
summary(lm)
lm = lm(df$up_dur_hr/df$num_sighting~df$feed)
anova(lm)
summary(lm)

lm = lm(df$sum_calls_dur*60*60/df$num_sighting~df$social)
anova(lm)
summary(lm)
lm = lm(df$gs_dur_hr/df$num_sighting~df$social)
anova(lm)
summary(lm)
lm = lm(df$mf_dur_hr/df$num_sighting~df$social)
anova(lm)
summary(lm)
lm = lm(df$up_dur_hr~df$social)
anova(lm)
summary(lm)

lm = lm(df$sum_calls_dur*60*60/df$num_sighting~df$other_behv)
anova(lm)
summary(lm)
lm = lm(df$gs_dur_hr/df$num_sighting~df$other_behv)
anova(lm)
summary(lm)
lm = lm(df$mf_dur_hr/df$num_sighting~df$other_behv)
anova(lm)
summary(lm)
lm = lm(df$up_dur_hr/df$num_sighting~df$other_behv)
anova(lm)
summary(lm)

t = hist(df$num_sighting)
t

lm = lm(df$yday~df$num_sighting)
p = shapiro.test(residuals(lm))
p
