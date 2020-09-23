## fig_calls_facet_yr_plots.R ##


# libraries
library(tidyverse)
library(ggplot2)

df1 = readRDS("data/processed/proc_acou_photoid.rds")

pltc = df1 %>%
  select("yday", "year", "up_per_hr", "mf_per_hr", "gs_per_hr") %>%
  gather(key = "call_type" , value = "cval", up_per_hr, mf_per_hr, gs_per_hr) %>%
  filter(call_type %in% c('up_per_hr', 'mf_per_hr', 'gs_per_hr')) %>%
  ggplot(aes(x = as.Date(yday,'1970-01-01'), y = cval, fill = call_type))+
  geom_bar(position="stack", stat = 'identity', width = 2)+
  scale_fill_grey(start = 0, end = .9,  
                  breaks=c("up_per_hr", "mf_per_hr", "gs_per_hr"),
                  labels=c("Upcall", "Mid-frequency","Gunshot"))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(fill = "Call Type",
       x = NULL,
       y = NULL,
       title = 'Calls detected per deployment duration')+
  # facet by year
  facet_wrap(~year)+
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

pltc = df1 %>%
  select("yday", "year", "up_per_hr_per_whale", "mf_per_hr_per_whale", "gs_per_hr_per_whale") %>%
  gather(key = "call_type" , value = "cval", up_per_hr_per_whale, mf_per_hr_per_whale, gs_per_hr_per_whale) %>%
  filter(call_type %in% c('up_per_hr_per_whale', 'mf_per_hr_per_whale', 'gs_per_hr_per_whale')) %>%
  ggplot(aes(x = yday, y = cval, fill = call_type))+
  geom_bar(position="stack", stat = 'identity', width = 2)+
  scale_fill_grey(start = 0, end = .9,  
                  breaks=c("up_per_hr_per_whale", "mf_per_hr_per_whale", "gs_per_hr_per_whale"),
                  labels=c("Upcall", "Mid-frequency","Gunshot"))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(fill = "Call Type",
       x = NULL,
       y = NULL,
       title = 'Calls detected per deployment duration per whale')+
  # facet by year
  facet_wrap(~year)+
  ylab('Call Rate (call/hour/whale)')+
  xlab('yday')+
  #scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  #scale_y_continuous(breaks = seq(0,275,50))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  #theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  theme(text = element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))
pltc

pltc = df1 %>%
  select("yday", "year", "up_per_hr", "mf_per_hr", "gs_per_hr") %>%
  gather(key = "call_type" , value = "cval", up_per_hr, mf_per_hr, gs_per_hr) %>%
  filter(call_type %in% c('up_per_hr', 'mf_per_hr', 'gs_per_hr')) %>%
  ggplot(aes(x = yday, y = cval, fill = call_type))+
  geom_bar(position="stack", stat = 'identity', width = 2)+
  scale_fill_grey(start = 0, end = .9,  
                  breaks=c("up_per_hr", "mf_per_hr", "gs_per_hr"),
                  labels=c("Upcall", "Mid-frequency","Gunshot"))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(fill = "Call Type",
       x = NULL,
       y = NULL,
       title = 'Calls detected per delpoyment duration')+
  # facet by year
  facet_wrap(~year)+
  ylab('Call Rate (call/hour)')+
  xlab('yday')+
  #scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  #scale_y_continuous(breaks = seq(0,275,50))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  #theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  theme(text = element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))
pltc

pltc = df1 %>%
  select("yday", "year", "up", "mf", "gs") %>%
  gather(key = "call_type" , value = "cval", up, mf, gs) %>%
  filter(call_type %in% c('up', 'mf', 'gs')) %>%
  ggplot(aes(x = yday, y = cval, fill = call_type))+
  geom_bar(position="stack", stat = 'identity', width = 2)+
  scale_fill_grey(start = 0, end = .9,  
                  breaks=c("up", "mf", "gs"),
                  labels=c("Upcall", "Mid-frequency","Gunshot"))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(fill = "Call Type",
       x = NULL,
       y = NULL,
       title = 'Calls detected')+
  # facet by year
  facet_wrap(~year)+
  ylab('Call')+
  xlab('yday')+
  #scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  #scale_y_continuous(breaks = seq(0,275,50))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  #theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  theme(text = element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))
pltc
