## thesis_test_figures

# figure directory
fig_dir = 'figures/'

# define input data file
ifile = readRDS("data/processed/sight_dfs.rds")
df= ifile

# libraries 
library(tidyverse)
library(viridis)
library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(oce)


# THE BAR GRAPH EXAMPLE ---------------------------------------------------
# call rate by week
plt = df %>%
  select(as.numeric("yday"), "year", "up_dur_hr", "mf_dur_hr", "gs_dur_hr") %>%
  gather(key = "call_type" , value = "cval", up_dur_hr, mf_dur_hr, gs_dur_hr) %>%
  filter(call_type %in% c('up_dur_hr', 'mf_dur_hr', 'gs_dur_hr')) %>%
  ggplot(aes(x = as.Date(yday(date),'1970-01-01'), y = cval, fill = call_type))+
  geom_bar(position="stack", stat = 'identity', width = 2, colour = 'black')+
  geom_col(position = position_dodge2(padding = 0, width = 2, preserve = "single"))+
  scale_fill_grey(start = 0.1, end = 0.7,  
                  breaks=c("up_dur_hr", "mf_dur_hr", "gs_dur_hr"),
                  labels=c("Upcall", "Mid-frequency","Gunshot"))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(fill = "Call Type",
       x = NULL,
       y = NULL)+
  #title = 'Calls detected per duration of deployment')+
  # facet by year
  #facet_wrap(~year)+
  ylab('Call Rate (call/hour)')+
  xlab('Time (Week)')+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(breaks = seq(0,275,50))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  #theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  theme(text = element_text(size=14, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))
plt

ggsave(plt, filename = paste0('figures/call_rate_category_ugly.png'), 
       height = 5, width = 8, units = 'in', dpi = 300)


# THE SCATTER PLOT EXAMPLE ------------------------------------------------
# up call rate
Uplt = df %>%
  ggplot()+
  geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = up_dur_hr), color = 'black', size = 3)+
  geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = up_dur_hr), method = "lm", color='black',
              se = TRUE, fullrange = TRUE)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="bottom")+
  theme(text = element_text(size=12, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  #limits=c(0,max(toPlot$value))+
  #expand_limits(y=0)+
  #ylim(0,range)+
  #scale_y_continuous(expand = c(0,0))+
  labs(x = NULL,
       y = NULL,
       title = 'd) Upcall call rate')+
  ylab('Call Rate (call/hour)')+
  xlab('Time (Week)')

Uplt

