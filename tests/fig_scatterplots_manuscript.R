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



# call production rate by week
# total 
Tplt = df %>%
  mutate(call_prate = sum_calls/((rec_duration)/60/60)/num_sighting) %>%
  ggplot()+
  geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = call_prate), 
             color = 'black', shape = 1, size = 3)+
  # geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = call_rate), method = "lm", color='black',
  #             se = TRUE, fullrange = TRUE)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  # remove white space around plot
  scale_y_continuous(expand = c(0,NA))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="bottom")+
  theme(text = element_text(size=12, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  labs(x = NULL,
       y = NULL,
       title = 'a) Total call production rate')+ #(expression(paste('a) Total call rate, ',rho, ' = 0.549'))))+
  ylab('Call Production Rate (call/hour/whale)')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())
#axis.ticks.x=element_blank())
#xlab('Week')
Tplt

# up call rate
Uplt = df %>%
  ggplot()+
  geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = up_per_hr_per_whale),
             color = 'black', shape = 1, size = 3)+
  # geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = up_dur_hr), method = "lm", color='black',
  #             se = TRUE, fullrange = TRUE)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,NA))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="bottom")+
  theme(text = element_text(size=12, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  labs(x = NULL,
       y = NULL,
       title = 'd) Upcall call production rate')+ #(expression(paste('d) Upcall call rate, ',rho, ' = 0.146'))))+
  ylab('Call Production Rate (call/hour/whale)')+
  xlab('Time (Week)')
Uplt

# gs call rate
Gplt = df %>%
  ggplot()+
  geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = gs_per_hr_per_whale), 
             color = 'black', shape = 1, size = 3)+
  # geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = gs_dur_hr), method = "lm", color='black',
  #             se = TRUE, fullrange = TRUE)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,NA))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  # theme(axis.text.x = element_text(angle = 45, hjust = 1),
  #      legend.position="bottom")+
  theme(text = element_text(size=12, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  labs(x = NULL,
       y = NULL,
       title = 'b) Gunshot call production rate')+ #(expression(paste('b) Gunshot call rate, ',rho, ' = 0.659'))))+
  ylab('Call Production Rate (call/hour/whale)')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())
Gplt

# mf call rate
Mplt = df %>%
  ggplot()+
  geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = mf_per_hr_per_whale), 
             color = 'black', shape = 1, size = 3)+
  # geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = mf_per_hr_per_whale), method = "lm", color='black',
  #             se = TRUE, fullrange = TRUE)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,NA))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="bottom")+
  theme(text = element_text(size=12, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  labs(x = NULL,
       y = NULL,
       title = 'c) Mid-frequency call production rate')+ #(expression(paste('c) Mid-frequency call rate, ',rho, ' = 0.671'))))+
  ylab('Call Production Rate (call/hour/whale)')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())
#xlab('Week')
Mplt

png(paste0(fig_dir, 'calls_p_rates_scatterplot.png'), height = 12, width = 7,
    units = 'in', res = 300)

grid.newpage()
grid.draw(rbind(ggplotGrob(Tplt), 
                ggplotGrob(Gplt), 
                ggplotGrob(Mplt), 
                ggplotGrob(Uplt), size = "last"))

dev.off()

# demogrpahics regressions
pltsex = df %>%
  select("yday", "sum_female", "sum_male") %>%
  gather(key = "sex" , value = "cval", sum_female, sum_male) %>%
  filter(sex %in% c('sum_female', 'sum_male')) %>%
  ggplot(aes(x = as.Date(yday,origin = '1970-01-01'), y = cval))+
  geom_point(aes(colour = sex), alpha = 0.5, size = 3)+
  # geom_smooth(aes(color = sex, fill = sex), method = "lm",
  #             se = TRUE, fullrange = TRUE, show.legend = FALSE)+
  scale_colour_manual(values = c("red", "blue"),
                      breaks=c("sum_female", "sum_male"),
                      labels=c("Female", "Male"))+
  labs(colour = "Sex",
       x = NULL,
       y = NULL)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,NA))+
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
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  labs(x = NULL,
       y = 'Number of individuals',
       title = 'a) Sex') #(expression(paste('a) Sex, Male ',rho, ' = 0.144, ', 
                                 #'Female ', rho, ' = 0.0161'))))
#xlab('Week')
pltsex

pltage = df %>%
  select("yday", "sum_juvenile", "sum_adult") %>%
  gather(key = "age" , value = "cval", sum_juvenile, sum_adult) %>%
  filter(age %in% c('sum_juvenile', 'sum_adult')) %>%
  ggplot(aes(x = as.Date(yday,'1970-01-01'), y = cval))+
  geom_point(aes(colour = age), size = 3, alpha = 0.5)+
  # geom_smooth(aes(color = age, fill = age), method = "lm",
  #             se = TRUE, fullrange = TRUE, show.legend = FALSE)+
  scale_colour_manual(values = c("red", "blue"),
                      breaks=c("sum_juvenile", "sum_adult"),
                      labels=c("Juvenile", "Adult"))+
  labs(colour = "Age",
       x = NULL,
       y = NULL)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,NA))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  labs(x = NULL,
       y = NULL,
       title = 'b) Age')+ #(expression(paste('b) Age, Adult ',rho, ' = 0.0622, ', 
                                 #'Juvenile ', rho, ' = 0.288'))))+
  theme(text = element_text(size=14, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  ylab('Number of individuals')
#xlab('Time (Week)')
pltage

png(paste0(fig_dir, 'demo_scatterplot.png'), height = 10, width = 8,
    units = 'in', res = 300)

grid.newpage()
grid.draw(rbind(ggplotGrob(pltsex), 
                ggplotGrob(pltage),  
                size = "last"))
dev.off()


# behaviour regressions
# feed
fplt = df %>%
  ggplot()+
  geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = foraging_bhv_whale),
             color = 'black', shape = 1, size = 3)+
  # geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = feed), method = "lm", color='black',
  #             se = TRUE, fullrange = TRUE)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,NA))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  # theme(axis.text.x = element_text(angle = 45, hjust = 1),
  #      legend.position="bottom")+
  theme(text = element_text(size=12, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  labs(x = NULL,
       y = NULL,
       title = 'a) Foraging behaviour rate')+ #(expression(paste('a) Feeding behaviour, ',rho, ' = -0.378'))))+
  ylab('Behaviour Rate')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())
#xlab('Week')
fplt

# social
splt = df %>%
  ggplot()+
  geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = social_bhv_whale), 
             color = 'black', shape = 1, size = 3)+
  # geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = social), method = "lm", color='black',
  #             se = TRUE, fullrange = TRUE)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,NA))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  # theme(axis.text.x = element_text(angle = 45, hjust = 1),
  #      legend.position="bottom")+
  theme(text = element_text(size=12, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  labs(x = NULL,
       y = NULL,
       title = 'b) Social behaviour rate')+ #(expression(paste('b) Socalizing behaviour, ',rho, ' = 0.533'))))+
  ylab('Behaviour Rate')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())
#xlab('Week')
splt

obplt = df %>%
  ggplot()+
  geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = other_bhv_whale), 
             shape = 1, color = 'black', size = 3)+
  # geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = other_behv), method = "lm", color='black',
  #             se = TRUE, fullrange = TRUE)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,NA))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="bottom")+
  theme(text = element_text(size=12, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  labs(x = NULL,
       y = NULL,
       title = 'c) Other behaviour rate')+ #(expression(paste('c) Other behaviour, ',rho, ' = 0.219'))))+
  ylab('Behaviour Rate')
#xlab('Time (Week)')
obplt

png(paste0(fig_dir, 'behaviours_scatterplot.png'), height = 8, width = 6,
    units = 'in', res = 300)

grid.newpage()
grid.draw(rbind(ggplotGrob(fplt), 
                ggplotGrob(splt), 
                ggplotGrob(obplt), 
                size = "last"))

dev.off()
