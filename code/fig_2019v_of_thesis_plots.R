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
  select("yday", "up_dur_hr", "mf_dur_hr", "gs_dur_hr") %>%
  mutate(date = as.Date(yday, origin = '2018-01-01')) %>% 
  gather(key = "call_type" , value = "cval", up_dur_hr, mf_dur_hr, gs_dur_hr)
# plot
plt = ggplot(data = dfp, aes(x=date,y=cval,fill=call_type))+
  geom_col(position = "stack")+
  
  # remove white space around plot
  scale_y_continuous(expand = c(0,0))+
  
  # specify weekly axis labels
  scale_x_date(breaks = '1 week', date_labels = '%b-%d')+
  
  # make pretty
  scale_fill_grey(start = 0.1, end = 0.7,  
                  breaks=c("up_dur_hr", "mf_dur_hr", "gs_dur_hr"),
                  labels=c("Upcall", "Mid-frequency","Gunshot"))+
  labs(fill = "Call Type",
       y = 'Call Rate (call/hour)',
       x = NULL)+
  theme_bw()+
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, colour = 'black'),
        axis.text.y = element_text(colour = 'black'),
        text = element_text(size=14, family = "serif"))

plt
ggsave(plt, filename = paste0('figures/call_rate_category_2019_UPDATED.png'), 
       height = 5, width = 8, units = 'in', dpi = 300)


# call rate by week
# total 
Tplt = df %>%
  mutate(call_rate = sum_calls_dur*60*60) %>%
  ggplot()+
  geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = call_rate), 
             color = 'black', shape = 1, size = 3)+
  # geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = call_rate), method = "lm", color='black',
  #             se = TRUE, fullrange = TRUE)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  # remove white space around plot
  scale_y_continuous(expand = c(0,10))+
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
       title = 'Total call rate')+ #(expression(paste('a) Total call rate, ',rho, ' = 0.549'))))+
  ylab('Call Rate (call/hour)')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())
#axis.ticks.x=element_blank())
#xlab('Week')
Tplt

# up call rate
Uplt = df %>%
  ggplot()+
  geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = up_dur_hr),
             color = 'black', shape = 1, size = 3)+
  # geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = up_dur_hr), method = "lm", color='black',
  #             se = TRUE, fullrange = TRUE)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,0.5))+
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
       title = 'Upcall call rate')+ #(expression(paste('d) Upcall call rate, ',rho, ' = 0.146'))))+
  ylab('Call Rate (call/hour)')+
  xlab('Time (Week)')

Uplt

# gs call rate
Gplt = df %>%
  ggplot()+
  geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = gs_dur_hr), 
             color = 'black', shape = 1, size = 3)+
  # geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = gs_dur_hr), method = "lm", color='black',
  #             se = TRUE, fullrange = TRUE)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,2))+
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
       title = 'Gunshot call rate')+ #(expression(paste('b) Gunshot call rate, ',rho, ' = 0.659'))))+
  ylab('Call Rate (call/hour)')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())

Gplt

# mf call rate
Mplt = df %>%
  ggplot()+
  geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = mf_dur_hr), 
             color = 'black', shape = 1, size = 3)+
  # geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = mf_dur_hr), method = "lm", color='black',
  #             se = TRUE, fullrange = TRUE)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,10))+
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
       title = 'Mid-frequency call rate')+ #(expression(paste('c) Mid-frequency call rate, ',rho, ' = 0.671'))))+
  ylab('Call Rate (call/hour)')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())
#xlab('Week')
Mplt

png(paste0(fig_dir, 'calls_rates_all_2019_UPDATED.png'), height = 12, width = 7,
    units = 'in', res = 300)

grid.newpage()
grid.draw(rbind(ggplotGrob(Tplt), 
                ggplotGrob(Gplt), 
                ggplotGrob(Mplt), 
                ggplotGrob(Uplt), size = "last"))

dev.off()

# demographics bar graph
dfp = df %>%
  select("yday", "juvenile_male", "juvenile_female", 
         "adult_male", "adult_female") %>%
  mutate(date = as.Date(yday, origin = '2018-01-01')) %>% 
  gather(key = "age_sex" , value = "cval", juvenile_male, juvenile_female, adult_male, adult_female)

# plot
pltd = ggplot(data = dfp, aes(x=date,y=cval,fill=age_sex))+
  geom_col(position = "stack")+
  
  # remove white space around plot
  scale_y_continuous(expand = c(0,0))+
  
  # specify weekly axis labels
  scale_x_date(breaks = '1 week', date_labels = '%b-%d')+
  
  # make pretty
  scale_fill_grey(start = 0.1, end = 0.7,  
                  breaks=c("juvenile_male", "juvenile_female", "adult_male", "adult_female"),
                  labels=c("Juvenile, Male", "Juvenile, Female","Adult, Male", "Adult, Female"))+
  labs(fill = "Age Class, Sex",
       y = 'Sighted Whales',
       x = NULL)+
  theme_bw()+
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, colour = 'black'),
        axis.text.y = element_text(colour = 'black'),
        text = element_text(size=14, family = "serif"))

pltd
ggsave(pltd, filename = paste0('figures/demogrpahics_category_2019_UPDATED.png'), 
       height = 5, width = 8, units = 'in', dpi = 300)


# behaviour stacked bar graph
# wrangle plot data (coercing all data to the same year makes plotting much easier)
dfp = df %>%
  select("yday", "foraging", "social", "other_bhv",
  "mom_calf","entg_eff") %>%
  mutate(date = as.Date(yday, origin = '2018-01-01')) %>% 
  gather(key = "behaviour" , value = "cval", foraging, social, other_bhv,
         mom_calf,entg_eff)

# plot
pltb = ggplot(data = dfp, aes(x=date,y=cval,fill=behaviour))+
  geom_col(position = "stack")+
  
  # remove white space around plot
  scale_y_continuous(expand = c(0,0))+
  
  # specify weekly axis labels
  scale_x_date(breaks = '1 week', date_labels = '%b-%d')+
  
  # make pretty
  scale_fill_grey(start = 0.1, end = 0.7,  
                  breaks=c("foraging", "social_notsag", "social_sag", "other_bhv",
                           "mom_calf","entg_eff"),
                  labels=c("Foraging", "Social non-SAG", "Social SAG", "Other Behaviour",
                           "Mom and Calf","Enganglement"))+
  labs(fill = "Behaviour",
       y = 'Sighted Whale Behaviour',
       x = NULL)+
  theme_bw()+
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, colour= 'black'),
        axis.text.y = element_text(colour = 'black'),
        text = element_text(size=14, family = "serif"))

pltb
ggsave(pltb, filename = paste0('figures/behaviour_category_2019_UPDATED.png'), 
       height = 5, width = 8, units = 'in', dpi = 300)




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
  ylab('Number of individuals')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  labs(x = NULL,
       y = NULL,
       title = 'Sex') #(expression(paste('a) Sex, Male ',rho, ' = 0.144, ', 
                                 #'Female ', rho, ' = 0.0161'))))
#xlab('Week')
pltsex

df2=df%>%
  mutate(sum_adult=adult_female+adult_male)
pltage = df2 %>%
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
  scale_y_continuous(expand = c(0,1))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  labs(x = NULL,
       y = NULL,
       title = 'Age')+ #(expression(paste('b) Age, Adult ',rho, ' = 0.0622, ', 
                                 #'Juvenile ', rho, ' = 0.288'))))+
  theme(text = element_text(size=14, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  ylab('Number of individuals')
#xlab('Time (Week)')

pltage

png(paste0(fig_dir, 'vsual_all_2019_UPDATED.png'), height = 10, width = 8,
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
  geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = foraging),
             color = 'black', shape = 1, size = 3)+
  # geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = feed), method = "lm", color='black',
  #             se = TRUE, fullrange = TRUE)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,0.5))+
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
       title = 'Foraging')+ #(expression(paste('a) Feeding behaviour, ',rho, ' = -0.378'))))+
  ylab('Behaviour Occurance')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())
#xlab('Week')
fplt

# social
splt = df %>%
  ggplot()+
  geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = social), 
             color = 'black', shape = 1, size = 3)+
  # geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = social), method = "lm", color='black',
  #             se = TRUE, fullrange = TRUE)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,0.1))+
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
       title = 'Social')+ #(expression(paste('b) Socalizing behaviour, ',rho, ' = 0.533'))))+
  ylab('Behaviour Occurance')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())
#xlab('Week')
splt

obplt = df %>%
  ggplot()+
  geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = other_bhv), 
             shape = 1, color = 'black', size = 3)+
  # geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = other_behv), method = "lm", color='black',
  #             se = TRUE, fullrange = TRUE)+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,0.15))+
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
       title = 'Other behaviour')+ #(expression(paste('c) Other behaviour, ',rho, ' = 0.219'))))+
  ylab('Behaviour Occurance')
#xlab('Time (Week)')
obplt

png(paste0(fig_dir, 'bheaviours_regressions_2019_UPDATED.png'), height = 8, width = 6,
    units = 'in', res = 300)

grid.newpage()
grid.draw(rbind(ggplotGrob(fplt), 
                ggplotGrob(splt), 
                ggplotGrob(obplt), 
                size = "last"))

dev.off()

# # non sag
# nsplt = df %>%
#   ggplot()+
#   geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = social_notsag), 
#              shape = 1, color = 'black', size = 3)+
#   # geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = other_behv), method = "lm", color='black',
#   #             se = TRUE, fullrange = TRUE)+
#   scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
#   scale_y_continuous(expand = c(0,0.15))+
#   theme_bw()+
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position="bottom")+
#   theme(text = element_text(size=12, family = "serif"))+
#   theme(axis.text.y = element_text(colour = "black"))+
#   theme(axis.text.x = element_text(colour = "black"))+
#   labs(x = NULL,
#        y = NULL,
#        title = 'Social - Not SAG')+ #(expression(paste('c) Other behaviour, ',rho, ' = 0.219'))))+
#   ylab('Behaviour Occurance')
# #xlab('Time (Week)')
# nsplt
# 
# # sag
# ssplt = df %>%
#   ggplot()+
#   geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = social_sag), 
#              shape = 1, color = 'black', size = 3)+
#   # geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = other_behv), method = "lm", color='black',
#   #             se = TRUE, fullrange = TRUE)+
#   scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
#   scale_y_continuous(expand = c(0,0.15))+
#   theme_bw()+
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position="bottom")+
#   theme(text = element_text(size=12, family = "serif"))+
#   theme(axis.text.y = element_text(colour = "black"))+
#   theme(axis.text.x = element_text(colour = "black"))+
#   labs(x = NULL,
#        y = NULL,
#        title = 'Social - Not SAG')+ #(expression(paste('c) Other behaviour, ',rho, ' = 0.219'))))+
#   ylab('Behaviour Occurance')
# #xlab('Time (Week)')
# ssplt
# 
# # engtanglment
# eplt = df %>%
#   ggplot()+
#   geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = entg_eff), 
#              shape = 1, color = 'black', size = 3)+
#   # geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = other_behv), method = "lm", color='black',
#   #             se = TRUE, fullrange = TRUE)+
#   scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
#   scale_y_continuous(expand = c(0,0.15))+
#   theme_bw()+
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position="bottom")+
#   theme(text = element_text(size=12, family = "serif"))+
#   theme(axis.text.y = element_text(colour = "black"))+
#   theme(axis.text.x = element_text(colour = "black"))+
#   labs(x = NULL,
#        y = NULL,
#        title = 'Entanglement effort')+ #(expression(paste('c) Other behaviour, ',rho, ' = 0.219'))))+
#   ylab('Behaviour Occurance')
# #xlab('Time (Week)')
# eplt
# 
# # mom calf
# mcplt = df %>%
#   ggplot()+
#   geom_point(aes(x = as.Date(yday(date),'1970-01-01'), y = mom_calf), 
#              shape = 1, color = 'black', size = 3)+
#   # geom_smooth(aes(x = as.Date(yday(date),'1970-01-01'), y = other_behv), method = "lm", color='black',
#   #             se = TRUE, fullrange = TRUE)+
#   scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
#   scale_y_continuous(expand = c(0,0.15))+
#   theme_bw()+
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position="bottom")+
#   theme(text = element_text(size=12, family = "serif"))+
#   theme(axis.text.y = element_text(colour = "black"))+
#   theme(axis.text.x = element_text(colour = "black"))+
#   labs(x = NULL,
#        y = NULL,
#        title = 'Mom calf relations')+ #(expression(paste('c) Other behaviour, ',rho, ' = 0.219'))))+
#   ylab('Behaviour Occurance')
# #xlab('Time (Week)')
# mcplt
# 
# png(paste0(fig_dir, 'bheaviours_regressions_2019ALL.png'), height = 18, width = 7,
#     units = 'in', res = 300)
# 
# grid.newpage()
# grid.draw(rbind(ggplotGrob(fplt), 
#                 ggplotGrob(splt), 
#                 ggplotGrob(nsplt), 
#                 ggplotGrob(ssplt),
#                 ggplotGrob(mcplt),
#                 ggplotGrob(eplt),
#                 ggplotGrob(obplt),
#                 size = "last"))
# dev.off()
