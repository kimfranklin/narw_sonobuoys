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
  scale_colour_manual(values = c("red", "darkred","black"),
                      breaks=c("up_per_hr", "gs_per_hr", "mf_per_hr"),
                      labels=c("Upcall", "Gunshot","Tonal"))+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  #scale_y_continuous(expand = c(0,1))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  # theme(axis.text.x = element_text(angle = 45, hjust = 1), 
  #       legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  theme(text = element_text(size=12, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  labs(x = NULL,
       y = 'Rate (call/h)\n',
       colour = "Call Type",
       title = (expression(paste('a) Call rates'))))+
  geom_hline(yintercept=0
             #,linetype = 'dashed' 
             #,size = 0.1
  )
             
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
  scale_colour_manual(values = c("darkorange", "khaki3"),
                      breaks=c("foraging_bhv_whale", "social_bhv_whale"),
                      labels=c("Foraging", "Socalizing"))+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  #scale_y_continuous(expand = c(0,0.0025))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  # theme(axis.text.x = element_text(angle = 45, hjust = 1), 
  #       legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  theme(text = element_text(size=12, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  labs(x = NULL,
       y = 'Behavior rate (behavior/whale)\n',
       colour = "Behavior",
       title = (expression(paste('b) Behavior rates'))))+
  geom_hline(yintercept=0 
             #,linetype = 'dashed'
             )
pltb

# whale abundance scatterplot
pltc = df %>%
  select("yday", 'year', "num_sighting", "ratio_male_female") %>%
  # gather(key = "dem" , value = "cval", num_sighting) %>%
  # filter(dem %in% c("num_sighting")) %>%
  ggplot(aes(x = as.Date(yday,'1970-01-01'), y = num_sighting, colour = cut(ratio_male_female,c(0, 1, 2, 3, 4))))+
  geom_point(#aes(colour = 'red'), 
    #color = ratio_male_female, 
    #alpha = 0.5, 
    size = 3)+
  scale_colour_brewer(palette = "Blues")+
  # geom_smooth(aes(color = sex, fill = sex), method = "lm",
  #             se = TRUE, fullrange = TRUE, show.legend = FALSE)+
  # scale_colour_manual(values = c("red"),
  #                     breaks=c("num_sighting"),
  #                     labels=c("Whale abundance"))+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,1))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  theme(text = element_text(size=12, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  # theme(axis.title.x=element_blank(),
  #       axis.text.x=element_blank())+
  labs(x = 'Week',
       y = 'Number of Whales\n',
       colour = "Male/Female Ratio",
       title = (expression(paste('c) Whale Abundance'))))
  # geom_hline(yintercept=0
  #            #,linetype = 'dashed'
  #            )
pltc


png(paste0(fig_dir, 'manuscript_scatterplots_new.png'), height = 9, width = 7,
    units = 'in', res = 300)

grid.newpage()
grid.draw(rbind(ggplotGrob(plta), 
                ggplotGrob(pltb),
                ggplotGrob(pltc), size = "last"))

dev.off()





# add year as shape in the scatterplots
# call rate scatterplot
plta = df %>%
  select("yday", 'year', "up_per_hr", "gs_per_hr", "mf_per_hr") %>%
  gather(key = "call" , value = "cval", up_per_hr, gs_per_hr, mf_per_hr) %>%
  filter(call %in% c("up_per_hr", "gs_per_hr", "mf_per_hr")) %>%
  ggplot(aes(x = as.Date(yday,'1970-01-01'), y = cval))+
  geom_point(aes(colour = call, shape = as.character(year)), alpha = 0.5, size = 3)+
  # geom_smooth(aes(color = sex, fill = sex), method = "lm",
  #             se = TRUE, fullrange = TRUE, show.legend = FALSE)+
  scale_colour_manual(values = c("red", "darkred","black"),
                      breaks=c("up_per_hr", "gs_per_hr", "mf_per_hr"),
                      labels=c("Upcall", "Gunshot","Tonal"))+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  #scale_y_continuous(expand = c(0,1))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  # theme(axis.text.x = element_text(angle = 45, hjust = 1), 
  #       legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  theme(text = element_text(size=12, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  labs(x = NULL,
       y = 'Rate (call/h)\n',
       colour = "Call Type",
       shape = 'Year',
       title = (expression(paste('a) Call rates'))))+
  geom_hline(yintercept=0
             #,linetype = 'dashed' 
             #,size = 0.1
  )
plta

# behaviour rate scatterplot
pltb = df %>%
  select("yday", 'year', "foraging_bhv_whale", "social_bhv_whale") %>%
  gather(key = "bhv" , value = "cval", foraging_bhv_whale, social_bhv_whale) %>%
  filter(bhv %in% c("foraging_bhv_whale", "social_bhv_whale")) %>%
  ggplot(aes(x = as.Date(yday,'1970-01-01'), y = cval))+
  geom_point(aes(colour = bhv, shape = as.character(year)), alpha = 0.5, size = 3)+
  # geom_smooth(aes(color = sex, fill = sex), method = "lm",
  #             se = TRUE, fullrange = TRUE, show.legend = FALSE)+
  scale_colour_manual(values = c("darkorange", "khaki3"),
                      breaks=c("foraging_bhv_whale", "social_bhv_whale"),
                      labels=c("Foraging", "Socalizing"))+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  #scale_y_continuous(expand = c(0,0.0025))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  # theme(axis.text.x = element_text(angle = 45, hjust = 1), 
  #       legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  theme(text = element_text(size=12, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  labs(x = NULL,
       y = 'Behavior rate (behavior/whale)\n',
       colour = "Behavior",
       shape = 'Year',
       title = (expression(paste('b) Behavior rates'))))+
  geom_hline(yintercept=0 
             #,linetype = 'dashed'
  )
pltb

# whale abundance scatterplot
pltc = df %>%
  select("yday", 'year', "num_sighting", "ratio_male_female") %>%
  # gather(key = "dem" , value = "cval", num_sighting) %>%
  # filter(dem %in% c("num_sighting")) %>%
  ggplot(aes(x = as.Date(yday,'1970-01-01'), y = num_sighting, 
             colour = cut(ratio_male_female,c(0, 1, 2, 3, 4)), shape = as.character(year)))+
  geom_point(#aes(colour = 'red'), 
    #color = ratio_male_female, 
    #alpha = 0.5, 
    size = 3)+
  scale_colour_brewer(palette = "Blues")+
  # geom_smooth(aes(color = sex, fill = sex), method = "lm",
  #             se = TRUE, fullrange = TRUE, show.legend = FALSE)+
  # scale_colour_manual(values = c("red"),
  #                     breaks=c("num_sighting"),
  #                     labels=c("Whale abundance"))+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,1))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  theme(text = element_text(size=12, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  # theme(axis.title.x=element_blank(),
  #       axis.text.x=element_blank())+
  labs(x = 'Week',
       y = 'Number of Whales\n',
       colour = "Male/Female Ratio",
       shape = 'Year',
       title = (expression(paste('c) Whale Abundance'))))
# geom_hline(yintercept=0
#            #,linetype = 'dashed'
#            )
pltc


png(paste0(fig_dir, 'manuscript_scatterplots_new_wyear.png'), height = 9, width = 7,
    units = 'in', res = 300)

grid.newpage()
grid.draw(rbind(ggplotGrob(plta), 
                ggplotGrob(pltb),
                ggplotGrob(pltc), size = "last"))

dev.off()









# ggsave(pltc, filename = paste0('figures/manuscript_abundance.png'), 
#         height = 5, width = 7, units = 'in', dpi = 300)

# # demographics bar graph
# df$date_discrete = c('2017-06-27', '2017-06-29', '2017-07-05', '2017-07-08', '2017-07-20', 
#                     '2017-07-21', '2017-07-24', '2017-07-26', '2018-06-06', '2018-06-07', 
#                     '2018-06-11', '2018-06-15', '2018-06-17', '2018-06-26', '2018-06-29', 
#                     '2018-07-19', '2018-07-21', '2018-07-27', '2018-08-03', '2018-08-06', 
#                     '2018-08-12', '2019-06-04', '2019-06-05', '2019-06-07', '2019-06-09', 
#                     '2019-06-10', '2019-06-13', '2019-07-11', '2019-07-16', '2019-07-19 a', 
#                     '2019-07-19 b', '2019-08-14', '2019-08-15', '2019-08-16', '2019-08-21',
#                     '2019-08-25', '2019-08-26')
# 
# dfp = df %>%
#   select("date_discrete", "adult_female","adult_male","calf_female","calf_male",
#           "juvenile_female","juvenile_male","unknown") %>%
#   #mutate(date = as.Date(yday, origin = '2018-01-01')) %>% 
#   gather(key = "age_sex" , value = "cval", adult_female,adult_male,calf_female,calf_male,
#          juvenile_female,juvenile_male,unknown)
# 
# # plot
# pltd = ggplot(data = dfp, aes(x=date_discrete,y=cval,fill=age_sex))+
#   geom_col(position = "stack")+
#   
#   # remove white space around plot
#   scale_y_continuous(expand = c(0,0))+
#   
#   # specify weekly axis labels
#   #scale_x_date(breaks = '1 week', date_labels = '%b-%d')+
#   
#   # make pretty
#   scale_fill_grey(start = 0.1, end = 0.9,  
#                   breaks=c("adult_female","adult_male","calf_female","calf_male",
#                            "juvenile_female","juvenile_male","unknown"),
#                   labels=c("Adult, Female","Adult, Male","Calf, Female", "Calf, Male", "Juvenile, Female","Juvenile, Male", "Unknown"))+
#   labs(fill = "Age Class, Sex",
#        y = 'Number of Whales',
#        x = 'Date')+
#   theme_bw()+
#   theme(panel.border = element_blank(), 
#         axis.line = element_line(colour = "black"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1, colour = 'black'),
#         axis.text.y = element_text(colour = 'black'),
#         text = element_text(size=14, family = "serif"))
# 
# pltd
# 
# ggsave(pltd, filename = paste0('figures/manuscript_demographics.png'), 
#        height = 5, width = 8, units = 'in', dpi = 300)
