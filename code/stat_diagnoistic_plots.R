## diagnoistic_plot_selections.R
# plotting 

# input -------------------------------------------------------------------

# figure directory
fig_dir = 'figures/'

# define input data file
ifile = readRDS('data/processed/acoustic.rds')
 
# save plot
ggsave(pie1, filename = paste0(fig_dir,'2017_pie_update1.png'), 
       height = 5, width = 7, units = 'in', dpi = 300) 
# setup -------------------------------------------------------------------

library(tidyverse)
library(viridis)
library(ggplot2)
library(dplyr)
library(lubridate)
library(grid)
library(RColorBrewer)

# The palette with black:
cbbPalette = c("#009E73", "#56B4E9" , "#CC79A7", "#000000", "#F0E442", "#0072B2", "#D55E00")


# create figure directory if it does not exist
if(!dir.exists(fig_dir)){
  dir.create(fig_dir)
}


# read in data -------------------------------------------------------------

df = ifile

# bar graph plots ----------------------------------------------------------

#grouped bar graph

# stacking years ontop of each other
plt = df %>%
  filter(call_type %in% c('up', 'gs', 'mf')) %>%
  filter(is.na(score) | score == 1) %>%
  ggplot(aes(fill=call_type, x=year_day)) + 
  #geom_histogram(position="dodge", stat='count')+
  scale_fill_viridis(discrete = TRUE, option = "D")+
  facet_wrap(.~year, ncol = 1)+
  geom_bar(position="stack", stat='count')
plt

# stacking years ontop but classifying by week in year and stacking calls per dep
plt = df %>%
  mutate(week_num = week(date)) %>%
  filter(call_type %in% c('up', 'gs', 'mf')) %>%
  filter(is.na(score) | score == 1) %>%
  ggplot(aes(fill=call_type, x=week_num)) + 
  #geom_histogram(position="dodge", stat='count')+
  scale_fill_manual(values=cbbPalette, breaks=c("gs", "mf", "up"),
                    labels=c("Gunshot", "Mid-frequency", "Up Call"))+
  facet_wrap(.~year, ncol = 1)+
  geom_bar(position="stack", stat='count')+
  labs(fill = 'Call Type', x = 'Week Number', y = 'Count')+
  theme(text = element_text(size=15))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))
plt

# calls separated by year
plt = df %>%
  mutate(week_num = week(date)) %>%
  filter(call_type %in% c('up', 'gs', 'mf')) %>%
  filter(is.na(score) | score == 1) %>%
  ggplot(aes(fill=call_type, x=week_num)) + 
  #geom_histogram(position="dodge", stat='count')+
  scale_fill_viridis(discrete = TRUE, option = "D")+
  facet_wrap(.~year+call_type, scales="free_y", nrow = 2, ncol = 3)+
  geom_bar(position="stack", stat='count')
plt

# for years side by side plot
plt = df %>%
  filter(call_type %in% c('up', 'gs', 'mf')) %>%
  filter(is.na(score) | score == 1) %>%
  ggplot(aes(fill=call_type, x=id)) + 
  geom_histogram(position="dodge", stat='count')+
  scale_fill_viridis(discrete = TRUE, option = "D")+
  facet_wrap(~year)
plt

# bargraph by call (can change call type or even add more than 
# one kind with call_type %in% c('then list all the call types interested in'))
plt = df %>%
  filter(call_type == 'up') %>%
  filter(is.na(score) | score == 1) %>%
  ggplot(aes(fill = factor(year), x = month)) + 
  geom_bar(position="dodge", stat='count')+
  scale_fill_viridis(discrete = TRUE, option = "E")

plt

# bar graph, call on x axis 
plt = df %>%
  filter(call_type %in% c('up', 'gs', 'mf', 'lf', 'hf')) %>%
  filter(is.na(score) | score == 1) %>%
  ggplot(aes(fill = factor(year), x = call_type)) + 
  geom_bar(position="dodge", stat='count')+
  scale_fill_viridis(discrete = TRUE, option = "E")+
  labs(fill = 'Year', x = 'Call Type', y = 'Count')
plt

# stacked bar graph
plt = df %>%
  filter(call_type %in% c('up', 'gs', 'mf', 'lf', 'hf')) %>%
  filter(is.na(score) | score == 1) %>%
  ggplot(aes(x = id, fill = call_type)) + 
  geom_bar(position="stack", stat='count')+
  scale_fill_viridis(discrete = TRUE, option = "D")#+
#facet_grid(rows = vars(year))
plt

# by year bar plot/individual year plots
plt = df %>%
  filter(year == '2017') %>%
  filter(is.na(score) | score == 1) %>%
  filter(call_type %in% c('up', 'gs', 'mf')) %>%
  ggplot(aes(fill=call_type, x=id)) + 
  geom_histogram(position="dodge", stat='count')+
  scale_fill_viridis(discrete = TRUE, option = "D")+
  labs(fill = 'Call Type', x = 'ID', y = 'Count' )
plt


# pie graphs --------------------------------------------------------------

# pie chart per year of calls
pie = df %>% 
  filter (year == '2017') %>%
  filter(is.na(score) | score == 1) %>%
  filter(call_type %in% c('mf','up','gs')) %>%
  count(call_type) %>%
  mutate(perc = n /sum(n) * 100)

pie1 = ggplot(pie, aes(x = "", y = perc, fill = call_type)) + 
  geom_bar(width = 1, stat='identity')+
  geom_text(aes( label = paste0(round(n / sum(n) * 100, 2),
                               "%")),
            position = position_stack(vjust = 0.5), size = 7)+
  coord_polar(theta = "y")+
  theme_void()+
  scale_fill_manual(values=cbbPalette, 
                    breaks=c("gs", "mf", "up"),
                    labels=c("Gunshot", "Mid-frequency", "Up Call"))+
  labs(fill = "Call Type",
       x = NULL,
       y = NULL,
       title = "2017, n = 8")+
  theme(plot.title = element_text(size = 27, hjust = 0.5))+
  theme(text = element_text(size=20))
pie1

# pie of all necessary calls
pie = df %>% 
  #filter (year == '2017') %>%
  filter(is.na(score) | score == 1) %>%
  filter(call_type %in% c('mf','up','gs','hf','lf','bl','mw','bw','sw')) %>%
  count(call_type) %>%
  mutate(perc = n /sum(n) * 100)

ggplot(pie, aes(x = "", y = perc, fill = call_type)) + 
  geom_bar(width = 1, stat='identity')+
  geom_text(aes(label = paste0(call_type, '\n',
                               round(n / sum(n) * 100, 2),
                               "%")),
            position = position_stack(vjust = 0.5))+
  coord_polar(theta = "y")+
  theme_void()+
  scale_fill_viridis(discrete = TRUE, option = "D")+
  labs(fill = "Call Type",
       x = NULL,
       y = NULL,
       title = "Pie chart of all calls with percentages, respectively")

#Old pie formatting
plt_all_2017 = df %>%
  filter(year == '2017') %>%
  filter(is.na(score) | score == 1) %>%
  filter(call_type %in% c('mf','up','gs','bl','lf','hf')) %>%
  ggplot(aes(fill=call_type, x='')) + 
  geom_bar(width = 1, stat='count') + 
  facet_wrap(~year)+
  coord_polar("y", start=0)
pie_all_2017

# stacked bar graph for duration, number of whales, number of calls --------
# THIS WAS FOR THE PRESENTATION FIGURES

# location of completed acoustic file
df = readRDS('data/processed/acoustic.rds')
#df_sightings = readRDS('data/interim/sightings_data.rds')
sight_dfs = readRDS('data/processed/sight_dfs.rds')
photo_id = readRDS('data/processed/photo_id.rds') # full sightings, sight_df 


fig_dir = 'figures/'

library(tidyverse)
library(lubridate)
library(oce)
library(viridis)
library(grid)
library(xtable)
library(ggplot2)

# merge sex and_ahe class is photo-id
photo_id$sex_age_class = paste0(photo_id$sex,photo_id$age_class)

# calculate recording durations
#durs = calc_recording_durations(df)

plt_yr = 2018

# count calls per deployment, IN FUTURE CHANGE TO CALLS PER HOUR
plt = df %>%
  filter(year == plt_yr) %>%
  filter(call_type %in% c('gs', 'mf', 'up')) %>%
  filter(is.na(score) | score == 1) %>%
  mutate(
    call_type = as.factor(call_type),
    date = as.Date(start_time)
  ) %>%
  count(call_type, date, id) %>%
  droplevels()

# make label for plotting
plt$label = paste0(str_split(plt$id, '_', simplify = TRUE)[,3], ' (',
                   format(plt$date, '%b-%d'), ')') 

# rename levels for plotting
# levels(plt$call_type) = c('Low-freq', 'Mid-freq', 'Gunshot', 'Upcall')

# only plt_yr
yr = df %>%
  filter(year == plt_yr) %>%
  transmute(id,deployment_duration) %>%
  unique() %>%
  filter(deployment_duration>0.5)


sight_dfs = sight_dfs %>%
  filter(year == plt_yr)

# sightings
p1 = ggplot(data = sight_dfs, aes(x = id, y = num_sighting))+
  geom_bar(stat = 'identity')+
  ggtitle('Sightings within 15 km')+
  ylab('Sightings per deployment duration plus an hour before and after recording')+
  xlab('')+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  theme(text = element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black"))
p1

p1 = photo_id %>%
  filter(year == plt_yr) %>%
  filter(sex_age_class %in% c('FA', 'FJ', 'MA', 'MJ')) %>%
  ggplot(aes(x = id, fill = sex_age_class)) + 
  ggtitle('Sightings within 15 km')+
  geom_bar(position="stack", stat='count')+
  scale_fill_viridis(discrete = TRUE, option = "D", 
                     breaks=c("FA", "FJ", "MA", "MJ", "UJ", "XA", "XJ", "XU"),
                     labels=c("Adult, Female", "Juvenile, Female",
                              "Adult, Male", "Juvenile, Male", "Juvenile Unknown Sex",
                              "Adult X", "Juvenile X", "Unknown Age, Unknown Sex"))+
  labs(fill = "Age Class, Sex",
       x = NULL,
       y = NULL,
       title = 'Sightings within 15 km plus an hour before and after recording')+
  #scale_fill_manual = (breaks=c("FA", "FJ", "MA", "MJ", "UJ", "XA", "XJ", "XU"),
                       #labels=c("Adult, Female", "Juvenile, Female",
                       #"Adult, Male", "Juvenile, Male", "Juvenile, Unknown Sex",
                       #"Adult, X", "Juvenile, X", "Unknown Age, Unknown Sex"))+
  #ylab(expression(atop('Sightings per deployment duration', paste('plus an hour before and after recording'))))+
  ylab('Sightings per deployment duration')+
  xlab('')+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  theme(text = element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black"))
p1

# non ggplot alternative way to stack info in p1 by sex and age
library(plotly)
p12 = plot_ly(data=sight_dfs, x = ~id, y = ~juvenile_female, 
        type = 'bar', name = 'Juenile Females') %>%
  add_trace(y = ~juvenile_male, name = 'Juvenile Males') %>%
  add_trace(y = ~adult_female, name = 'Adult Females') %>%
  add_trace(y = ~adult_male, name = 'Adult Males') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
p12
# recording duration
p2 = ggplot(data = yr, aes(x = id, y = deployment_duration))+
  geom_bar(stat = 'identity')+
  ggtitle('Sonobuoy recording duration')+
  ylab('Hours')+
  xlab('')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  #theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  theme(text = element_text(size=15))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(strip.text.x = element_text(size = 16))
p2

# call types/counts
#tmp = plt[1,]
#tmp$id = "2017_noaa_DEP17"
#tmp$label = "DEP17 (Jul-20)"
#tmp$call_type = 'up'
#tmp$n = NA
#plt2=rbind(plt,tmp)

p3 = ggplot(data = plt, aes(x = label, y = n, fill = call_type))+
  geom_bar(stat = 'identity', size = 0.25, 
           color = 'black', position = position_dodge2(width = 1, 
                                                       preserve = "single")) +
  #scale_fill_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c'))+
  #scale_fill_manual(values = c('white','grey','darkslategrey','black'))+
  scale_fill_manual(values = c('grey','grey28','black'), 
                     breaks=c("gs", "mf", "up"),
                     labels=c("Gunshot", "Mid-frequency", "Up Call"))+
  # scale_fill_manual(values = grey.colors(4))+
  ggtitle('Calls detected')+
  ylab('Number of calls')+
  xlab('')+
  labs(fill = 'Call Type:')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="right")+
  theme(text = element_text(size=20))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))
p3

# plot
png(paste0(fig_dir,plt_yr, 'age_sex2018_duration2.png'), height = 10, width = 10,
    units = 'in', res = 300)

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p3), size = "last"))

dev.off()

# adding photo_id data ----------------------------------------------------

sight_df = readRDS('data/processed/photo_id.rds')

# sex vs. week separated by year
plt = sight_df %>%
  filter(sex %in% c('M', 'F', 'X'))%>%
  mutate(week_num = week(date)) %>%
  ggplot(aes(x = week_num, fill = sex)) + 
  geom_bar(position="stack", stat='count')+
  scale_fill_manual(values=cbbPalette, 
                     breaks=c("F", "M", "X"),
                     labels=c("Female", "Male", "Unknown"))+
  labs(fill = 'Sex', x = 'Week Number', y = 'Number of NARW Sighted')+
  theme(text = element_text(size=15))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  facet_wrap(.~year, scales = 'free_y', nrow = 3, ncol = 3)+
  theme(strip.text.x = element_text(size = 16))
plt

ggsave(plt, filename = paste0(fig_dir,'sighted ages2.png'), 
       height = 5, width = 9, units = 'in', dpi = 300) 

# trying to plot age 
plt = sight_df %>%
  mutate(week_num = week(date)) %>%
  ggplot()+
  geom_bar(aes(x = week_num, y = as.factor(age), fill = age), 
           position="stack", stat='identity')+
  scale_fill_viridis(discrete = FALSE, option = "D")+
  facet_wrap(.~year, ncol = 1)
plt

# age by sighted  
plt = sight_df %>%
  ggplot()+
  geom_histogram(aes(x = age), colour = 'black',
           position="dodge", stat='count')+
  labs(x = 'Age', y = 'Number of NARW Sighted')+
  theme(text = element_text(size=15))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(strip.text.x = element_text(size = 16))+
  facet_wrap(.~year, ncol = 1)
plt

ggsave(plt, filename = paste0(fig_dir,'sighted ages2.png'), 
       height = 5, width = 9, units = 'in', dpi = 300)

# trying to plot age 
plt = sight_df %>%
  ggplot(aes(fill = as.factor(year), x = age)) + 
  geom_bar(position="dodge", stat='count')+
  scale_color_manual(values=c("black", "red"))+
  labs(fill = 'Year', x = 'Age', y = 'Count')+
  theme(text = element_text(size=15))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(strip.text.x = element_text(size = 16))
plt

# trying to plot age
plt = sight_df %>%
  mutate(week_num = week(date)) %>%
  ggplot()+
  geom_bar(aes(x = week_num, y = age_class, fill = age), 
           position="stack", stat='identity')+
  scale_fill_viridis(discrete = FALSE, option = "D")+
  facet_wrap(.~year, ncol = 1)
plt
