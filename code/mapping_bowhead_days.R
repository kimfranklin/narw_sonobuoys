## mapping_bowhead_days.R ##
# creating maps for the days that bowhead whales were seen in GSl 2019


library(readxl)

# ead in data
df = read.csv('data/raw/visual/photo-id/2019NOAAGSL_sightingsUPDATED.csv')
dfs = read_excel('data/raw/acoustic/position/noaa_sono_positions_ALL.xlsx')

# figure directory (used to save the figure at end of code)
fig_dir = 'figures/'

# setup -------------------------------------------------------------------

# libraries (i can't remember if i use them all but better to have more
# than not enough libraries)
library(tidyverse)
library(lubridate)
library(oce)
library(viridis)
library(grid)
library(xtable)
library(mapproj)
source('functions.R')

# process -----------------------------------------------------------------

# find bow sightings
bowh = df %>%
  filter(EGNO == 'BOWH') %>%
  filter(Date == '8/15/2019')
#bowhead dates are '8/15/2019' and '8/25/2019'
tmp = as.character(dfs$date)
dfs$date = tmp
sono = dfs %>%
  filter(date == c('2019-08-15'))
whale = df %>%
  filter(Date == '8/15/2019')
whale = whale[!(whale$EGNO=="BOWH"),]



# read in map data
load('data/processed/map_data.rda')

# read in data
sono = readRDS(sono_file) %>%
  mutate(year = as.character(year))

# city positions
cities = data.frame(
  name = c("Shippagan", "Percé"),
  lat = c(47.7439, 48.5244),
  lon = c(-64.7057, -64.2127)
)

# gsl map
gsl = ggplot() + 
  coord_map(xlim = c(-63.5,-64), ylim = c(48,47.75))+
  
  # land and bathymetry
  geom_polygon(data = nam, aes(x = long, y = lat, group = group),
               fill = "darkgrey", color = NA) +
  geom_contour(data = bf, aes(x=x, y=y, z=z),
               breaks=c(-50),colour="grey", size=0.3)+
  geom_contour(data = bf, aes(x=x, y=y, z=z),
               breaks=c(-100),colour="grey", size=1)+
  
  # city labels
  geom_point(data = cities, aes(x = lon, y = lat), color = "darkslategrey", 
             size = 1)+
  geom_text(data = cities, aes(x = lon, y = lat, label = name), color = "darkslategrey",
            nudge_y = 0.07, size = 3)+
  
  # # sightings
  # geom_point(data = obs, aes(x = lon, y = lat), 
  #            color = 'grey', shape = 1, alpha = 0.3)+
  
  # sonobuoy positions
  geom_point(data = sono, aes(x = lon, y = lat), 
             shape = 16, size = 3, alpha = 1, colour = 'black')+
  #scale_fill_manual(values = c("black"))+
  
  #  positions
  geom_point(data = bowh, aes(x = Longitude, y = Latitude), 
             shape = 16, size = 3, alpha = 1, colour = 'red')+
  #scale_fill_manual(values = c("red"))+
  
  #  positions
  geom_point(data = whale, aes(x = Longitude, y = Latitude), 
             shape = 16, size = 3, alpha = 0.5, colour = 'orange')+
  #scale_fill_manual(values = c("blue"))+
  
  
  # facet to compare years
  # facet_wrap(~year)+
  
  # formatting
  ylab("Longitude (N)")+xlab("Latitude (W)")+
  theme_bw()+
  theme(legend.title = element_blank(), 
        legend.position = 'bottom', 
        legend.direction = "horizontal",
        panel.spacing = unit(2, "lines"))+
  theme(text = element_text(size=15))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))

gsl
# save
ggsave(gsl, filename = paste0(fig_dir, 'bowhead_day_15Aug2019_zoomed.png'), height = 6, width = 4)

