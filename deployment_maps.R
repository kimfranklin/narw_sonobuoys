## deployment maps

# input

df = readRDS('data/interim/sono_log.rds')

# figure directory

fig_dir = 'figures/'

jpeg (filename = paste0(fig_dir,'2019_only_deployments.jpg'), 
      width = 6, height = 7, units = 'in', res = 200)

# setup

library(oce)
require(oce)
library(ncdf4)
library(ocedata)
library(tidyverse)
data("coastlineWorldFine")
#library(maps)
#data(world.cities)

# process
df1 = df %>% 
  filter (year == '2017')
df2 = df %>% 
  filter (year == '2018')

b = ('get_map_data.R')

# older method, still good but not ggplot way
plot(coastlineWorldFine, col = 'grey',
     clon = mean(df$lon, na.rm = TRUE),
     clat = mean(df$lat, na.rm = TRUE), 
     span = 200, 
     projection = "+proj=merc")
mapPoints(df1$lon, df1$lat, gird=TRUE, pch = 16, col = 'black')
mapPoints(df2$lon, df2$lat, gird=TRUE, pch = 16, col = 'red')
txt = paste0(min(df1$date) , ' to ', max(df1$date), ' and ',
             min(df2$date) , ' to ', max(df2$date))
mtext(text = txt, side = 3, line = 0, adj = 0, cex = 0.8)
legend("topright", as.character(unique(df$year)), fill = unique(df$year))
dev.off()




# using ggplot ------------------------------------------------------------

# sonobuoy log
#sono_file = 'data/processed/sono_sightings.rds'
sono_file = 'data/interim/sono_log.rds'

# observation file
#obs_file = '~/Projects/WhaleMap/data/processed/observations.rds'

# figure directory
fig_dir = 'figures/'

# setup -------------------------------------------------------------------

# libraries
library(tidyverse)
library(lubridate)
library(oce)
library(ggplot2)
library(viridis)
library(grid)
library(xtable)
library(mapproj)
source('functions.R')

# process -----------------------------------------------------------------

# read in map data
load('data/processed/map_data.rda')

# read in points data 
ifile = readRDS("data/processed/sight_dfs.rds")
df= ifile

# # read in data
# sono = readRDS(sono_file) %>%
#   mutate(year = as.character(year))
# 
# 
# # remove not good deployments
# sono = sono[!(sono$id=="2017_noaa_DEP17"),]
# sono = sono[!(sono$id=="2018_noaa_DEP07"),]
# sono = sono[!(sono$id=="2018_noaa_DEP13"),]
# sono = sono[!(sono$id=="2018_noaa_DEP14"),]
# sono = sono[!(sono$id=="2018_noaa_DEP17"),]
# sono = sono[!(sono$id=="2018_noaa_DEP18"),]


#obs = readRDS(obs_file)

# subset observations
#obs = subset(obs, species == 'right' & 
               #score %in% c('definite visual', 'definite acoustic') &
               #date %in% sono$date)

# add observation month
#obs$month = month(obs$date, label = TRUE, abbr = FALSE)
#sono$month = month(sono$date, label = TRUE, abbr = FALSE)

# remove failed deployments
#sono = subset(sono, deploy_success == 'yes')

# city positions
cities = data.frame(
  name = c("Shippagan", "Percé"),
  lat = c(47.7439, 48.5244),
  lon = c(-64.7057, -64.2127)
)

# gsl map
gsl = ggplot() + 
  coord_map(xlim = c(-65,-63), ylim = c(47,48.75))+
  
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
  geom_point(data = df, aes(x = lon, y = lat), 
             shape = 16, size = 3, alpha = 0.7, 
             colour = as.character(year)+
  scale_fill_manual(values = c("black",'red'))+
  
  # facet
  # facet_wrap(~year)+
  
  # formatting
  ylab("Longitude (N)")+xlab("Latitude (W)")+
  #labs(x = 'Longitude', y = 'Latitude', color = 'Depth (m)')+
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
ggsave(gsl, filename = paste0(fig_dir, 'sonobuoys_year_month.png'), height = 6, width = 4)



# using ggplot for taggart 2019 aerial and vessel map -------------------------------------

# sonobuoy log
#sono_file = 'data/processed/sono_sightings.rds'
sono_file = 'data/interim/sono_log_to_2019_aug.rds'

#sonobuoy vessel log
sono_vess = 'data/interim/sono_vessel_lat_lon_to_2019_aug.rds'

# figure directory
fig_dir = 'figures/'

# setup -------------------------------------------------------------------

# libraries
library(tidyverse)
library(lubridate)
library(oce)
library(viridis)
library(grid)
library(xtable)
library(mapproj)
source('functions.R')

# process -----------------------------------------------------------------

# read in map data
load('data/processed/map_data.rda')

# read in data
sono_p = readRDS(sono_file) %>%
  mutate(year = as.character(year))

sono_v = readRDS(sono_vess) 
sono_v$Latitude = as.numeric(sono_v$Latitude)
sono_v$Longitude = as.numeric(sono_v$Longitude)


sono_p = sono_p %>%
  filter(year == '2019')

sono_v = sono_v[-c(1),]
sono_v$Longitude = sono_v$Longitude*(-1)


# city positions
cities = data.frame(
  name = c("Shippagan", "Percé"),
  lat = c(47.7439, 48.5244),
  lon = c(-64.7057, -64.2127)
)


# gsl map
gsl = ggplot() + 
  coord_map(xlim = c(-65,-61.75), ylim = c(47,49))+
  #-72, 40
  
  # land and bathymetry
  geom_polygon(data = name, aes(x = long, y = lat, group = group),
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
  geom_point(data = sono_p, aes(x = lon, y = lat), 
             shape = 16, size = 3, alpha = 0.7, colour = 'blue')+
  #scale_fill_manual(values == "black")+
  geom_point(data = sono_v, aes(x = Longitude, y = Latitude), 
             shape = 16, size = 3, alpha = 0.7, colour = 'orange')+
  #scale_fill_manual(values == "black")+
  
  # facet
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
ggsave(gsl, filename = paste0(fig_dir, 'Taggart_2019_sonobuoys_map.png'), height = 6, width = 4)
