## fig_map.R ##
# sonobuoy deployment map

# libraries ---------------------------------------------------------------

library(tidyverse)
library(RColorBrewer)
library(oce)
library(mapproj)

# input -------------------------------------------------------------------

# input files
ifile = readRDS("data/processed/proc_acou_photoid.rds")

# read in map data
load('data/processed/map_data.rda')

# setup -------------------------------------------------------------------

# data
df= ifile

# output file 
ofile = 'sono_map_new.png'

# figure directory
fig_dir = 'figures/'

# process -----------------------------------------------------------------

# city positions
cities = data.frame(
  name = c("Shippagan", "Percé"),
  lat = c(47.7439, 48.5244),
  lon = c(-64.7057, -64.2127)
)

# gsl map
gsl = ggplot() + 
  coord_map(xlim = c(-65,-62.1), ylim = c(47.25,48.75))+
  
  # land and bathymetry
  geom_polygon(data = nam, aes(x = long, y = lat, group = group),
               fill = "darkgrey", color = NA) +
  # geom_contour(data = bf, aes(x=x, y=y, z=z),
  #              breaks=c(-50),colour="grey", size=0.3)+
  # geom_contour(data = bf, aes(x=x, y=y, z=z),
  #               breaks=c(-100),colour="grey", size=1)+
  geom_contour(data = bf, aes(x=x,y=y,z=z,
                              color=as.factor(..level..)), 
               breaks = c(-50,-100), size=1)+
  scale_color_brewer(palette = 'Blues',
                     breaks=c("-50", "-100"),
                     labels=c("50", "100"))+
  
  # city labels
  geom_point(data = cities, aes(x = lon, y = lat), color = "darkslategrey", size = 1)+
  geom_text(data = cities, aes(x = lon, y = lat, label = name), color =
              "darkslategrey", nudge_y = 0.07, size = 4, family = "serif")+
  
  # sonobuoy positions
  geom_point(data = df, aes(x = lon, y = lat,
                            shape = as.character(year)), size = 2)+
  #scale_colour_manual(values = c("black","red","blue"))+
  scale_shape_manual(values = c(1,2,0))+
  
  # formatting
  #ylab("Longitude (N)")+xlab("Latitude (W)")+
  labs(x = "Longitude", 
       y = "Latitude", 
       col = "Depth (m)",
       shape = "Year")+
  
  theme_bw()+
  theme(#legend.title = element_blank(), 
        #legend.position = c(0, 0),
        #legend.justification = c(0,0),
        legend.position = 'right', 
        #legend.direction = "horizontal"
        panel.spacing = unit(2, "lines"))+
  # legend.background = element_rect(fill="white", 
  #                                  size=0.5, 
  #                                  color = 'black'))+
  theme(text = element_text(size=12, family = "serif"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))

gsl

ggsave(gsl, filename = paste0(fig_dir, ofile), 
       height = 8, width = 6, dpi = 300)
