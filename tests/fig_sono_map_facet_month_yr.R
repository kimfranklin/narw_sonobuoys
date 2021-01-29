## fig_sono_map_facet_month_yr.R ##

# an alterative to the thesis sonobuoy map


#Some useful libraries for plotting
library(tidyverse)
library(lubridate)


# Data and set up
# read in the data
df = readRDS("data/processed/proc_acou_photoid.rds")

# read in map data
load('data/processed/map_data.rda')

# define map limits
min_lat = 47.2
min_lon = -65
max_lat = 48.8
max_lon = -62.25

# fix month labels
df$month = month(df$date, label = T, abbr = F)

# fix bathy data
ggb = bf %>% dplyr::filter(x >= min_lon & x <= max_lon & y >= min_lat & y <= max_lat)
ggb$z[ggb$z>=0]=0
ggb$z = abs(ggb$z)

# city positions
cities = data.frame(
  name = c("Shippagan", "Percé"),
  lat = c(47.7439, 48.5244),
  lon = c(-64.7057,-64.2127)
)


## Maps with call production rates and behaviour rates as colours {.tabset}

### Up calls 
# up production rate
# gsl map
gsl = ggplot() +
  
  # plot limits
  coord_quickmap(xlim = c(min_lon,max_lon), ylim = c(min_lat, max_lat), expand = F)+
  
  # bathymetry
  geom_contour_filled(data=ggb, aes(x=x,y=y,z=z), color = NA, 
                      breaks = c(seq(from = 0,to = 150, by = 25),500))+
  scale_fill_grey(start = 0.8, end = 0.3)+
  
  # land
  geom_polygon(data = nam, aes(x = long, y = lat, group = group),fill = "cornsilk", color = NA)+
  
  # orphaline trough position
  #annotate("text", x = -63.90, y = 47.90, label = '*')+
  
  # add city points and labels
  geom_point(data = cities,
             aes(x = lon, y = lat),
             color = "darkslategrey",
             size = 1) +
  geom_text(
    data = cities,
    aes(x = lon, y = lat, label = name),
    color =
      "darkslategrey",
    nudge_y = 0.07,
    size = 3,
    family = "serif"
  ) +
  
  # sonobuoy positions
  geom_point(data = df, aes(x = lon, y = lat), 
             shape = 21, fill = 'red', alpha = 0.6) + 
  
  # facet
  facet_grid(year ~ month) +
  
  # labels
  labs(x = "Latitude (W)", 
       y = "Longitude (N)",
       size = 'Upcall prod. rate\n(call/hr/whale)',
       fill = 'Depth (m)')+
  
  # formatting
  theme_bw()+
  theme(
    text = element_text(size = 15, family = "serif"),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank())

gsl


# output file 
ofile = 'sono_map_facet_month_yr.png'

# figure directory
fig_dir = 'figures/'

ggsave(gsl, filename = paste0(fig_dir, ofile), 
       height = 10, width = 12, dpi = 300)
