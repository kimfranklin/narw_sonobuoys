## fig_map_detailed.R ##

#Some useful libraries for plotting
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(metR)

# Data and set up
# read in the data
df = readRDS("data/processed/proc_acou_photoid.rds")

# read in map data
load('data/processed/map.rda')

# # define map limits
# min_lat = 46.5
# min_lon = -65
# max_lat = 49
# max_lon = -61

# plot inset map
bg_inset = ne_countries(scale = "medium", returnclass = "sf")
p_inset = ggplot()+
  geom_sf(data = bg_inset,fill = "cornsilk", color = "cornsilk4", size = 0.2)+
  geom_rect(aes(xmin=min_lon,xmax=max_lon,ymin=min_lat,ymax=max_lat), color = 'black', fill = NA)+
  annotate('rect', xmin = -100, xmax = -50, ymin = 20, ymax = 58, fill = NA, color = 'black', size = 1)+
  coord_sf(xlim = c(-100,-50), ylim = c(20,58), expand = FALSE)+
  theme_void()

# fix month labels
df$month = month(df$date, label = T, abbr = F)

# # fix bathy data
# ggb = bf %>% dplyr::filter(x >= min_lon & x <= max_lon & y >= min_lat & y <= max_lat)
# ggb$z[ggb$z>=0]=0
# ggb$z = abs(ggb$z)

# city positions
cities = data.frame(
  name = c("Shippagan,\nNB", "Percé,\nQC", "Magdalen\nIslands, QC", "Tignish,\nPEI"),
  lat = c(47.7439, 48.5244, 47.386071, 46.9503),
  lon = c(-64.7057,-64.2127,-61.938777, -64.033)
)


# gsl map
gsl = ggplot() +
  
  # bathymetry
  geom_contour_filled(data=bf, aes(x=x,y=y,z=z), color = NA,
                      breaks = c(seq(from = 0,to = 150, by = 25), 250, 550))+
  # scale_fill_grey(start = 0.9, end = 0.3)+
  scale_fill_brewer(palette = 'Blues') +
  
  # coastline
  geom_sf(data = cf,fill = "cornsilk", color = "cornsilk4", size = 0.1)+
  
  # # plot limits
  # coord_quickmap(xlim = c(min_lon,max_lon), ylim = c(min_lat, max_lat), expand = F)+
  # 
  # # bathymetry
  # geom_contour_filled(data=bf, aes(x=x,y=y,z=z),color = NA,
  #                     breaks = c(seq(from = 0,to = 150, by = 25),500))+
  # scale_fill_brewer(palette = 'Blues')+
  # 
  # # land
  # geom_polygon(data = nam, aes(x = long, y = lat, group = group),fill = "snow3", color = NA)+
  
  # orphaline trough position
  #annotate("text", x = -63.90, y = 47.90, label = '*')+
  
  # add city points and labels
  geom_point(data = cities,
             aes(x = lon, y = lat),
             color = "black",
             size = 1) +
  geom_text(
    data = cities,
    aes(x = lon, y = lat, label = name),
    color =
      "black",
    nudge_y = 0.07,
    size = 4,
    family = "serif"
  ) +
  
  # sonobuoy positions
  geom_point(data = df, aes(x = lon, y = lat, 
                            shape = as.character(year)), size = 4)+
  scale_shape_manual(values = c(1,2,0))+
  
  # labels
  labs(x = "Latitude (W)", 
       y = "Longitude (N)",
       size = 'Upcall prod. rate\n(call/hr/whale)',
       fill = 'Depth (m)',
       shape = 'Year')+
  
  # inset
  annotation_custom(grob = ggplotGrob(p_inset),
                    xmin = max_lon,
                    xmax = max_lon+1,
                    ymin = max_lat-0.5,
                    ymax = max_lat)+
  
  # # formatting
  # theme_bw()+
  # theme(
  #   text = element_text(size = 15, family = "serif"),
  #   axis.text.x = element_text(colour = "black"),
  #   axis.text.y = element_text(colour = "black"),
  #   panel.grid = element_blank(),
  #   axis.text = element_blank(),
  #   axis.ticks = element_blank())


# formatting
coord_sf(expand = FALSE, clip = 'off')+
  labs(#x = "Latitude (W)", y = "Longitude (N)", 
    x = NULL, y = NULL, fill = 'Depth (m)')+
  theme_bw()+
  annotation_scale(text_family = "serif", location = 'bl') +
  theme(legend.position = "right",
        legend.key = element_rect(color = 'black', fill = NA),
        text = element_text(size = 15, family = "serif"),
        panel.grid = element_blank())
gsl

# output file 
ofile = 'sono_map_more_detailbathy2.png'

# figure directory
fig_dir = 'figures/'

ggsave(gsl, filename = paste0(fig_dir, ofile),
       height = 10, width = 15, dpi = 300)
