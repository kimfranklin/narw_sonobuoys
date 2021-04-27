## fig_map_detailed.R ##

#Some useful libraries for plotting
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(metR)
library(rnaturalearth)
library(ggspatial)

# Data and set up
# read in the data
df = readRDS("data/processed/proc_acou_photoid.rds")

# read in map data
load('data/processed/map.rda')


# plot inset map
bg_inset = ne_countries(scale = "medium", returnclass = "sf")

p_inset = ggplot()+
  geom_sf(data = bg_inset,fill = "cornsilk", color = "cornsilk4", size = 0.2)+
  geom_rect(aes(xmin=min_lon,xmax=max_lon,ymin=min_lat,ymax=max_lat), color = 'black',fill = NA)+
  annotate('rect', xmin = -80, xmax = -58, ymin = 34, ymax = 51, fill = NA, color = 'black', size = 1)+
  coord_sf(xlim = c(-80,-58), ylim = c(34,51), expand = FALSE)+
  geom_text(aes(x = -71.5, y = 42.8, label = 'United States'), color = 'darkslategrey', size = 4, family = "serif")+
  geom_text(aes(x = -68.7, y = 48.4, label = 'Canada'), color = 'darkslategrey', size = 4, family = "serif")+
  theme_void()+
  theme(panel.background = element_rect(fill = 'white'))

# # for ofile = 'sono_map_more_detailbathy4.png' and for ofile = 'sono_map_more_detailbathy5.png'
# p_inset = ggplot()+
#   geom_sf(data = bg_inset,fill = "cornsilk", color = "cornsilk4", size = 0.2)+
#   geom_rect(aes(xmin=min_lon,xmax=max_lon,ymin=min_lat,ymax=max_lat), color = 'black',fill = NA)+
#   annotate('rect', xmin = -100, xmax = -50, ymin = 20, ymax = 58, fill = NA, color = 'black', size = 1)+
#   coord_sf(xlim = c(-100,-50), ylim = c(20,58), expand = FALSE)+
#   theme_void()+
#   theme(panel.background = element_rect(fill = 'white'))

# fix month labels
df$month = month(df$date, label = T, abbr = F)

# city positions
cities = data.frame(
  name = c("Shippagan,\nNB", "Percé,\nQC"),
  lat = c(47.7439, 48.5244),
  lon = c(-64.7057, -64.2127)
)

# gsl map
gsl = ggplot() +
  
  # bathymetry
  geom_contour_filled(
    data = bf,
    aes(x = x, y = y, z = z),
    color = NA,
    breaks = c(seq(
      from = 0, to = 150, by = 25
    ), 250, 550)
  ) +
  # scale_fill_grey(start = 0.9, end = 0.3)+
  scale_fill_brewer(palette = 'Blues') +
  
  # coastline
  geom_sf(
    data = cf,
    fill = "cornsilk",
    color = "cornsilk4",
    size = 0.1
  ) +
  
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
  geom_point(data = df,
             aes(x = lon, y = lat,
                 shape = as.character(year)),
             size = 4) +
  scale_shape_manual(values = c(1, 2, 0)) +
  
  # labels
  labs(
    x = "Latitude (W)",
    y = "Longitude (N)",
    #size = 'Upcall prod. rate\n(call/hr/whale)',
    fill = 'Depth (m)',
    shape = 'Year'
  ) +
  
  # inset
  annotation_custom( # for ofile = 'sono_map_more_detailbathy6.png' 
    grob = ggplotGrob(p_inset),
    xmin = -63.25,
    xmax = -62.11,
    ymin = 47.2,
    ymax = 47.8
  ) +
  
  # annotation_custom( # for ofile = 'sono_map_more_detailbathy5.png' 
  #   grob = ggplotGrob(p_inset),
  #   xmin = -63.25,
  #   xmax = -62.17,
  #   ymin = 47.2,
  #   ymax = 47.8
  # ) +

  # annotation_custom(grob = ggplotGrob(p_inset), # for ofile = 'sono_map_more_detailbathy4.png'
  #                   xmin = -62.2,
  #                   xmax = -62.2+0.5,
  #                   ymin = 49-0.75,
  #                   ymax = 49)+
  # 
  # formatting
  coord_sf(expand = FALSE, clip = 'off') +
  labs(#x = "Latitude (W)", y = "Longitude (N)",
    x = NULL, y = NULL, fill = 'Depth (m)') +
  theme_bw() +
  annotation_scale(text_family = "serif", location = 'bl') +
  theme(
    legend.position = "right",
    legend.key = element_rect(color = 'black', fill = NA),
    text = element_text(size = 15, family = "serif"),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    panel.grid = element_blank()
  )
gsl

# output file 
ofile = 'sono_map_more_detailbathy6.png'

# figure directory
fig_dir = 'figures/'

ggsave(gsl, filename = paste0(fig_dir, ofile),
       height = 7, width = 10, dpi = 300)
