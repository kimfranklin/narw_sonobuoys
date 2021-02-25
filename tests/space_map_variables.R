# space_map_of_varibles.R #

# make maps that faceted by month year that show call behaviour and behaviour 
# strength for each deployment position
# more or less same as space_dist_of_variables_17-19.Rmd file

# libraries
library(ggplot2)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(oce)
library(mapproj)

# figure directory
fig_dir = 'figures/'

# read in data
ifile = readRDS("data/processed/proc_acou_photoid.rds")
df1= ifile

# change month year so that it can be ploted
df1$myear = as.Date(df1$myear)

# read in map data
load('data/processed/map_data.rda')


# up production rate
# city positions
cities = data.frame(
  name = c("Shippagan", "Percé",'OT'),
  lat = c(47.7439, 48.5244,47.90),
  lon = c(-64.7057,-64.2127,-63.90)
)

# gsl map
gsl = ggplot() +
  coord_map(xlim = c(-65,-62), ylim = c(47.2, 48.8)) +
  
  # land and bathymetry
  geom_polygon(
    data = nam,
    aes(x = long, y = lat, group = group),
    fill = "darkgrey",
    color = NA
  ) +
  geom_contour(
    data = bf,
    aes(x = x, y = y, z = z),
    breaks = c(-50),
    colour = "grey",
    size = 0.3
  ) +
  geom_contour(
    data = bf,
    aes(x = x, y = y, z = z),
    breaks = c(-100),
    colour = "grey",
    size = 1
  ) +
  
  # city labels
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
    size = 3
  ) +
  
  # sonobuoy positions
  geom_point(data = df1,
             aes(x = lon,
                 y = lat,
                 colour = up_per_hr_per_whale)) + #, colour = yday)) +
  #scale_colour_continuous(type = "viridis")+
  # scale_fill_viridis_c(guide = guide_colourbar(barheight = 15,
  #                                              title.theme = element_text(angle=90),
  #                                              title.position = 'right', 
  #                                              title.hjust = 0.5))+
  #scale_fill_continuous(type = "viridis")+
  #scale_colour_viridis(guide='colourbar') +
  scale_colour_viridis() +
  scale_radius(range = c(3, 8)) +
  
  labs(x = "Latitude (W)", 
       y = "Longitude (N)")+
  #guides(colour=guide_legend(title="Rate"))+
  
  # facet by year
  #facet_wrap(~myear)+
  facet_grid(year ~ month) +
  
  # formatting
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = 'bottom',
    legend.direction = "horizontal",
    panel.spacing = unit(2, "lines")
  ) +
  theme(text = element_text(size = 15)) +
  theme(axis.text.x = element_text(colour = "black")) +
  theme(axis.text.y = element_text(colour = "black"))

gsl

#direct.label(gsl, method = "bottom.pieces")

ggsave(gsl, filename = paste0(fig_dir,'up_prate_map.png'), 
       height = 10, width = 8, dpi = 300)

# gs production rate
# city positions
cities = data.frame(
  name = c("Shippagan", "Percé",'OT'),
  lat = c(47.7439, 48.5244,47.90),
  lon = c(-64.7057,-64.2127,-63.90)
)

# gsl map
gsl = ggplot() +
  coord_map(xlim = c(-65,-62), ylim = c(47.2, 48.8)) +
  
  # land and bathymetry
  geom_polygon(
    data = nam,
    aes(x = long, y = lat, group = group),
    fill = "darkgrey",
    color = NA
  ) +
  geom_contour(
    data = bf,
    aes(x = x, y = y, z = z),
    breaks = c(-50),
    colour = "grey",
    size = 0.3
  ) +
  geom_contour(
    data = bf,
    aes(x = x, y = y, z = z),
    breaks = c(-100),
    colour = "grey",
    size = 1
  ) +
  
  # city labels
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
    size = 3
  ) +
  
  # sonobuoy positions
  geom_point(data = df1,
             aes(x = lon,
                 y = lat,
                 colour = gs_per_hr_per_whale)) + #, colour = yday)) +
  #scale_colour_continuous(type = "viridis")+
  # scale_fill_viridis_c(guide = guide_colourbar(barheight = 15,
  #                                              title.theme = element_text(angle=90),
  #                                              title.position = 'right', 
  #                                              title.hjust = 0.5))+
  #scale_fill_continuous(type = "viridis")+
  #scale_colour_viridis(guide='colourbar') +
  scale_colour_viridis() +
  scale_radius(range = c(3, 8)) +
  
  labs(x = "Latitude (W)", 
       y = "Longitude (N)")+
  #guides(colour=guide_legend(title="Rate"))+
  
  # facet by year
  #facet_wrap(~myear)+
  facet_grid(year ~ month) +
  
  # formatting
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = 'bottom',
    legend.direction = "horizontal",
    panel.spacing = unit(2, "lines")
  ) +
  theme(text = element_text(size = 15)) +
  theme(axis.text.x = element_text(colour = "black")) +
  theme(axis.text.y = element_text(colour = "black"))

gsl

ggsave(gsl, filename = paste0(fig_dir,'gs_prate_map.png'), 
       height = 10, width = 8, dpi = 300)

# mf production rate
# city positions
cities = data.frame(
  name = c("Shippagan", "Percé",'OT'),
  lat = c(47.7439, 48.5244,47.90),
  lon = c(-64.7057,-64.2127,-63.90)
)

# gsl map
gsl = ggplot() +
  coord_map(xlim = c(-65,-62), ylim = c(47.2, 48.8)) +
  
  # land and bathymetry
  geom_polygon(
    data = nam,
    aes(x = long, y = lat, group = group),
    fill = "darkgrey",
    color = NA
  ) +
  geom_contour(
    data = bf,
    aes(x = x, y = y, z = z),
    breaks = c(-50),
    colour = "grey",
    size = 0.3
  ) +
  geom_contour(
    data = bf,
    aes(x = x, y = y, z = z),
    breaks = c(-100),
    colour = "grey",
    size = 1
  ) +
  
  # city labels
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
    size = 3
  ) +
  
  # sonobuoy positions
  geom_point(data = df1,
             aes(x = lon,
                 y = lat,
                 colour = mf_per_hr_per_whale)) + #, colour = yday)) +
  #scale_colour_continuous(type = "viridis")+
  # scale_fill_viridis_c(guide = guide_colourbar(barheight = 15,
  #                                              title.theme = element_text(angle=90),
  #                                              title.position = 'right', 
  #                                              title.hjust = 0.5))+
  #scale_fill_continuous(type = "viridis")+
  #scale_colour_viridis(guide='colourbar') +
  scale_colour_viridis() +
  scale_radius(range = c(3, 8)) +
  
  labs(x = "Latitude (W)", 
       y = "Longitude (N)")+
  #guides(colour=guide_legend(title="Rate"))+
  
  # facet by year
  #facet_wrap(~myear)+
  facet_grid(year ~ month) +
  
  # formatting
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = 'bottom',
    legend.direction = "horizontal",
    panel.spacing = unit(2, "lines")
  ) +
  theme(text = element_text(size = 15)) +
  theme(axis.text.x = element_text(colour = "black")) +
  theme(axis.text.y = element_text(colour = "black"))

gsl

ggsave(gsl, filename = paste0(fig_dir,'mf_prate_map.png'), 
       height = 10, width = 8, dpi = 300)

# foraging rate
# city positions
cities = data.frame(
  name = c("Shippagan", "Percé",'OT'),
  lat = c(47.7439, 48.5244,47.90),
  lon = c(-64.7057,-64.2127,-63.90)
)

# gsl map
gsl = ggplot() +
  coord_map(xlim = c(-65,-62), ylim = c(47.2, 48.8)) +
  
  # land and bathymetry
  geom_polygon(
    data = nam,
    aes(x = long, y = lat, group = group),
    fill = "darkgrey",
    color = NA
  ) +
  geom_contour(
    data = bf,
    aes(x = x, y = y, z = z),
    breaks = c(-50),
    colour = "grey",
    size = 0.3
  ) +
  geom_contour(
    data = bf,
    aes(x = x, y = y, z = z),
    breaks = c(-100),
    colour = "grey",
    size = 1
  ) +
  
  # city labels
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
    size = 3
  ) +
  
  # sonobuoy positions
  geom_point(data = df1,
             aes(x = lon,
                 y = lat,
                 colour = foraging_bhv_whale)) + #, colour = yday)) +
  #scale_colour_continuous(type = "viridis")+
  # scale_fill_viridis_c(guide = guide_colourbar(barheight = 15,
  #                                              title.theme = element_text(angle=90),
  #                                              title.position = 'right', 
  #                                              title.hjust = 0.5))+
  #scale_fill_continuous(type = "viridis")+
  #scale_colour_viridis(guide='colourbar') +
  scale_colour_viridis() +
  scale_radius(range = c(3, 8)) +
  
  labs(x = "Latitude (W)", 
       y = "Longitude (N)")+
  #guides(colour=guide_legend(title="Rate"))+
  
  # facet by year
  #facet_wrap(~myear)+
  facet_grid(year ~ month) +
  
  # formatting
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = 'bottom',
    legend.direction = "horizontal",
    panel.spacing = unit(2, "lines")
  ) +
  theme(text = element_text(size = 15)) +
  theme(axis.text.x = element_text(colour = "black")) +
  theme(axis.text.y = element_text(colour = "black"))

gsl

ggsave(gsl, filename = paste0(fig_dir,'forage_rate_map.png'), 
       height = 10, width = 8, dpi = 300)


# social rate
# city positions
cities = data.frame(
  name = c("Shippagan", "Percé",'OT'),
  lat = c(47.7439, 48.5244,47.90),
  lon = c(-64.7057,-64.2127,-63.90)
)

# gsl map
gsl = ggplot() +
  coord_map(xlim = c(-65,-62), ylim = c(47.2, 48.8)) +
  
  # land and bathymetry
  geom_polygon(
    data = nam,
    aes(x = long, y = lat, group = group),
    fill = "darkgrey",
    color = NA
  ) +
  geom_contour(
    data = bf,
    aes(x = x, y = y, z = z),
    breaks = c(-50),
    colour = "grey",
    size = 0.3
  ) +
  geom_contour(
    data = bf,
    aes(x = x, y = y, z = z),
    breaks = c(-100),
    colour = "grey",
    size = 1
  ) +
  
  # city labels
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
    size = 3
  ) +
  
  # sonobuoy positions
  geom_point(data = df1,
             aes(x = lon,
                 y = lat,
                 colour = social_bhv_whale)) + #, colour = yday)) +
  #scale_colour_continuous(type = "viridis")+
  # scale_fill_viridis_c(guide = guide_colourbar(barheight = 15,
  #                                              title.theme = element_text(angle=90),
  #                                              title.position = 'right', 
  #                                              title.hjust = 0.5))+
  #scale_fill_continuous(type = "viridis")+
  #scale_colour_viridis(guide='colourbar') +
  scale_colour_viridis() +
  scale_radius(range = c(3, 8)) +
  
  labs(x = "Latitude (W)", 
       y = "Longitude (N)")+
  #guides(colour=guide_legend(title="Rate"))+
  
  # facet by year
  #facet_wrap(~myear)+
  facet_grid(year ~ month) +
  
  # formatting
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = 'bottom',
    legend.direction = "horizontal",
    panel.spacing = unit(2, "lines")
  ) +
  theme(text = element_text(size = 15)) +
  theme(axis.text.x = element_text(colour = "black")) +
  theme(axis.text.y = element_text(colour = "black"))

gsl

ggsave(gsl, filename = paste0(fig_dir,'social_rate_map.png'), 
       height = 10, width = 8, dpi = 300)

# other rate
# city positions
cities = data.frame(
  name = c("Shippagan", "Percé",'OT'),
  lat = c(47.7439, 48.5244,47.90),
  lon = c(-64.7057,-64.2127,-63.90)
)

# gsl map
gsl = ggplot() +
  coord_map(xlim = c(-65,-62), ylim = c(47.2, 48.8)) +
  
  # land and bathymetry
  geom_polygon(
    data = nam,
    aes(x = long, y = lat, group = group),
    fill = "darkgrey",
    color = NA
  ) +
  geom_contour(
    data = bf,
    aes(x = x, y = y, z = z),
    breaks = c(-50),
    colour = "grey",
    size = 0.3
  ) +
  geom_contour(
    data = bf,
    aes(x = x, y = y, z = z),
    breaks = c(-100),
    colour = "grey",
    size = 1
  ) +
  
  # city labels
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
    size = 3
  ) +
  
  # sonobuoy positions
  geom_point(data = df1,
             aes(x = lon,
                 y = lat,
                 colour = other_bhv_whale)) + #, colour = yday)) +
  #scale_colour_continuous(type = "viridis")+
  # scale_fill_viridis_c(guide = guide_colourbar(barheight = 15,
  #                                              title.theme = element_text(angle=90),
  #                                              title.position = 'right', 
  #                                              title.hjust = 0.5))+
  #scale_fill_continuous(type = "viridis")+
  #scale_colour_viridis(guide='colourbar') +
  scale_colour_viridis() +
  scale_radius(range = c(3, 8)) +
  
  labs(x = "Latitude (W)", 
       y = "Longitude (N)")+
  #guides(colour=guide_legend(title="Rate"))+
  
  # facet by year
  #facet_wrap(~myear)+
  facet_grid(year ~ month) +
  
  # formatting
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = 'bottom',
    legend.direction = "horizontal",
    panel.spacing = unit(2, "lines")
  ) +
  theme(text = element_text(size = 15)) +
  theme(axis.text.x = element_text(colour = "black")) +
  theme(axis.text.y = element_text(colour = "black"))

gsl

ggsave(gsl, filename = paste0(fig_dir,'other_bhvrate_map.png'), 
       height = 10, width = 8, dpi = 300)















# OLD
# # gs production rate
# # city positions
# cities = data.frame(
#   name = c("Shippagan", "Percé"),
#   lat = c(47.7439, 48.5244),
#   lon = c(-64.7057, -64.2127)
# )
# 
# # gsl map
# gsl = ggplot() + 
#   coord_map(xlim = c(-65,-62), ylim = c(47.2,48.8))+
#   
#   # land and bathymetry
#   geom_polygon(data = nam, aes(x = long, y = lat, group = group),
#                fill = "darkgrey", color = NA) +
#   # geom_contour(data = bf, aes(x=x, y=y, z=z),
#   #               breaks=c(-50),colour="grey", size=0.3)+
#   # geom_contour(data = bf, aes(x=x, y=y, z=z),
#   #               breaks=c(-100),colour="grey", size=1)+
#   geom_contour(data = bf, aes(x=x,y=y,z=z,
#                               color=as.factor(..level..)), 
#                breaks = c(-50,-100), size=1)+
#   scale_color_brewer(palette = 'Blues',
#                      breaks=c("-50", "-100"),
#                      labels=c("50", "100"))+
#   
#   # city labels
#   geom_point(data = cities, aes(x = lon, y = lat), color = "darkslategrey", size = 1)+
#   geom_text(data = cities, aes(x = lon, y = lat, label = name), color =
#               "darkslategrey", nudge_y = 0.07, size = 3)+
#   
#   # sonobuoy positions
#   geom_point(data = df1, aes(x = lon, y = lat, colour = gs_per_hr_per_whale#, colour = yday
#   ))+
#   scale_radius(range=c(3,8))+
#   
#   # facet by year
#   facet_wrap(~myear)+
#   
#   # formatting
#   ylab("Longitude (N)")+xlab("Latitude (W)")+
#   theme_bw()+
#   theme(legend.title = element_blank(), 
#         legend.position = 'bottom', 
#         legend.direction = "horizontal",
#         panel.spacing = unit(2, "lines"))+
#   theme(text = element_text(size=15))+
#   theme(axis.text.x = element_text(colour = "black"))+
#   theme(axis.text.y = element_text(colour = "black"))
# 
# gsl
# 
# ggsave(gsl, filename = paste0(fig_dir,'gs_prate_map.png'), 
#        height = 10, width = 8, dpi = 300)
# 
# 
# # mf production rate
# # city positions
# cities = data.frame(
#   name = c("Shippagan", "Percé"),
#   lat = c(47.7439, 48.5244),
#   lon = c(-64.7057, -64.2127)
# )
# 
# # gsl map
# gsl = ggplot() + 
#   coord_map(xlim = c(-65,-62), ylim = c(47.2,48.8))+
#   
#   # land and bathymetry
#   geom_polygon(data = nam, aes(x = long, y = lat, group = group),
#                fill = "darkgrey", color = NA) +
#   # geom_contour(data = bf, aes(x=x, y=y, z=z),
#   #               breaks=c(-50),colour="grey", size=0.3)+
#   # geom_contour(data = bf, aes(x=x, y=y, z=z),
#   #               breaks=c(-100),colour="grey", size=1)+
#   geom_contour(data = bf, aes(x=x,y=y,z=z,
#                               color=as.factor(..level..)), 
#                breaks = c(-50,-100), size=1)+
#   scale_color_brewer(palette = 'Blues',
#                      breaks=c("-50", "-100"),
#                      labels=c("50", "100"))+
#   
#   # city labels
#   geom_point(data = cities, aes(x = lon, y = lat), color = "darkslategrey", size = 1)+
#   geom_text(data = cities, aes(x = lon, y = lat, label = name), color =
#               "darkslategrey", nudge_y = 0.07, size = 3)+
#   
#   # sonobuoy positions
#   geom_point(data = df1, aes(x = lon, y = lat, colour = mf_per_hr_per_whale#, colour = yday
#   ))+
#   scale_radius(range=c(3,8))+
#   
#   # facet by year
#   #facet_wrap(~myear)+
#   facet_grid(month~year)+
#   
#   # formatting
#   ylab("Longitude (N)")+xlab("Latitude (W)")+
#   theme_bw()+
#   theme(legend.title = element_blank(), 
#         legend.position = 'bottom', 
#         legend.direction = "horizontal",
#         panel.spacing = unit(2, "lines"))+
#   theme(text = element_text(size=15))+
#   theme(axis.text.x = element_text(colour = "black"))+
#   theme(axis.text.y = element_text(colour = "black"))
# 
# gsl
# 
# ggsave(gsl, filename = paste0(fig_dir,'mf_prate_map.png'), 
#        height = 10, width = 8, dpi = 300)
# 
# 
# # foraging bhv rate
# # city positions
# cities = data.frame(
#   name = c("Shippagan", "Percé"),
#   lat = c(47.7439, 48.5244),
#   lon = c(-64.7057, -64.2127)
# )
# 
# # gsl map
# gsl = ggplot() + 
#   coord_map(xlim = c(-65,-62), ylim = c(47.2,48.8))+
#   
#   # land and bathymetry
#   geom_polygon(data = nam, aes(x = long, y = lat, group = group),
#                fill = "darkgrey", color = NA) +
#   # geom_contour(data = bf, aes(x=x, y=y, z=z),
#   #               breaks=c(-50),colour="grey", size=0.3)+
#   # geom_contour(data = bf, aes(x=x, y=y, z=z),
#   #               breaks=c(-100),colour="grey", size=1)+
#   geom_contour(data = bf, aes(x=x,y=y,z=z,
#                               color=as.factor(..level..)), 
#                breaks = c(-50,-100), size=1)+
#   scale_color_brewer(palette = 'Blues',
#                      breaks=c("-50", "-100"),
#                      labels=c("50", "100"))+
#   
#   # city labels
#   geom_point(data = cities, aes(x = lon, y = lat), color = "darkslategrey", size = 1)+
#   geom_text(data = cities, aes(x = lon, y = lat, label = name), color =
#               "darkslategrey", nudge_y = 0.07, size = 3)+
#   
#   # sonobuoy positions
#   geom_point(data = df1, aes(x = lon, y = lat, colour = foraging_bhv_whale#, colour = yday
#   ))+
#   scale_radius(range=c(3,8))+
#   
#   # facet by year
#   #facet_wrap(~myear)+
#   facet_grid(month~year)+
#   
#   # formatting
#   ylab("Longitude (N)")+xlab("Latitude (W)")+
#   theme_bw()+
#   theme(legend.title = element_blank(), 
#         legend.position = 'bottom', 
#         legend.direction = "horizontal",
#         panel.spacing = unit(2, "lines"))+
#   theme(text = element_text(size=15))+
#   theme(axis.text.x = element_text(colour = "black"))+
#   theme(axis.text.y = element_text(colour = "black"))
# 
# gsl
# 
# ggsave(gsl, filename = paste0(fig_dir,'forage_rate_map.png'), 
#        height = 10, width = 8, dpi = 300)
# 
# 
# # other bhv rate
# # city positions
# cities = data.frame(
#   name = c("Shippagan", "Percé"),
#   lat = c(47.7439, 48.5244),
#   lon = c(-64.7057, -64.2127)
# )
# 
# # gsl map
# gsl = ggplot() + 
#   coord_map(xlim = c(-65,-62), ylim = c(47.2,48.8))+
#   
#   # land and bathymetry
#   geom_polygon(data = nam, aes(x = long, y = lat, group = group),
#                fill = "darkgrey", color = NA) +
#   # geom_contour(data = bf, aes(x=x, y=y, z=z),
#   #               breaks=c(-50),colour="grey", size=0.3)+
#   # geom_contour(data = bf, aes(x=x, y=y, z=z),
#   #               breaks=c(-100),colour="grey", size=1)+
#   geom_contour(data = bf, aes(x=x,y=y,z=z,
#                               color=as.factor(..level..)), 
#                breaks = c(-50,-100), size=1)+
#   scale_color_brewer(palette = 'Blues',
#                      breaks=c("-50", "-100"),
#                      labels=c("50", "100"))+
#   
#   # city labels
#   geom_point(data = cities, aes(x = lon, y = lat), color = "darkslategrey", size = 1)+
#   geom_text(data = cities, aes(x = lon, y = lat, label = name), color =
#               "darkslategrey", nudge_y = 0.07, size = 3)+
#   
#   # sonobuoy positions
#   geom_point(data = df1, aes(x = lon, y = lat, colour = other_bhv_whale#, colour = yday
#   ))+
#   scale_radius(range=c(3,8))+
#   
#   # facet by year
#   #facet_wrap(~myear)+
#   facet_grid(month~year)+
#   
#   # formatting
#   ylab("Longitude (N)")+xlab("Latitude (W)")+
#   theme_bw()+
#   theme(legend.title = element_blank(), 
#         legend.position = 'bottom', 
#         legend.direction = "horizontal",
#         panel.spacing = unit(2, "lines"))+
#   theme(text = element_text(size=15))+
#   theme(axis.text.x = element_text(colour = "black"))+
#   theme(axis.text.y = element_text(colour = "black"))
# 
# gsl
# 
# ggsave(gsl, filename = paste0(fig_dir,'other_bhvrate_map.png'), 
#        height = 10, width = 8, dpi = 300)
# 
# 
# # social bhv rate
# # city positions
# cities = data.frame(
#   name = c("Shippagan", "Percé"),
#   lat = c(47.7439, 48.5244),
#   lon = c(-64.7057, -64.2127)
# )
# 
# # gsl map
# gsl = ggplot() + 
#   coord_map(xlim = c(-65,-62), ylim = c(47.2,48.8))+
#   
#   # land and bathymetry
#   geom_polygon(data = nam, aes(x = long, y = lat, group = group),
#                fill = "darkgrey", color = NA) +
#   # geom_contour(data = bf, aes(x=x, y=y, z=z),
#   #               breaks=c(-50),colour="grey", size=0.3)+
#   # geom_contour(data = bf, aes(x=x, y=y, z=z),
#   #               breaks=c(-100),colour="grey", size=1)+
#   geom_contour(data = bf, aes(x=x,y=y,z=z,
#                               color=as.factor(..level..)), 
#                breaks = c(-50,-100), size=1)+
#   scale_color_brewer(palette = 'Blues',
#                      breaks=c("-50", "-100"),
#                      labels=c("50", "100"))+
# 
#   # city labels
#   geom_point(data = cities, aes(x = lon, y = lat), color = "darkslategrey", size = 1)+
#   geom_text(data = cities, aes(x = lon, y = lat, label = name), color =
#               "darkslategrey", nudge_y = 0.07, size = 3)+
#   
#   # sonobuoy positions
#   geom_point(data = df1, aes(x = lon, y = lat, colour = social_bhv_whale#, colour = yday
#   ))+
#   scale_radius(range=c(3,8))+
#   
#   # facet by year
#   #facet_wrap(~myear)+
#   facet_grid(month~year)+
#   
#   # formatting
#   ylab("Longitude (N)")+xlab("Latitude (W)")+
#   theme_bw()+
#   theme(legend.title = element_blank(), 
#         legend.position = 'bottom', 
#         legend.direction = "horizontal",
#         panel.spacing = unit(2, "lines"))+
#   theme(text = element_text(size=15))+
#   theme(axis.text.x = element_text(colour = "black"))+
#   theme(axis.text.y = element_text(colour = "black"))
# 
# gsl
# 
# ggsave(gsl, filename = paste0(fig_dir,'social_rate_map.png'), 
#        height = 10, width = 8, dpi = 300)
