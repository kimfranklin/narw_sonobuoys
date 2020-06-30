## site_map ##
# Map of nomans study site

# input -------------------------------------------------------------------

fig_dir = 'figures/'
map_data = 'cache/map_data.rda'
mooring_positions = 'cache/mooring_positions.rds'

lat_buff = 1.4
lon_buff = 1.9

# setup -------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
if(!dir.exists(fig_dir)){dir.create(fig_dir,recursive = TRUE)}

# process -----------------------------------------------------------------

# read in map data
load(map_data)

# get vla position
pos = readRDS(mooring_positions) %>%
  filter(platform == 'vla')

# define map limits
min_lat = pos$lat - lat_buff
min_lon = pos$lon - lon_buff
max_lat = pos$lat + lat_buff
max_lon = pos$lon + lon_buff
min_z = -500
max_z = 0

# define boston lat/lon
boston = data.frame(
  lat = 42.3601,
  lon = -71.0589
)

# subset
ggb = filter(bf, x>= min_lon & x<= max_lon & y>= min_lat & y<= max_lat & z < max_z & z > min_z) %>%
  mutate(z = abs(z))

# plot
mp = ggplot()+
  geom_polygon(data = nam, aes(x = long, y = lat, group = group), 
               fill = "grey", size = 0.3, color = 'darkgrey') +
  geom_contour(data=ggb, aes(x=x,y=y,z=z,color=as.factor(..level..)), breaks = c(25,50,200))+
  scale_color_brewer(palette = 'Blues')+
  geom_point(data = pos, aes(x = lon, y = lat), shape = 21, fill = 'red', size = 3) +
  geom_point(data = boston, aes(x = lon, y = lat), color = 'darkslategrey', size = 1.5, shape = 8) +
  geom_text(data = boston, aes(x = lon, y = lat, label = 'Boston'), nudge_x = -0.3, 
            color = 'darkslategrey', size = 4, family = "Times New Roman") +
  coord_quickmap(xlim = c(min_lon,max_lon), ylim = c(min_lat,max_lat), expand = 0)+
  scale_x_continuous(sec.axis = dup_axis(name = NULL)) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
  labs(x = 'Longitude', y = 'Latitude', color = 'Depth (m)')+
  theme_bw()+
  theme(legend.position = c(0, 0),legend.justification = c(0,0),
        text = element_text(size=14, family = "Times New Roman"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.background = element_rect(fill="white", size=0.5, color = 'black'))

# save
ggsave(filename = paste0(fig_dir, '/site_map.jpg'), plot = mp, width = 5, height = 5, dpi = 300)
