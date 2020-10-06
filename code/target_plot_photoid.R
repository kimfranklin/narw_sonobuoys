## target_plot_photoid ##
# produce 'target' circle plots for all deployments using photo-id data

# define max radius (km) for associating sightings with a sonobuoy
r = 10

# define time offset (s) for associating sightings with a sonobuoy
t = 1*60*60

# setup -------------------------------------------------------------------

library(tidyverse)
library(oce)
source('code/functions.R')

# read in photoid data
obs = readRDS('data/processed/all_noaa_photoid.rds')

# read in acoustic data and pull time/position of each deployment
# dep = readRDS('data/processed/all_noaa_acoustic.rds') %>%
#   group_by(id) %>%
#   summarize(time = unique(datetime),
#             duration = as.numeric(unique(duration)),
#             lat = unique(lat),
#             lon = unique(lon))
log = readRDS('data/processed/log.rds')

# process -----------------------------------------------------------------

# if directory does not exist make sure it does
lapply(state.name, function(x) if(!dir.exists('figures/target_plot_photoid')) dir.create('figures/target_plot_photoid'))

# choose a specific deployment

for(i in 1:nrow(log)){
  ilog = log[i,]
# subset obs to deployment date
iobs = obs %>% filter(date == as.Date(ilog$datetime))

# compute distance to sonobuoy
iobs$dist = geodDist(longitude1 = iobs$lon, latitude1 = iobs$lat, 
         latitude2 = ilog$lat, longitude2 = ilog$lon)

# compute local coordinates of sightings
coords = geodXy(longitude = iobs$lon, latitude = iobs$lat, 
                longitudeRef = ilog$lon, latitudeRef = ilog$lat)/1e3
iobs$x = coords$x
iobs$y = coords$y

# compute the time/space subset (column indicates if a sighting is included or not)
iobs$include = iobs$datetime >= ilog$datetime - t & iobs$datetime <= ilog$datetime+ilog$duration+t & iobs$dist <=r

# make circle
cr = make_circle(r=r)

# plot
ggplot()+
  geom_point(aes(x=0,y=0), shape = 4)+
  geom_path(data=cr,aes(x=x,y=y))+
  geom_point(data=iobs, aes(x=x,y=y,fill=include), shape=21, alpha = 0.8)+
  scale_fill_manual(values=c("TRUE" = 'grey', "FALSE" = 'red'))+
  labs(x='Easting (km)',y='Northing (km)',fill='Include?',title=ilog$id)+
  coord_equal()+
  theme_bw()

ggsave(filename = paste0('figures/target_plot_photoid/',ilog$id, '.png'), 
       height = 5, width = 8, units = 'in', dpi = 300)

}
