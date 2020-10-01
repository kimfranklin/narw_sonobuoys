## ex_target_plot ##
# quick example of how to produce a single 'target' plot

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
dep = readRDS('data/processed/all_noaa_acoustic.rds') %>%
  group_by(id) %>%
  summarize(time = unique(datetime_UTC),
            duration = as.numeric(unique(duration)),
            lat = unique(lat),
            lon = unique(lon))

# process -----------------------------------------------------------------

# choose a specific deployment
idep = dep[1,]

# subset obs to deployment date
iobs = obs %>% filter(date == as.Date(idep$time))

# compute distance to sonobuoy
iobs$dist = geodDist(longitude1 = iobs$lon, latitude1 = iobs$lat, 
         latitude2 = idep$lat, longitude2 = idep$lon)

# compute local coordinates of sightings
coords = geodXy(longitude = iobs$lon, latitude = iobs$lat, 
                longitudeRef = idep$lon, latitudeRef = idep$lat)/1e3
iobs$x = coords$x
iobs$y = coords$y

# compute the time/space subset (column indicates if a sighting is included or not)
iobs$include = iobs$datetime >= idep$time - t & iobs$datetime <= idep$time+idep$duration+t & iobs$dist <=r

# make circle
cr = make_circle(r=r)

# plot
ggplot()+
  geom_point(aes(x=0,y=0), shape = 4)+
  geom_path(data=cr,aes(x=x,y=y))+
  geom_point(data=iobs, aes(x=x,y=y,fill=include), shape=21, alpha = 0.8)+
  scale_fill_manual(values=c("TRUE" = 'grey', "FALSE" = 'red'))+
  labs(x='Easting (km)',y='Northing (km)',fill='Include?',title=idep$id)+
  coord_equal()+
  theme_bw()


