## track_maps.R ##

# maps with plane track lines, sono drop, and photoid sightings


# libraries ---------------------------------------------------------------

library(tidyverse)
library(RColorBrewer)
library(oce)
library(mapproj)

# setup -------------------------------------------------------------------

# read in map data
load('data/processed/map_data.rda')

# read in the data
trk = read.csv('data/raw/visual/sightings/noaa_sono_tracks_whalemap.csv')
pid = readRDS('data/processed/all_noaa_photoid.rds')
log = readRDS('data/processed/log.rds')

# make folder for these images if it does not already exist
if (!dir.exists('figures/track_map_sight')) {
  dir.create('figures/track_map_sight')
}

# process -----------------------------------------------------------------

# loop to combine data and create maps for all sonobuoy deployments
for (i in 1:nrow(log)) {
  # loop for each line in log
  ilog = log[i, ]
  
  # identify which track line data want to use
  itrk = trk %>%
    filter(date == as.Date(ilog$date))
  
  # subset the track line for line on during sonobuoy duration
  itrks = trk %>%
    filter(time >= ilog$datetime &
             time <= ilog$datetime + ilog$duration)
  
  # identify the photo-id data and filter by date
  ipid = pid %>%
    filter(date == as.Date(ilog$date))
  
  # create maps
  gsl = ggplot() +
    coord_map(xlim = c(-65, -62.1), ylim = c(46, 48.75)) +
    
    #land and bathymetry
    geom_polygon(
      data = nam,
      aes(x = long, y = lat, group = group),
      fill = "darkgrey",
      color = NA
    ) +
    # geom_contour(data = bf, aes(x=x, y=y, z=z),
    #              breaks=c(-50),colour="grey", size=0.3)+
    # geom_contour(data = bf, aes(x=x, y=y, z=z),
    #               breaks=c(-100),colour="grey", size=1)+
    geom_contour(
      data = bf,
      aes(
        x = x,
        y = y,
        z = z,
        color = as.factor(..level..)
      ),
      breaks = c(-50, -100),
      size = 0.25
    ) +
    scale_color_brewer(
      palette = 'Spectral',
      breaks = c("-50", "-100"),
      labels = c("50", "100")
    ) +
    
    # city labels
    # geom_point(data = cities, aes(x = lon, y = lat), color = "darkslategrey", size = 1)+
    # geom_text(data = cities, aes(x = lon, y = lat, label = name), color =
    #             "darkslategrey", nudge_y = 0.07, size = 4, family = "serif")+
    
    # track line
    geom_path(data = itrk,
              aes(x = lon, y = lat),
              linetype = 'dotted') +
    
    # track line for sonobuoy duration
    geom_path(data = itrks,
              aes(x = lon, y = lat)) +
    scale_shape_identity() +
    
    # whale positions
    geom_point(data = ipid,
               aes(
                 x = lon,
                 y = lat,
                 shape = 1,
                 colour = 'darkblue'
               )) +
    
    # sonobuoy positions
    geom_point(data = ilog,
               aes(
                 x = lon,
                 y = lat,
                 shape = 0,
                 colour = 'green'
               ),
               size = 2) +
    
    # formatting
    labs(x = "Longitude",
         y = "Latitude",
         col = "Depth (m)") +
    ggtitle(
      'green hollow circles are whale sightings,
    blue hollow square is sonobuoy location,
    dotted line is NOAA plane track line,
    solid line is NOAA plane track
    line for the time the sono was recording'
    )
  
  theme_bw() +
    theme(#legend.title = element_blank(),
      #legend.position = c(0, 0),
      #legend.justification = c(0,0),
      legend.position = 'right',
      #legend.direction = "horizontal"
      panel.spacing = unit(2, "lines")) +
    # legend.background = element_rect(fill="white",
    #                                  size=0.5,
    #                                  color = 'black'))+
    theme(text = element_text(size = 15, family = "serif")) +
    theme(axis.text.x = element_text(colour = "black")) +
    theme(axis.text.y = element_text(colour = "black"))
  
  # save the plots
  ggsave(
    filename = paste0('figures/track_map_sight/', ilog$id, '.png'),
    height = 5,
    width = 8,
    units = 'in',
    dpi = 300
  )
  
}
