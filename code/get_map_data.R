## get_map_data ##
# Read and save bathymetry data from NOAA server

# map file
map_file = 'data/map_data.rda'

# Grab data from NOAA server ----------------------------------------------

if (!file.exists(map_file)) {
  
  # useful libraries
  library(maps)
  library(mapdata)
  library(marmap)
  library(ggplot2)
  data(coastlineWorldFine, package="ocedata")
  
  # set lat/lon limits
  min_lat = 40
  max_lat = 55
  min_lon = -70
  max_lon = -55
  
  # bathymetry data
  b = getNOAA.bathy(min_lon,max_lon,min_lat,max_lat,1) 
 
  # convert bathymetry to data frame
  bf = fortify.bathy(b)
  
  # get regional polygons
  w2hr = map_data("world2Hires")
  
  # convert lat longs
  w2hr$long = (360 - w2hr$long)*-1
  
  # determine polygons in region
  ins = subset(w2hr,
         long >= min_lon & long <= max_lon &
         lat >= min_lat & lat <= max_lat)
  
  # subset
  nam = subset(w2hr, group %in% ins$group)
  
  # save all data
  save(b,
       nam,
       bf,
       file = map_file)
  
  message('Map data saved at: ', map_file)
  
} else {
  message('Using map data from: \n', map_file)
  load(map_file)
}
