## proc_map_data_v2 ##
# Read and save bathymetry and coastline data for later use

# input -------------------------------------------------------------------

# map file
map_file = 'data/processed/map.rda'

# download map data from here, then extract the zipped file
# 'https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/gshhg-bin-2.3.7.zip'

# path to coastline data
gshhs_path = '/Users/kfranklin/Projects/narw_sonobuoys/data/processed/noaa_detailed_bathy_coastline/gshhs_f.b'

# define map limits
min_lat = 47.2
min_lon = -65
max_lat = 48.8
max_lon = -62.25

# min_lat = 46.5
# min_lon = -65
# max_lat = 49
# max_lon = -61
# # set lat/lon limits
# min_lat = 46
# max_lat = 51
# min_lon = -67
# max_lon = -58


# bathymetry resolution (arc minutes)
res = 1

# process -----------------------------------------------------------------

#if (!file.exists(map_file)) {
# libraries
library(maptools)
library(marmap)
library(tidyverse)
library(sf)
library(rgdal)
library(rgeos)

# download etopo1 bathymetry data
b = getNOAA.bathy(min_lon, max_lon, min_lat, max_lat, res)

# convert bathymetry to data frame
bf = fortify.bathy(b)

# fix bathy data
bf = bf %>% dplyr::filter(x >= min_lon &
                            x <= max_lon & y >= min_lat & y <= max_lat)
bf$z[bf$z >= 0] = 0
bf$z = abs(bf$z)

# read in coastline data for specific region and convert to sf format
cf = getRgshhsMap(gshhs_path,
                  xlim = c(min_lon, max_lon),
                  ylim = c(min_lat, max_lat)) %>%
  st_as_sf()

# save all data
save(b, cf, bf, min_lon, max_lon, min_lat, max_lat, file = map_file)

#   message('Map data saved at: ', map_file)
# } else {
#   message('Map data exists at: ', map_file)
# }
