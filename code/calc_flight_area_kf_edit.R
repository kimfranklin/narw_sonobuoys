## calc_flight_area ##
# calculate area covered by survey flight

# input -------------------------------------------------------------------

# track file
#tfile = 'data/raw/visual/sightings/Kim Franklin NEAQ&NEFSC 2017-2019 GoStL effort & cetaceans 05-22-2020.xlsx - KIM_0522.csv'
tfile = 'data/raw/acoustic/position/noaa_sono_tracks_whalemap.csv'

# sono file
sfile = readRDS('data/processed/proc_acou_photoid.rds')
#sfile = readRDS('data/processed/log.rds')

# plot directory
pdir = 'code/area/plots_kf_edited_timetest/'

# output file
ofile = 'code/area/area_kf_edited_timetest.rds'

# sonobuoy distance (km)
max_dist = 30

# buffer distance (radial; km)
bdist = 1.5

# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(sf)
library(oce)

# define CRS
ll_crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
xy_crs = "+proj=utm +zone=20 +datum=WGS84"

# create plot directory
if(!dir.exists(pdir)){dir.create(pdir, recursive = TRUE)}

# process -----------------------------------------------------------------

# # read in sono file
# son = readRDS("sfile") %>%
#   as_tibble()
son = as_tibble(sfile)

# read track file
trk = read_csv(tfile)

# change 2019 sono times back to local times
tmp = son %>%
  filter(year == 2019)

tmp$datetime = tmp$datetime -10800

son <- son[-c(22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37), ]

son = rbind(son, tmp)

# son$datetime = son$datetime - 10800

# format time
# trk$time = as.POSIXct(paste0(trk$YEAR,'-',trk$MONTH,'-',trk$DAY,' ', trk$`TIME(EST)`), 
#                       format = '%Y-%m-%d %H%M%S', tz = 'America/New_York')
trk$time = with_tz(trk$time, tzone = 'UTC')


# initialize new columns
son$visual_area_surveyed_m2 = NA
son$acoustic_area_surveyed_m2 = NA
son$proportion_surveyed = NA

# loop through all deployments
for(ii in 1:nrow(son)){
  
  # isolate deployment
  ison = son[ii,]
  
  # dep start time
  stime = ison$datetime -3600
  
  # dep end time
  etime = stime+ison$dep_duration +3600
  
  # buoy lon
  blon = ison$lon
  
  # buoy lat
  blat = ison$lat
  
  # compute track distances to buoy
  trk$dist = geodDist(longitude1 = trk$lon, latitude1 = trk$lat, longitude2 = blon, latitude2 = blat, alongPath = F)
  
  # restrict tracks to time
  itrk = trk %>% filter(time >= stime & time <= etime) %>% drop_na(lon, lat)
  
  # create point for sono
  sf_ison = st_sfc(st_point(cbind(ison$lon,ison$lat)), crs = ll_crs)
  
  # create spatial line for trackline
  sf_itrk = st_sfc(st_linestring(cbind(itrk$lon,itrk$lat)), crs = ll_crs)
  
  # transform to local coords (project)
  sf_itrk_xy = st_transform(sf_itrk, crs = xy_crs)
  sf_ison_xy = st_transform(sf_ison, crs = xy_crs)
  
  # compute buffers
  sf_itrk_buff = st_buffer(sf_itrk_xy, bdist*1e3)
  sf_ison_buff = st_buffer(sf_ison_xy, max_dist*1e3)
  
  # restrict trackline to within sono buffer
  # sf_itrk_xy = st_intersection(sf_itrk_xy,sf_ison_buff) # use for trackline
  sf_itrk_buff = st_intersection(sf_itrk_buff,sf_ison_buff)
  
  # catch error
  if(length(sf_itrk_buff)==0){
    message('No survey effort found for depoyment: ', ison$id)
    next
  }
  
  # compute areas
  itrk_buff_area = st_area(sf_itrk_buff)
  ison_buff_area = st_area(sf_ison_buff)
  
  # compute proportion surveyed
  p_surveyed = round(as.numeric(itrk_buff_area / ison_buff_area), 2)
  
  # plot to check
  plt = ggplot()+
    geom_sf(data = sf_itrk_buff)+
    geom_sf(data = sf_itrk_xy, alpha = 0.3)+
    geom_sf(data = sf_ison_buff, color = 'red', fill = NA, alpha = 0.3)+
    geom_sf(data = sf_ison_xy, fill = 'red', shape = 21)+
    labs(subtitle = paste0('ID: ', ison$id, '\nProportion surveyed: ', p_surveyed))+
    theme_bw()
  
  # save plot
  ggsave(plt, filename = paste0(ison$id, '.png'), path = pdir, 
         width = 5, height = 5, units = 'in')
  
  # store values
  son$visual_area_surveyed_m2[ii] = as.numeric(itrk_buff_area)
  son$acoustic_area_surveyed_m2[ii] = as.numeric(ison_buff_area)
  son$proportion_surveyed[ii] = as.numeric(p_surveyed)
  
}

# write output data
saveRDS(object = son, file = ofile)
