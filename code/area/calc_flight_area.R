## calc_flight_area ##
# calculate area covered by survey flight

# input -------------------------------------------------------------------

# track file
tfile = 'wrk/area/data/Kim Franklin NEAQ&NEFSC 2017-2019 GoStL effort & cetaceans 05-22-2020.xlsx - KIM_0522.csv'

# sono file
sfile = 'wrk/area/data/proc_acou_photoid.rds'

# plot directory
pdir = 'wrk/area/plots/'

# output file
ofile = 'wrk/area/area.rds'

# sonobuoy distance (km)
max_dist = 30

# buffer distance (radial; km)
bdist = 1.5

# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(sf)

# define CRS
ll_crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
xy_crs = "+proj=utm +zone=20 +datum=WGS84"

# create plot directory
if(!dir.exists(pdir)){dir.create(pdir, recursive = TRUE)}

# process -----------------------------------------------------------------

# read in sono file
son = readRDS(sfile) %>%
  as_tibble()

# read track file
trk = read_csv(tfile)

# format time
trk$time = as.POSIXct(paste0(trk$YEAR,'-',trk$MONTH,'-',trk$DAY,' ', trk$`TIME(EST)`), 
                      format = '%Y-%m-%d %H%M%S', tz = 'America/New_York')
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
  stime = ison$datetime
  
  # dep end time
  etime = stime+ison$dep_duration
  
  # buoy lon
  blon = ison$lon
  
  # buoy lat
  blat = ison$lat
  
  # compute track distances to buoy
  trk$dist = geodDist(longitude1 = trk$LONGITUDE, latitude1 = trk$LATITUDE, longitude2 = blon, latitude2 = blat, alongPath = F)
  
  # restrict tracks to time
  itrk = trk %>% filter(time >= stime & time <= etime) %>% drop_na(LONGITUDE, LATITUDE)
  
  # create point for sono
  sf_ison = st_sfc(st_point(cbind(ison$lon,ison$lat)), crs = ll_crs)
  
  # create spatial line for trackline
  sf_itrk = st_sfc(st_linestring(cbind(itrk$LONGITUDE,itrk$LATITUDE)), crs = ll_crs)
  
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
