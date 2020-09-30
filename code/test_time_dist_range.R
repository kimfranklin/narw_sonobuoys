## test_time_dist_range.R ##
# RANGING, target practice, trying to see (testing) what time and distance 
# bounds should be used around the deployed sonobuoys 

# libraries
library(lubridate)
library(stringr)
library(oce)
library(ggplot2)
source('code/functions_updated.R')
library(tidyverse)

# distance and time variables
# maximum km
dmax = 15

# time before and after duration
t_buffer = 60*60*1

# plot?
plot_maps = TRUE

# read in the data
# sighting effort data
sight_df = read.csv("data/raw/visual/sightings/Kim Franklin NEAQ&NEFSC 2017-2019 GoStL effort & cetaceans 05-22-2020.xlsx - KIM_0522.csv")

# complete acoustic data
acou_df = readRDS('data/processed/all_noaa_acoustic.rds')


# process

# add datetime column for sightings data
tmp = paste0(sight_df$YEAR,"-",sight_df$MONTH,"-",sight_df$DAY," ",sprintf("%06.0f", sight_df$TIME.EST.))
sight_df$datetime = as.POSIXct(tmp, format = "%Y-%m-%d %H%M%S", tz = 'EST')

# add date column to sightings data
tmp = paste0(sight_df$YEAR,"-",sight_df$MONTH,"-",sight_df$DAY)
sight_df$date = as.Date(tmp)

# change sighting data time to UTC
sight_df$datetime = as.POSIXct(sight_df$datetime, tz = "EST")
attributes(sight_df$datetime)$tzone = "UTC"

# identifying unique deployment dates
tmp = as.Date(acou_df$date)
acou_df$date = tmp
dep_dates = unique(acou_df$date)  

# subset sightings df to only consider dates on which sonobuoys were deployed
#sightings_dep = sight_df %>% filter(date %in% dep_dates)

# change duration to numeric
tmp = as.numeric(acou_df$duration)
acou_df$duration = tmp

# subsetting acoustic for combining data
dep_df = acou_df %>% 
  filter(call_type == 'START') %>%
  transmute(
    id = id,
    lat= lat,
    lon = lon, 
    date = date, 
    datetime = datetime_UTC,
    deplyoment_duration = (duration)
  )

# remove duplicated rows
dep_df = dep_df[!duplicated(dep_df$id),]


# loop in time and space
DF = vector('list', length = nrow(dep_df))
for(ii in 1:nrow(dep_df)){
  
  # get acoustic data
  ilat = dep_df$lat[ii]
  ilon = dep_df$lon[ii]
  itime = dep_df$datetime[ii]
  idur = dep_df$deplyoment_duration[ii]
  idep = dep_df$id[ii]
  idate = dep_df$date[ii]
  
  # add deployment id
  #idf = sightings_dep
  #idf$id = idep

  # subset by time
  
  idf = sight_df %>% 
    filter(datetime > itime-t_buffer & datetime < itime+idur+t_buffer)
  
  # subset by space

  # make new column to calculate distance from the buoy to maximum distance away
  # a whale might be 
  idf$dist = geodDist(longitude2 = ilon, latitude2 = ilat, 
                      longitude1 = idf$LONGITUDE, latitude1 = idf$LATITUDE, alongPath = FALSE)
  
  # only sightings with in our specified range (defined as dmax)
  idf = idf %>%
    filter(dist<=dmax)
  
  # identify duplicates
  idf$dup = duplicated(idf$datetime)
  
  # store output
  DF[[ii]] = idf 
  
  message('Done ', ii)
}

# flatten list to data frame
df = bind_rows(DF)

# remove sightings not in same day - nope all same day thus the loop is double counting
tmp = df %>%
  filter(date %in% dep_dates)
tmp2 = sight_df %>%
  filter(date %in% dep_dates)
  
# remove dead whale
#df = df[-grep("FLTG DEAD, TELBUOY", df$behaviour),]

# remove deployments that have no useful information
#df = df[!(df$id=="2017_noaa_DEP17"),]
#df = df[!(df$id=="2018_noaa_DEP07"),]
#df = df[!(df$id=="2018_noaa_DEP13"),]
#df = df[!(df$id=="2018_noaa_DEP14"),]










# add this to loop!!! THE PLOTS
if(plot_maps){
  # plot file
  pdf(paste0('figures/single_drops/', idrop$sono_id, '.pdf'), width = 5, height = 5)
  
  # create plot  
  plot(0,0,
       xlim = c(-dmax,dmax), ylim = c(-dmax,dmax), 
       xlab = 'Eastings [km]', ylab = 'Northings [km]')
  grid()
  
  with(subset(itrk, name == 'noaa_twin_otter'),
       lines(x, y, col = 'grey')
  )
  
  with(subset(iobs, name == 'noaa_twin_otter'),
       points(x, y, bg = 'grey', pch = 21)
  )
  
  # with(subset(itrk, name == 'jdmartin'),
  #      lines(x, y, col = 'blue')
  # )
  #
  # with(subset(iobs, name == 'jdmartin'),
  #      points(x, y, bg = 'blue', pch = 21)
  # )
  
  # determine gliders deployments
  # igld = subset(itrk, platform == 'slocum')
  
  # for(idep in unique(igld$id)){
  #   
  #   with(subset(itrk, id == idep),
  #        lines(x, y, col = 'red')
  #   )
  #   
  #   with(subset(iobs, id == idep),
  #        points(x, y, bg = 'red', pch = 21)
  #   )
  #   
  # }
  # 
  # filled circle
  plot_filled_circle(r = dmax)
  
  # add point
  points(0,0,pch = 16, col = 'yellow')
  points(0,0,pch = 10, col = 'darkslategrey')
  
  # total sightings
  sig = subset(iobs, score == 'definite visual' & dist <= dmax)
  n_sig = sum(sig$number, na.rm = TRUE)
  
  # add text
  mtext(paste0('Sightings within ', dmax, ' km: ', n_sig), side = 3, adj = 0, cex = 0.8)
  mtext(paste0(idrop$date), side = 3, adj = 1, cex = 0.8)
  title(idrop$sono_id)
  
  # add legend
  legend('topright', cex = 0.7, bg = 'white',
         lty = c(1,1,1), 
         pch = c(21,21,21), 
         pt.bg = c('grey', 'blue', 'red'), 
         col = c('grey', 'blue', 'red'), 
         c('plane', 'vessel', 'glider')
  )
}

# save output data
sono$sightings[i] = n_sig

dev.off()
}

















# 1. plot sonobuoy location on map (acou)
# 2. subset for distance (make this a fluid variable)(sight)
# 3. using the sono point's time (acou) subset for time (make this a fluid variable)(sight)
# 4. put whale sightings in this subset on map (sight)
# 5. put track lines in the subset on map (sight)
# 6. repeat for all sonobuoy locations