## proc_sonobuoy_sightings ##
# find nearby sightings/effort for each deployment and plot a map

# user input --------------------------------------------------------------

# sonobuoy log
sono_file = 'data/interim/sono_log.rds'

# observation file
obs_file = '~/Projects/WhaleMap/data/processed/observations.rds'

# tracks file
trk_file = '~/Projects/WhaleMap/data/processed/tracks.rds'

# outfile
ofile = 'data/processed/sono_sightings.rds'

# distance threshold (km)
dmax = 15

# plot?
plot_maps = TRUE

# setup -------------------------------------------------------------------

library(oce)
library(ggplot2)
source('src/functions.R')

# read in data
sono = readRDS(sono_file)
obs = readRDS(obs_file)
trk = readRDS(trk_file)

# process -----------------------------------------------------------------

# subset obs
obs = subset(obs, species == 'right' & score %in% c('definite visual', 'definite acoustic'))

# remove failed deployments
sono = subset(sono, deploy_success == 'yes')

# initialize data columns
sono$sightings_radius = dmax
sono$sightings = 0

# loop and plot
for(i in 1:nrow(sono)){
  
  # get drop data
  idrop = sono[i,]
  
  # subset by date
  iobs = subset(obs, date == idrop$date)
  itrk = subset(trk, date == idrop$date)
  
  # calculate distance to drop
  iobs$dist = geodDist(longitude1 = iobs$lon, latitude1 = iobs$lat, 
                       longitude2 = idrop$lon, latitude2 = idrop$lat, alongPath = FALSE)
  itrk$dist = geodDist(longitude1 = itrk$lon, latitude1 = itrk$lat, 
                       longitude2 = idrop$lon, latitude2 = idrop$lat, alongPath = FALSE)
  
  # subset observations by distance
  # iobs = subset(iobs, dist <= dmax)
  # itrk = subset(itrk, dist <= dmax)
  
  # convert to local coordinates
  iobs = cbind(iobs, geodXy(longitude = iobs$lon, latitude = iobs$lat, 
                            longitudeRef = idrop$lon, latitudeRef = idrop$lat)/1000)
  itrk = cbind(itrk, geodXy(longitude = itrk$lon, latitude = itrk$lat, 
                            longitudeRef = idrop$lon, latitudeRef = idrop$lat)/1000)
  
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
    
    with(subset(itrk, name == 'jdmartin'),
         lines(x, y, col = 'blue')
    )
    
    with(subset(iobs, name == 'jdmartin'),
         points(x, y, bg = 'blue', pch = 21)
    )
    
    # determine gliders deployments
    igld = subset(itrk, platform == 'slocum')
    
    for(idep in unique(igld$id)){
      
      with(subset(itrk, id == idep),
           lines(x, y, col = 'red')
      )
      
      with(subset(iobs, id == idep),
           points(x, y, bg = 'red', pch = 21)
      )
      
    }
    
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

# save
saveRDS(sono, file = ofile)

