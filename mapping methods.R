# the differnet types of glider maps can make

# first need the data,
load(file ='rda files/ PICK A FILE')

# ---- the full deployment map ----
library(oce)
require(oce)
library(ncdf4)
library(ocedata)
data("coastlineWorldFine")
source('processing data/glider_functions.R')

data_dir='raw data/'
proc_dir='processing data/'
deployment_id = deploymentid

glider=get_glider_data(deployment_id=deployment_id, 
                       data_dir = data_dir, 
                       proc_dir = proc_dir)

plot(coastlineWorldFine, col = 'grey',
     clon = mean(glider$lon, na.rm = TRUE),
     clat = mean(glider$lat, na.rm = TRUE), 
     span = 200, 
     projection = "+proj=merc")
mapLines(glider$lon, glider$lat, col = 'blue')
txt = paste0(min(glider$time), ' to ', max(glider$time))
mtext(text = txt, side = 3, line = 0, adj = 0, cex = 0.8)

# if the margins are off/ too large try 
par(mar=c(1,1,1,1))
par(mar=c(5.1,4.1,4.1,2.1))

# ---- the full deployment with the subsetted time track line -----
library(oce)
library(ggplot2)
source('processing data/glider_functions.R')

ggplot()+
  
  # add line for full deployment
  geom_path(data = df, aes(x = lon, y = lat),
            colour = "blue", alpha = 1)+
  
  # add line for subset
  geom_path(data = dfs, aes(x = lon, y = lat),
            colour = "red", alpha = 1, size=2)+
  
  # configure projection and plot domain
  coord_quickmap()+
  
  # formatting
  ylab("")+xlab("")+
  theme_bw()

# ---- just the track line of the subset with coords in lon/lat ----
library(oce)
library(ggplot2)

ggplot(dfs)+
  geom_path( aes(x=lng,y=lat),colour='red')+
  labs(x='Longitude', y='Latitude')+
  coord_quickmap()+
  theme_bw() +
  
  # to add the date sub heading
  ylab('Longitude')+xlab('Latitude')+
  labs(subtitle = paste0((t0), ' to ', (t1)), x = 'Longitude', y = 'Latitude') +
  theme_minimal()

# ---- just the track line of the subset with coords in km ----
library(oce)
library(ggplot2)

ggplot(dfs)+
  geom_path( aes(x=lon,y=lat),colour='red')+
  labs(x='Longitude', y='Latitude')+
  coord_quickmap()+
  theme_bw()

# compute xy distances (in meters) relative to initial point
tmp = geodXy(longitude = dfs$lon, latitude = dfs$lat, longitudeRef = df$lon[1], latitudeRef = df$lat[1])/1e3 
ndf = cbind.data.frame(dfs,tmp) 

# plot data in local coords
ggplot(ndf)+
  geom_path(aes(x=x,y=y),colour='blue')+
  labs(x='Easting [km]', y='Northing [km]')+
  coord_quickmap()+
  theme_bw() +
  
  # time stamp
  ylab('Easting [km]')+xlab('Northing [km]')+
  labs(subtitle = paste0((t0), ' to ', (t1)), x = 'Easting [km]', y = 'Northing [km]') +
  theme_minimal()