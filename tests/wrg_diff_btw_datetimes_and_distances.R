
library(dplyr)
library(oce)

df = readRDS("data/processed/proc_acou_photoid.rds")


diff(df$datetime)

#geodDist(df$lon, df$lat)

geodDist(lag(df$lon), lag(df$lat),df$lon, df$lat,alongPath = FALSE)
