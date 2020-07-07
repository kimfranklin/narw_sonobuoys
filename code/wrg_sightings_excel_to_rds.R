## sightings_excel_to_rds ##

library(readxl)

#sightings information
sightings = read_excel('data/sightings_data.xlsx')
saveRDS(sightings, file = 'data/sightings_data.rds')
sightings = readRDS('data/sightings_data.rds')
