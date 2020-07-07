## calculating_recording_durations
# calculating usable time for 2017, and 2018 NOAA so far
# rough approx times for each deployment in seconds
# this does not take into account radio chatter/inteference or times were 
# the recording goes in and out making it difficult to indicate strict start and
# end bounds

# the rds file is made from an excel file which is made from the data below
# in the header called rought data
library(readxl)
duration_time = read_excel('data/interim/duration_times.xlsx')
saveRDS(duration_time, file = 'data/interim/duration_times.rds')


# rough data -------------------------------------------------------------

total_2017_times=(7051.414-26.810)+
  (7539.937-7532.747)+
  (13382.223-11028.676)+
  (6625.571-28.229)+
  (7027.226-6986.833)+
  (7344.810-7231.905)+
  (10437.821-7621.905)+
  (10556.204-9.219)+
  (21969.905-12553.558)+
  (8280.361-35.990)+
  (17149.046-8858.751)+
  (11235.765-11.510)+
  (9883.781-29.271)+
  (6559.795-26.667)+
  (11674.400-6920.243)+
  (10688.721-2.448)+
  (11581.426-11049.150)
total_2017_times
total_2017_times/60/60

total_2018_noaa_times=(8313.302-1.146)+
  (8885.786-47.262)+
  (12672.997-39.695)+
  (12398.352-43.762)+
  (5780.240-33.000)+(18349.381-9007.740)+
  (9007.342-31.524)+
  (9900.705-28.893)+
  (4804.929-403.738)+(8752.405-6376.643)+
  (11832.623-31.095)+
  (8828.505-22.857)+
  
  (242.067-54.571)+
  (10926.042-18.381)+
  (12526.643-0.238)
total_2018_noaa_times
total_2018_noaa_times/60/60

# to see how many of each call for whole year
acoustic_selection_2017 = readRDS('data/2017/noaa/processed/selections.rds')
table(unlist(acoustic_selection_2017$call_type))


