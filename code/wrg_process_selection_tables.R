## process_selection_tables.R ##
# combing all yearly, platform (NOAA) data and looking at what we actually have


# input -------------------------------------------------------------------

# defines input directory
idir = 'data/raw/acoustic/2019/noaa/processed/'

# defines output directory
odir = 'data/interim/'

# defining characteristic for output file
char = 'noaa_2019_'

# setup -------------------------------------------------------------------

library(tidyverse)

# saving output file for selection tables 
ofile = paste0(odir, char, 'selections.rds')

# process -----------------------------------------------------------------

# list data files
flist = list.files(path = idir, pattern = '*_selections.txt$', full.names = TRUE)

# define empty list to hold selections
DF = vector('list',length = length(flist))

# loop through and read in selection tables
for(ii in seq_along(flist)){
  
  # define input file
  ifile = flist[ii]
  
  # read selectin table
  tmp = read.delim(file = ifile, sep = '\t', as.is = TRUE)
  
  # isolate deployment name
  dname = gsub(pattern = '_selections.txt', replacement = '', x = basename(ifile))
  
  # adding column of deployment information
  tmp$sono_id = dname
  
  # add selection table to list
  DF[[ii]] = tmp
}

# combine all selection tables into one df
df = bind_rows(DF)

# isolate platform name
#platform = strsplit(idir,"/")[[1]][4]
pname = as.character(strsplit(idir,"/")[[1]][5])

# add column with platform name
#cbind(df, platform)
df$platform = pname

# isolate year from path
yname = as.character(strsplit(idir,"/")[[1]][4])

# add column with year
df$year = yname

# editing the id column (this is to match the logs formating)
df$id = paste0(df$year,'_', df$platform,'_', df$sono_id)

# remove deployments that have no useful information
#df = df[!(df$id=="2017_noaa_DEP17"),]
#df = df[!(df$id=="2018_noaa_DEP07"),]
#df = df[!(df$id=="2018_noaa_DEP13"),]
#df = df[!(df$id=="2018_noaa_DEP14"),]
df = df[!(df$id=="2019_noaa_DEP75b"),]
##df = df[!(df$id=="2019_noaa_DEP77"),]

# save data frame
saveRDS(df, file = ofile)
