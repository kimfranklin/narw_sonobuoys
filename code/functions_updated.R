## functions ##

make_circle = function(x = 0, y = 0, r = 1, #fill_col = 'grey', 
                       npoints = 100#, nscale = 1.25
                       ){
  
  # create circle
  tt = seq(0,2*pi,length.out = npoints)
  xx = x + r * cos(tt)
  yy = y + r * sin(tt)
  
  # store coordinates
  coords = data.frame(x = xx, y = yy)
  
  # fill to corners
  #dd = r*nscale
  #tmp = rbind(c(dd,-dd), c(-dd,-dd),c(-dd,dd), c(dd,dd),c(dd,-dd), coords)
  
  # create polygon
  #polygon(tmp, border = NA, col = fill_col)
  
  return(coords)
}

read_selection_table = function(ifile, platform){
  # read selection table from raven
  
  # libraries
  library(lubridate)
  
  # read in table
  tmp = read.table(ifile, sep = '\t', header = TRUE)
  
  # extract data
  df = data.frame(
    start_time = as.POSIXct(paste0(tmp$Begin.Date, ' ', tmp$Begin.Clock.Time), format = '%Y/%m/%d %H:%M:%OS', tz = 'UTC'),
    end_time = as.POSIXct(paste0(tmp$End.Date, ' ', tmp$End.Clock.Time), format = '%Y/%m/%d %H:%M:%OS', tz = 'UTC'),
    duration = tmp$Delta.Time..s.,
    fmin = tmp$Low.Freq..Hz.,
    fmax = tmp$High.Freq..Hz.,
    file = tmp$Begin.File,
    call_type = tmp$call_type,
    notes = tmp$notes
  )
  
  # add other columns
  df$year = year(df$start_time)
  df$id = paste0(df$year, '_', platform, '_', substr(x = basename(ifile), start = 4, stop = 5))
  
  # return
  return(df)
}

proc_selection_tables = function(idir, platform){
  # process all selection tables in a directory
  
  # file list
  flist = list.files(path = idir, pattern = ".txt$", full.names = TRUE)
  
  # process files
  DF = vector('list', length = length(flist))
  for(i in seq_along(flist)){
    # read selection table
    DF[[i]] = read_selection_table(flist[i], platform = 'noaa')
  }
  
  # collapse data
  df = do.call(rbind, DF)
  
  return(df)
}

calc_recording_durations = function(df){
  
  # vector of deployments
  deps = unique(df$id)
  
  # initialize output
  durs = data.frame(
    id = deps,
    date = as.Date(df$start_time[match(deps, df$id)]),
    hrs = 0
  )
  
  # calculate durations from start/stop timestamps
  for(ii in seq_along(deps)){
    idep = subset(df, id == deps[ii] & call_type %in% c('START', 'END'))
    
    # determine indices of start/end times
    sts = which(idep$call_type == 'START')
    ens = which(idep$call_type == 'END')
    
    # calculate recording time for each chunk
    chk = vector('numeric', length(sts))
    for(jj in 1:length(sts)){
      chk[jj] = as.numeric(idep$end_time[ens[jj]]) - as.numeric(idep$start_time[sts[jj]])
    }
    
    # add to output frame [hours]
    durs$hrs[ii] = sum(chk)/60/60 
  }
  
  return(durs)
}