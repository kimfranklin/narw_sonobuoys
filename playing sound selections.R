# To make a snipet of sound from raven for a given selection

# below are the code from the shiny app for pitch tracks, and the second one is
# from the raven plot classifications image for each selection. Combine the two 
# codes to make a mp3 file for each selection/for desired selections.

# this is from the shiny app
## functions ##
# helpful functions for data processing

# libraries
library(oce)
library(ncdf4)
library(tools)
library(xml2)
library(tuneR)
library(signal)

process_pitchtracks = function(deployment_id, 
                               data_dir = '../glider_data_archive', 
                               cache_dir = 'cache'){
  
  message('Processing pitch track data for deployment: ', deployment_id)
  
  # define files
  fin = paste0(data_dir, '/', deployment_id, '/dmon/', deployment_id, '_pitchtracks.nc')
  fout = paste0(cache_dir, '/', deployment_id, '_pitchtracks.rda')
  
  # open file
  nc = nc_open(fin)
  
  # extract the full dataset
  call_time = ncvar_get(nc, 'call_time')
  call_amplitude = ncvar_get(nc, 'call_amplitude')
  call_freq = ncvar_get(nc, 'call_freq')
  call_start = ncvar_get(nc, 'call_start')
  call_n = ncvar_get(nc, 'call_n')
  detection_time = ncvar_get(nc, 'detection_time')
  call_group_auto = ncvar_get(nc, 'call_group_auto')
  mdist = ncvar_get(nc, 'mdist')
  
  # extract start time
  ptrack_origin = ncatt_get(nc, 'call_time')$start_date
  ptrack_origin = as.POSIXct(ptrack_origin, format = '%m/%d/%y %H:%M:%S', tz = 'UTC')
  
  # convert times
  call_time = as.POSIXct(call_time, origin = ptrack_origin, tz = 'UTC')
  detection_time = as.POSIXct(detection_time, origin = ptrack_origin, tz = 'UTC')
  
  # save data
  save(call_time, call_freq, call_amplitude, call_n, call_start, detection_time, call_group_auto, mdist, ptrack_origin, file = fout)
  
  # close file path
  nc_close(nc)
  
  message('Done :)')
}

dmon_fileinfo = function(deployment_id, data_dir, cache_dir){
  
  # define files
  idir = paste0(data_dir, '/', deployment_id, '/dmon/')
  ofile = paste0(cache_dir, '/', deployment_id, '_fileinfo.rda')
  
  # list wav files
  wav_flist = list.files(idir,pattern = '.wav$',full.names = TRUE)
  xml_flist = list.files(idir,pattern = '.xml$',full.names = TRUE)
  
  # convert to basenames
  flist = file_path_sans_ext(wav_flist)
  
  # loop through files
  start_times = vector()
  durations = vector()
  for(ii in seq_along(flist)){
    
    # paths to wav and xml files
    iwav = paste0(flist[ii], '.wav')
    ixml = paste0(flist[ii], '.xml')
    
    # check for xml file
    if(!file.exists(ixml)){
      stop(basename(ixml), ' does not exist!')
    }
    
    # read in xml
    x = read_xml(ixml)
    
    # extract all event logs
    evt = xml_find_all(x, "EVENT")
    
    # find run event
    ind = grep('RUN', evt)[1]
    
    # extract timestring
    if(!is.na(ind)){
      tstr = xml_attr(evt, "TIME")[ind]
    } else {
      tstr = xml_attr(evt, "TIME")[1]
    }
    
    # parse beginning timestamp
    t0 = as.POSIXct(tstr, '%Y,%m,%d,%H,%M,%S', tz = 'UTC')
    
    # save start time
    start_times[ii] = t0
    
    # read and save file duration
    x = readWave(wav_flist[ii], header = TRUE)
    durations[ii] = x$samples/x$sample.rate
    
  }
  
  # output data
  fileinfo = data.frame(
    flist = wav_flist,
    start_times = as.POSIXct(start_times, origin = '1970-01-01', tz = 'UTC'),
    durations
  )
  
  # save
  save(fileinfo, file = ofile)
}

wav_read = function(times, t0, dur, verbose = FALSE, demean = TRUE){
  
  # fail if duration is too long
  if(dur > 60*60){
    stop('Snippet is too long! Please shorten...')
  }
  
  # extract file info
  flist = as.character(times$flist)
  start_times = as.POSIXct(times$start_times, tz = 'UTC')
  durations = times$durations
  
  # snippet end time
  t1 = t0+dur
  
  # determine start file
  dt = as.numeric(t0) - as.numeric(start_times)
  si = which.min(dt[dt>0])
  s0 = dt[si]
  fin0 = flist[si]
  
  if(t1<start_times[si+1]){
    # read in data from single file
    if(verbose){message('Reading data from:\n', fin0)}
    
    # end of snippet (in seconds)
    s1 = s0+dur
    
    # read in data if it exists
    if(s1>durations[si]){
      if(verbose){message('No audio data exists! Returning NAs...')}
      snd = NA
      fs = NA
    } else {
      tmp = readWave(fin0, from = s0, to = s1, units = 'seconds')
      snd = tmp@left
      fs = tmp@samp.rate
    }
    
  } else {
    # read in data from two files
    fin1 = flist[si+1]
    if(verbose){message('Reading data from:\n', fin0, '\n', fin1)}
    
    # determine duration
    s1 = as.numeric(start_times[si+1]) - as.numeric(start_times[si])
    s2 = s1-s0
    
    # initialize snippet length
    idur = 0
    
    # read in data from fin0 if it exists
    if(s1>durations[si]){
      if(verbose){message('No audio data exists! Returning NAs...')}
      snd0 = NA
      fs0 = NA
      t0 = start_times[si+1]
      message('Changing start time to ', t0)
    } else {
      tmp = readWave(fin0, from = s0, to = s1, units = 'seconds')
      snd0 = tmp@left
      fs0 = tmp@samp.rate
      idur = idur+s1-s0
    }
    
    # read in data from fin1 if it exists
    if(s2>durations[si+1]){
      if(verbose){message('No audio data exists! Returning NAs...')}
      snd1 = NA
      fs1 = NA
    } else {
      tmp = readWave(fin1, from = 0, to = s2, units = 'seconds')
      snd1 = tmp@left
      fs1 = tmp@samp.rate
      idur = idur+s2-0
    }
    
    # extract and combine
    snd = c(snd0,snd1)
    snd = snd[!is.na(snd)]
    fs = c(fs0,fs1)
    fs = fs[!is.na(fs)][1]
    dur = idur
  }
  
  # demean to remove DC offset
  if(demean){
    snd = snd - mean(snd, na.rm = TRUE)
  }
  
  # assemble output
  data = list(
    x = snd,
    fs = fs,
    dur = dur,
    t0 = t0
  )
  
  return(data)
}

wav_spec = function(data, nfft=1024, nwindow=256, overlap=0.5, plot_spec = T, normalize = F, flims = c(0,1000), zlims = c(NA,NA)){
  
  # extract data
  snd = data$x
  fs = data$fs
  dur = data$dur
  t0 = data$t0
  
  # check data
  if(is.na(fs)|is.na(snd[1])){
    message('Missing data! Cannot plot spectrogram...')
    return()
  }
  
  # create spectrogram
  spec = specgram(x = snd,
                  n = nfft,
                  Fs = fs, 
                  window = hanning(nwindow),
                  overlap = ceiling(nwindow*overlap)
  )
  
  # discard phase info and rotate
  P = t(abs(spec$S))
  
  # normalize
  if(normalize){
    P = P/max(P)  
  }
  
  # convert to dB
  P = 10*log10(P)
  
  # extract time and freq
  f = spec$f
  t = spec$t
  
  if(plot_spec){
    
    # change plot colour defaults
    par(bg = "black")
    par(col.lab="white") 
    par(col.axis="white")
    par(col.main="white")
    
    # zlim scaling
    if(is.na(zlims[1])){
      zlims = range(P)
    }
    
    # plot spectrogram
    imagep(t, f, P, col = oce.colorsViridis, drawPalette = FALSE, ylim = flims, zlim = zlims,
           ylab = 'Frequency [Hz]', decimate = TRUE, axes = FALSE, useRaster = TRUE)
    
    # configure time axis labels
    labs = format(seq.POSIXt(t0,t0+dur, by = '5 sec'), '%H:%M:%S')
    
    # add axes and formatting
    box(col = 'white')
    axis(1, at = seq(0, dur, by = 5), labels = labs, col = 'white')
    axis(2, labels = T, col = 'white')
    mtext(paste0(format(t0, '%B-%d')), side = 1, adj = 0, line = 2, col = 'white')
  }
  
  # prep output
  spec = list(
    t0 = t0,
    t = t,
    f = f,
    p = P
  )
  
  return(spec)
}

# note input function
dataModal <- function() {
  modalDialog(
    textInput("note", label = 'Add Note:'),
    
    # radioButtons("score", 'Score: ', c('Detected', 
    #                                    'Possible', 
    #                                    'Other')),
    
    selectInput("score", label = 'Score: ', multiple = F, selectize = T,
                choices = c('Detected', 
                            'Possible', 
                            'Other')),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("ok", "OK")
    ),
    size = "s", 
    easyClose = TRUE, 
    fade = FALSE
  )
}

# config reactive data types
displayFormat <- function(df) {
  
  # format for display
  df$tmin = format(df$tmin, '%Y-%m-%d %H:%M:%OS2')
  df$tmax = format(df$tmax, '%Y-%m-%d %H:%M:%OS2')
  df$fmin = round(df$fmin, 2)
  df$fmax = round(df$fmax, 2)
  df$score = as.character(df$score)
  df$notes = as.character(df$notes)
  
  return(df)
}

ptrack = function(ip,
                  call_type = c(5,6,7,8,9,10),
                  max_mdist = 3.0,
                  min_mdist = 0,
                  detection_time=detection_time,
                  call_n=call_n,
                  call_start=call_start,
                  call_time=call_time,
                  call_amplitude=call_amplitude,
                  call_freq=call_freq,
                  call_group_auto=call_group_auto,
                  mdist=mdist
){
  
  # start index
  i = call_start[ip]+1
  
  # end index
  j = i+call_n[ip]-1
  
  # time
  t = detection_time[ip]
  
  # indices
  ind = i:j
  
  # assign color pallete to points
  pal = oce.colorsJet()
  c = colormap(call_amplitude[ind], 
               zlim = c(9.5000, 25.44), # limits from range of full amp dataset
               col = pal) 
  
  # add pitch tracks
  for(n in 1:(length(ind))){
    segments(call_time[ind[n]], call_freq[ind[n]],call_time[ind[n+1]], call_freq[ind[n+1]],
             col = c$zcol[n])
    points(call_time[ind[n]], call_freq[ind[n]], col = c$zcol[n], pch = '.')
  }
  
  # add stats
  md = mdist[ip]
  ct = call_group_auto[ip]
  
  if(md >= min_mdist & md <= max_mdist & ct %in% call_type){
    
    # position
    t=mean(call_time[i:j])
    f=min(call_freq[i:j])
    
    # label
    lab = paste0(ct,'\n',round(md,2))
    
    # plot
    text(x = t, y = f, labels = lab, pos = 1, col = 'white', cex = 0.7)  
  }
}

plot_ptracks = function(t0,
                        window=60,
                        call_type = c(5,6,7,8,9,10),
                        max_mdist = 3.0,
                        min_mdist = 0,
                        add=F,
                        flims = c(-20,1000),
                        detection_time=detection_time,
                        call_n=call_n,
                        call_start=call_start,
                        call_time=call_time,
                        call_amplitude=call_amplitude,
                        call_freq=call_freq,
                        call_group_auto=call_group_auto,
                        mdist=mdist,
                        return_blank=FALSE
){
  
  # format start and end times
  t0 = as.POSIXct(t0, origin = '1970-01-01')
  t1 = t0+window
  
  # determine which observations fall within the time window
  ind = which(detection_time >= t0 & detection_time <= t1)
  
  # convert call times
  call_time = as.numeric(call_time)-as.numeric(t0)
  
  # create plot
  if(add==F){
    
    # change plot colour defaults
    par(bg = "black")
    par(col.lab="white") 
    par(col.axis="white")
    par(col.main="white")
    par(mar = c(3.0,3.5,1.2,0.5),
        mai = c(0.60,0.70,0.24,0.10),
        oma=c(0,0,0,0)) # image margin settings
    
    # start plot
    plot(x = c(0, window), y = c(0,1000), type = 'n', xlab = '', 
         ylab = 'Frequency [Hz]', xaxt = 'n', ylim = flims, xaxs="i", yaxs="i")
    
    # configure time axis labels
    labs = format(seq.POSIXt(t0,t0+window, by = '5 sec'), '%H:%M:%S')
    
    # add axes and formatting
    box(col = 'white')
    axis(1, at = seq(0, window, by = 5), labels = labs, col = 'white')
    axis(2, labels = T, col = 'white')
    mtext(paste0(format(t0, '%B-%d')), side = 1, adj = 0, line = 2, col = 'white')
  }
  
  # return a blank plot with no pitch tracks
  if(return_blank){
    return()
  }
  
  # add tracks to plot
  if(length(ind)!=0){
    for(i in 1:length(ind)){
      
      ptrack(ind[i],
             call_type = call_type,
             max_mdist = max_mdist,
             min_mdist = min_mdist,
             detection_time=detection_time,
             call_n=call_n,
             call_start=call_start,
             call_time=call_time,
             call_amplitude=call_amplitude,
             call_freq=call_freq,
             call_group_auto=call_group_auto,
             mdist=mdist
      )
    } 
  }
}


# this is from the plot classifications 
## plot_classifications ##
# plot and save spectrograms of all call categories

# input -------------------------------------------------------------------

# acoustic data directory
data_dir = 'data/2018/noaa/data/'

# processed data directory (with selection tables)
proc_dir = 'data/2018/noaa/processed/'

# deployment basename
bname = 'DEP'

# ouput directory
out_dir = 'data/test_rcode_sono/2018'

# plot parameters
pad = 1
nfft=8192
nwindow=4096
overlap=0.95
normalize = F
flims = c(0,750)

# setup -------------------------------------------------------------------

library(tidyverse)
library(tuneR)
library(signal)
library(oce)

# process -----------------------------------------------------------------

# list deployments
flist = list.dirs(data_dir, full.names = T, recursive = F)

# remove test deployments
flist = flist[grep(pattern = 'TEST', flist, invert = T)]

for(ii in seq_along(flist)){
  
  # isolate deployment data path
  idep = flist[ii]
  
  # list all audio files
  wlist = list.files(pattern = "*.wav$", path = idep, full.names = T)
  
  # build table with wav file times
  wtb = data.frame(name = wlist, dur = NA)
  for(jj in seq_along(wlist)){
    tmp = readWave(wlist[jj], header = T)
    wtb$dur[jj] = tmp$samples/tmp$sample.rate
  }
  
  # add cumulative sum column
  wtb$total_dur = cumsum(wtb$dur)
  
  # determine deployment id
  i0 = gregexpr(pattern = bname, text = idep)[[1]][1]
  i1 = i0+5
  dep_id = substr(x = idep, start = i0, stop = i1)
  
  # find selection table file
  sfile = paste0(proc_dir, dep_id, 'selections.txt')
  
  # read in selection table file
  sel = read.delim(sfile, sep = '\t') %>%
    transmute(
      tmin = Begin.Time..s.,
      tmax = End.Time..s.,
      fmin = Low.Freq..Hz.,
      fmax = High.Freq..Hz.,
      time = as.POSIXct(paste0(Begin.Date, ' ', Begin.Clock.Time), format = '%Y/%m/%d %H:%M:%S'),
      call_type,
      notes
    )
  
  # define call_types to exclude from plotting
  exclude_types = c('START', 'END')
  
  # exclude call types
  sel = sel[!sel$call_type %in% exclude_types,]
  
  # exclude calls types with question mark
  sel = sel[grep(pattern = '?', x = sel$call_type, fixed = T, invert = T),]
  
  # plot spectrogram of each call
  for(kk in 1:nrow(sel)){
    
    # determine audio data file
    diffs = wtb$total_dur-sel$tmin[kk]
    diffs[diffs<0] = NA
    ind = which.min(diffs)
    
    # determine start and end of snippet relative to file start
    if(ind > 1){
      t0 = sel$tmin[kk] - wtb$total_dur[ind-1] - pad
      t1 = sel$tmax[kk] - wtb$total_dur[ind-1] + pad
    } else {
      t0 = sel$tmin[kk] - pad
      t1 = sel$tmax[kk] + pad
    }
    
    # read in audio data
    aud = readWave(filename = wlist[ind], from = t0, to = t1, units = "seconds")
    
    # extract data
    snd = aud@left
    fs = aud@samp.rate
    dur = sel$tmax[kk] - sel$tmin[kk]
    
    # create spectrogram
    spec = specgram(x = snd,
                    n = nfft,
                    Fs = fs, 
                    window = hanning(nwindow),
                    overlap = ceiling(nwindow*overlap)
    )
    
    # discard phase info and rotate
    P = t(abs(spec$S))
    
    # normalize
    if(normalize){
      P = P/max(P)  
    }
    
    # convert to dB
    P = 10*log10(P)
    
    # extract time and freq
    f = spec$f
    t = spec$t
    
    # create output file
    ofile = paste0(out_dir, '/', sel$call_type[kk], '/', paste0(dep_id, '_', format(sel$time[kk], '%Y%m%d_%H%M%S')), '.png')
    
    # create directory
    if(!dir.exists(dirname(ofile))){dir.create(dirname(ofile),recursive = T)}
    
    # save plot
    png(filename = ofile, width = 6, height = 6, units = 'in', res = 150)
    
    # plot spectrogram
    suppressWarnings(imagep(t, f, P, col = oce.colorsViridis, drawPalette = TRUE, ylim = flims,
                            ylab = 'Frequency [Hz]', xlab = 'Time [s]', decimate = TRUE,
                            mar =c(7,4,4,2)+0.1,  useRaster = T))
    
    # plot box
    rect(xleft = pad, xright = pad+dur, ybottom = sel$fmin[kk], ytop = sel$fmax[kk])
    
    # title
    title(paste0('Call type: ', sel$call_type[kk]))
    
    # add text
    mtext(text = paste0(dep_id, ': ', sel$time[kk]), side = 3, line = 0, adj = 0)
    
    # add bottom text
    mtext(paste0('Notes: ', sel$notes[kk]), side = 1, line = 3, adj = 0)
    
    # save plot
    dev.off()
  }
  message(dep_id, ' complete!')
}

