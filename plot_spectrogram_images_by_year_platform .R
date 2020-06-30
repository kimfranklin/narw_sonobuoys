## plot_spectrogram_images_by_year_platform ##
# plot and save spectrograms of all call categories

# input -------------------------------------------------------------------

# acoustic data directory
data_dir = 'data/2018/noaa/data/' 
# 'data/2017/noaa/data/' for 2017 file

# processed data directory (with selection tables)
proc_dir = 'data/2018/noaa/processed/' 
# 'data/2017/noaa/processed/' for 2017 file

# deployment basename
bname = 'DEP'

# ouput directory
out_dir = 'figures/selection_table_images/2018_noaa_raven_images/'
# 'figures/selection_table_images/2017_raven_images/' for 2017 file

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
    # mtext(paste0('Score: ', sel$notes[kk]), side = 1, line = 4, adj = 0)
    
    # save plot
    dev.off()
  }
  message(dep_id, ' complete!')
}
