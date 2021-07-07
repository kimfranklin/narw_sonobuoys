

# compare entangled deployments vs no entangled

# libraries
library(data.table)
library(tidyverse)

# input 
# read in data 
df = readRDS("data/processed/proc_acou_photoid.rds")

# add column in df for entangled and not entangled
df$entangled = c('False','False','True','True','False','False','False','False','False','False',
                 'True','False','False','False','False','False','False','False','False','False',
                 'False','False','False','False','False','False','False','False','True','True',
                 'False','False','False','False','False','False','False')

dfs = df %>% 
  select('id','entangled','date','up','gs','mf')

# upcall count
a = kruskal.test(up~entangled, data = df)
a = do.call(rbind.data.frame, a)
a=data.table::transpose(a)

# gunshot count
b = kruskal.test(gs~entangled, data = df)
b = do.call(rbind.data.frame, b)
b=data.table::transpose(b)

# tonal count
c = kruskal.test(mf~entangled, data = df)
c = do.call(rbind.data.frame, c)
c=data.table::transpose(c)

# upcall rate
d = kruskal.test(up_per_hr~entangled, data = df)
d = do.call(rbind.data.frame, d)
d=data.table::transpose(d)

# gunshot rate
e = kruskal.test(gs_per_hr~entangled, data = df)
e = do.call(rbind.data.frame, e)
e=data.table::transpose(e)

# tonal rate
f = kruskal.test(mf_per_hr~entangled, data = df)
f = do.call(rbind.data.frame, f)
f=data.table::transpose(f)

# foraging behaviour rate
g = kruskal.test(foraging_bhv_whale~entangled, data = df)
g = do.call(rbind.data.frame, g)
g=data.table::transpose(g)

# socalizing behaviour rate
h = kruskal.test(social_bhv_whale~entangled, data = df)
h = do.call(rbind.data.frame, h)
h=data.table::transpose(h)

# ratio
i = kruskal.test(ratio_male_female~entangled, data = df)
i = do.call(rbind.data.frame, i)
i=data.table::transpose(i)

# combine all rows into a dataframe
kw_df <- rbind(a,b,c,d,e,f,g,h,i)

# round to 3 decimal places
kw_df$V1 = round(as.numeric(kw_df$V1), 3)
kw_df$V3 = round(as.numeric(kw_df$V3), 3)

# rename column names
kw_df = kw_df %>% 
  dplyr::rename(
    test_stat = V1,
    degrees_freedom = V2,
    p_val = V3,
    test_type = V4,
    variables_comp = V5
  )
