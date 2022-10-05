

# compare entangled deployments vs no entangled

# libraries
library(data.table)
library(tidyverse)

# input 
# read in data 
df = readRDS("data/processed/proc_acou_photoid_fliptest.rds")

# add column in df for entangled and not entangled
df$entangled = c('False','False','True','True','False','False','False','False','False','False',
                 'True','False','False','False','False','False','False','False','False','False',
                 'False','False','False','False','False','False','False','False','True','True',
                 'False','False','False','False','False','False','False')

dfs = df %>% 
  select('id','entangled','date','up','gs','mf')

# Upcall
# count
a = kruskal.test(up~entangled, data = df)
a = do.call(rbind.data.frame, a)
a=data.table::transpose(a)

# rate
b=kruskal.test(up_per_hr~entangled, data = df)
b = do.call(rbind.data.frame, b)
b=data.table::transpose(b)

# production rate
c=kruskal.test(up_per_hr_per_whale~entangled, data = df)
c = do.call(rbind.data.frame, c)
c=data.table::transpose(c)

# Gunshot
# count
d=kruskal.test(gs~entangled, data = df)
d = do.call(rbind.data.frame, d)
d=data.table::transpose(d)

# rate
e=kruskal.test(gs_per_hr~entangled, data = df)
e = do.call(rbind.data.frame, e)
e=data.table::transpose(e)

# production rate
f=kruskal.test(gs_per_hr_per_whale~entangled, data = df)
f = do.call(rbind.data.frame, f)
f=data.table::transpose(f)

# Mid-freq
# count
g=kruskal.test(mf~entangled, data = df)
g = do.call(rbind.data.frame, g)
g=data.table::transpose(g)

# rate
h=kruskal.test(mf_per_hr~entangled, data = df)
h = do.call(rbind.data.frame, h)
h=data.table::transpose(h)

# production rate
i=kruskal.test(mf_per_hr_per_whale~entangled, data = df)
i = do.call(rbind.data.frame, i)
i=data.table::transpose(i)

# Number of whales
j=kruskal.test(num_sighting~entangled, data = df)
j = do.call(rbind.data.frame, j)
j=data.table::transpose(j)

# ratio
jk=kruskal.test(ratio_male_female~entangled, data = df)
jk = do.call(rbind.data.frame, jk)
jk=data.table::transpose(jk)

# Demographics
# adult male
k=kruskal.test(adult_male~entangled, data = df)
k = do.call(rbind.data.frame, k)
k=data.table::transpose(k)

# adult female
l=kruskal.test(adult_female~entangled, data = df)
l = do.call(rbind.data.frame, l)
l=data.table::transpose(l)

# juvenile male
m=kruskal.test(juvenile_male~entangled, data = df)
m = do.call(rbind.data.frame, m)
m=data.table::transpose(m)

# juvenile female
n=kruskal.test(juvenile_female~entangled, data = df)
n = do.call(rbind.data.frame, n)
n=data.table::transpose(n)

# calf male
na=kruskal.test(calf_male~entangled, data = df)
na = do.call(rbind.data.frame, na)
na=data.table::transpose(na)

# calf female
nb=kruskal.test(calf_female~entangled, data = df)
nb = do.call(rbind.data.frame, nb)
nb=data.table::transpose(nb)

# unknown dempgraphics
mno=kruskal.test(unknown~entangled, data = df)
mno = do.call(rbind.data.frame, mno)
mno=data.table::transpose(mno)

# Behaviours
# foraging rate
o=kruskal.test(foraging_bhv_whale~entangled, data = df)
o = do.call(rbind.data.frame, o)
o=data.table::transpose(o)

# social rate
p=kruskal.test(social_bhv_whale~entangled, data = df)
p = do.call(rbind.data.frame, p)
p=data.table::transpose(p)

# other behaviour rate
q=kruskal.test(other_bhv_whale~entangled, data = df)
q = do.call(rbind.data.frame, q)
q=data.table::transpose(q)

# recording duration
r=kruskal.test(rec_duration~entangled, data = df)
r = do.call(rbind.data.frame, r)
r=data.table::transpose(r)

# deployment duration
s=kruskal.test(dep_duration~entangled, data = df)
s = do.call(rbind.data.frame, s)
s=data.table::transpose(s)

# variablecol = c('Upcall count','Upcall rate','Upcall production rate',
#                 'Gunshot count','Gunshot rate','Gunshot production rate',
#                 'Tonal count','Tonal rate','Tonal production rate',
#                 'Number of whales','Adult male','Adult female','Juvenile male','Juvenile female',
#                 'Foraging rate','Socalizing rate')
# kw_df <- data.frame(variablecol)

# combine all rows into a dataframe
kw_df <- rbind(a,b,c,d,e,f,g,h,i,j,k,jk,l,m,n,na,nb,mno,o,p,q,r,s)

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

# set dataframe to be a table 
setDT(kw_df)

# save data table 
write.csv(kw_df,"data/processed/entangled_vs_nonentangled_fliptest.csv")
