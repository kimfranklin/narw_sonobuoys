# year_month_comparisons.R #

# making year and month comparison table for manuscript

# libraries ---------------------------------------------------------------

library(data.table)
library(tidyverse)

# input -------------------------------------------------------------------

# read in data 
df= readRDS("data/processed/proc_acou_photoid.rds")

# process -----------------------------------------------------------------

# year comparisons
# Upcall
# count
a = kruskal.test(up~year, data = df)
a = do.call(rbind.data.frame, a)
a=data.table::transpose(a)

# rate
b=kruskal.test(up_per_hr~year, data = df)
b = do.call(rbind.data.frame, b)
b=data.table::transpose(b)

# production rate
c=kruskal.test(up_per_hr_per_whale~year, data = df)
c = do.call(rbind.data.frame, c)
c=data.table::transpose(c)

# Gunshot
# count
d=kruskal.test(gs~year, data = df)
d = do.call(rbind.data.frame, d)
d=data.table::transpose(d)

# rate
e=kruskal.test(gs_per_hr~year, data = df)
e = do.call(rbind.data.frame, e)
e=data.table::transpose(e)

# production rate
f=kruskal.test(gs_per_hr_per_whale~year, data = df)
f = do.call(rbind.data.frame, f)
f=data.table::transpose(f)

# Mid-freq
# count
g=kruskal.test(mf~year, data = df)
g = do.call(rbind.data.frame, g)
g=data.table::transpose(g)

# rate
h=kruskal.test(mf_per_hr~year, data = df)
h = do.call(rbind.data.frame, h)
h=data.table::transpose(h)

# production rate
i=kruskal.test(mf_per_hr_per_whale~year, data = df)
i = do.call(rbind.data.frame, i)
i=data.table::transpose(i)

# Number of whales
j=kruskal.test(num_sighting~year, data = df)
j = do.call(rbind.data.frame, j)
j=data.table::transpose(j)

# ratio
jk=kruskal.test(ratio_male_female~year, data = df)
jk = do.call(rbind.data.frame, jk)
jk=data.table::transpose(jk)

# Demographics
# adult male
k=kruskal.test(adult_male~year, data = df)
k = do.call(rbind.data.frame, k)
k=data.table::transpose(k)

# adult female
l=kruskal.test(adult_female~year, data = df)
l = do.call(rbind.data.frame, l)
l=data.table::transpose(l)

# juvenile male
m=kruskal.test(juvenile_male~year, data = df)
m = do.call(rbind.data.frame, m)
m=data.table::transpose(m)

# juvenile female
n=kruskal.test(juvenile_female~year, data = df)
n = do.call(rbind.data.frame, n)
n=data.table::transpose(n)

# calf male
na=kruskal.test(calf_male~year, data = df)
na = do.call(rbind.data.frame, na)
na=data.table::transpose(na)

# calf female
nb=kruskal.test(calf_female~year, data = df)
nb = do.call(rbind.data.frame, nb)
nb=data.table::transpose(nb)

# unknown dempgraphics
mno=kruskal.test(unknown~year, data = df)
mno = do.call(rbind.data.frame, mno)
mno=data.table::transpose(mno)

# Behaviours
# foraging rate
o=kruskal.test(foraging_bhv_whale~year, data = df)
o = do.call(rbind.data.frame, o)
o=data.table::transpose(o)

# social rate
p=kruskal.test(social_bhv_whale~year, data = df)
p = do.call(rbind.data.frame, p)
p=data.table::transpose(p)

# other behaviour rate
q=kruskal.test(other_bhv_whale~year, data = df)
q = do.call(rbind.data.frame, q)
q=data.table::transpose(q)

# recording duration
r=kruskal.test(rec_duration~year, data = df)
r = do.call(rbind.data.frame, r)
r=data.table::transpose(r)

# deployment duration
s=kruskal.test(dep_duration~year, data = df)
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

# month comparisons
# Upcall
# count
a = kruskal.test(up~month, data = df)
a = do.call(rbind.data.frame, a)
a=data.table::transpose(a)

# rate
b=kruskal.test(up_per_hr~month, data = df)
b = do.call(rbind.data.frame, b)
b=data.table::transpose(b)

# production rate
c=kruskal.test(up_per_hr_per_whale~month, data = df)
c = do.call(rbind.data.frame, c)
c=data.table::transpose(c)

# Gunshot
# count
d=kruskal.test(gs~month, data = df)
d = do.call(rbind.data.frame, d)
d=data.table::transpose(d)

# rate
e=kruskal.test(gs_per_hr~month, data = df)
e = do.call(rbind.data.frame, e)
e=data.table::transpose(e)

# production rate
f=kruskal.test(gs_per_hr_per_whale~month, data = df)
f = do.call(rbind.data.frame, f)
f=data.table::transpose(f)

# Mid-freq
# count
g=kruskal.test(mf~month, data = df)
g = do.call(rbind.data.frame, g)
g=data.table::transpose(g)

# rate
h=kruskal.test(mf_per_hr~month, data = df)
h = do.call(rbind.data.frame, h)
h=data.table::transpose(h)

# production rate
i=kruskal.test(mf_per_hr_per_whale~month, data = df)
i = do.call(rbind.data.frame, i)
i=data.table::transpose(i)

# Number of whales
j=kruskal.test(num_sighting~month, data = df)
j = do.call(rbind.data.frame, j)
j=data.table::transpose(j)

# ratio
jk=kruskal.test(ratio_male_female~month, data = df)
jk = do.call(rbind.data.frame, jk)
jk=data.table::transpose(jk)

# Demographics
# adult male
k=kruskal.test(adult_male~month, data = df)
k = do.call(rbind.data.frame, k)
k=data.table::transpose(k)

# adult female
l=kruskal.test(adult_female~month, data = df)
l = do.call(rbind.data.frame, l)
l=data.table::transpose(l)

# juvenile male
m=kruskal.test(juvenile_male~month, data = df)
m = do.call(rbind.data.frame, m)
m=data.table::transpose(m)

# juvenile female
n=kruskal.test(juvenile_female~month, data = df)
n = do.call(rbind.data.frame, n)
n=data.table::transpose(n)

# calf male
na=kruskal.test(calf_male~month, data = df)
na = do.call(rbind.data.frame, na)
na=data.table::transpose(na)

# calf female
nb=kruskal.test(calf_female~month, data = df)
nb = do.call(rbind.data.frame, nb)
nb=data.table::transpose(nb)

# unknown dempgraphics
mno=kruskal.test(unknown~month, data = df)
mno = do.call(rbind.data.frame, mno)
mno=data.table::transpose(mno)

# Behaviours
# foraging rate
o=kruskal.test(foraging_bhv_whale~month, data = df)
o = do.call(rbind.data.frame, o)
o=data.table::transpose(o)

# social rate
p=kruskal.test(social_bhv_whale~month, data = df)
p = do.call(rbind.data.frame, p)
p=data.table::transpose(p)

# other behaviour rate
q=kruskal.test(other_bhv_whale~month, data = df)
q = do.call(rbind.data.frame, q)
q=data.table::transpose(q)

# recording duration
r=kruskal.test(rec_duration~month, data = df)
r = do.call(rbind.data.frame, r)
r=data.table::transpose(r)

# deployment duration
s=kruskal.test(dep_duration~month, data = df)
s = do.call(rbind.data.frame, s)
s=data.table::transpose(s)

# combine all rows into a dataframe
kw_df2 <- rbind(a,b,c,d,e,f,g,h,i,j,k,jk,l,m,n,na,nb,mno,o,p,q,r,s)

# round
kw_df2$V1 = round(as.numeric(kw_df2$V1), 3)
kw_df2$V3 = round(as.numeric(kw_df2$V3), 3)

# rename column names
kw_df2 = kw_df2 %>% 
  dplyr::rename(
    test_stat = V1,
    degrees_freedom = V2,
    p_val = V3,
    test_type = V4,
    variables_comp = V5
  )

# combine year dataframe and month dataframe by adding on the columns
kw_df = cbind(kw_df,kw_df2)

# set dataframe to be a table 
setDT(kw_df)

# save data table 
write.csv(kw_df,"data/processed/year_month_comparisons.csv")
