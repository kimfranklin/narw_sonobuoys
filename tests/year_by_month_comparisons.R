
# year_by_month_comparisons.R #

install.packages("data.table")
library(data.table)
library(tidyverse)

df= readRDS("data/processed/proc_acou_photoid_fliptest.rds")

df_june = df %>%
  filter(month == "6")

df_july = df %>%
  filter(month == "7")

df_august = df %>%
  filter(month == "8")

# year comparisons
# Upcall
# count
a = kruskal.test(up~year, data = df_june)
a = do.call(rbind.data.frame, a)
a=data.table::transpose(a)

# rate
b=kruskal.test(up_per_hr~year, data = df_june)
b = do.call(rbind.data.frame, b)
b=data.table::transpose(b)

# production rate
c=kruskal.test(up_per_hr_per_whale~year, data = df_june)
c = do.call(rbind.data.frame, c)
c=data.table::transpose(c)

# Gunshot
# count
d=kruskal.test(gs~year, data = df_june)
d = do.call(rbind.data.frame, d)
d=data.table::transpose(d)

# rate
e=kruskal.test(gs_per_hr~year, data = df_june)
e = do.call(rbind.data.frame, e)
e=data.table::transpose(e)

# production rate
f=kruskal.test(gs_per_hr_per_whale~year, data = df_june)
f = do.call(rbind.data.frame, f)
f=data.table::transpose(f)

# Mid-freq
# count
g=kruskal.test(mf~year, data = df_june)
g = do.call(rbind.data.frame, g)
g=data.table::transpose(g)

# rate
h=kruskal.test(mf_per_hr~year, data = df_june)
h = do.call(rbind.data.frame, h)
h=data.table::transpose(h)

# production rate
i=kruskal.test(mf_per_hr_per_whale~year, data = df_june)
i = do.call(rbind.data.frame, i)
i=data.table::transpose(i)

# Number of whales
j=kruskal.test(num_sighting~year, data = df_june)
j = do.call(rbind.data.frame, j)
j=data.table::transpose(j)

# ratio
jk=kruskal.test(ratio_male_female~year, data = df_june)
jk = do.call(rbind.data.frame, jk)
jk=data.table::transpose(jk)

# Demographics
# adult male
k=kruskal.test(adult_male~year, data = df_june)
k = do.call(rbind.data.frame, k)
k=data.table::transpose(k)

# adult female
l=kruskal.test(adult_female~year, data = df_june)
l = do.call(rbind.data.frame, l)
l=data.table::transpose(l)

# juvenile male
m=kruskal.test(juvenile_male~year, data = df_june)
m = do.call(rbind.data.frame, m)
m=data.table::transpose(m)

# juvenile female
n=kruskal.test(juvenile_female~year, data = df_june)
n = do.call(rbind.data.frame, n)
n=data.table::transpose(n)

# calf male
na=kruskal.test(calf_male~year, data = df_june)
na = do.call(rbind.data.frame, na)
na=data.table::transpose(na)

# calf female
nb=kruskal.test(calf_female~year, data = df_june)
nb = do.call(rbind.data.frame, nb)
nb=data.table::transpose(nb)

# unknown dempgraphics
mno=kruskal.test(unknown~year, data = df_june)
mno = do.call(rbind.data.frame, mno)
mno=data.table::transpose(mno)

# Behaviours
# foraging rate
o=kruskal.test(foraging_bhv_whale~year, data = df_june)
o = do.call(rbind.data.frame, o)
o=data.table::transpose(o)

# social rate
p=kruskal.test(social_bhv_whale~year, data = df_june)
p = do.call(rbind.data.frame, p)
p=data.table::transpose(p)

# other behaviour rate
q=kruskal.test(other_bhv_whale~year, data = df_june)
q = do.call(rbind.data.frame, q)
q=data.table::transpose(q)

# recording duration
r=kruskal.test(rec_duration~year, data = df_june)
r = do.call(rbind.data.frame, r)
r=data.table::transpose(r)

# deployment duration
s=kruskal.test(dep_duration~year, data = df_june)
s = do.call(rbind.data.frame, s)
s=data.table::transpose(s)

# variablecol = c('Upcall count','Upcall rate','Upcall production rate',
#                 'Gunshot count','Gunshot rate','Gunshot production rate',
#                 'Tonal count','Tonal rate','Tonal production rate',
#                 'Number of whales','Adult male','Adult female','Juvenile male','Juvenile female',
#                 'Foraging rate','Socalizing rate')
# kw_df <- data.frame(variablecol)

# combine all rows into a dataframe
kw_df1 <- rbind(a,b,c,d,e,f,g,h,i,j,k,jk,l,m,n,na,nb,mno,o,p,q,r,s)

# round to 3 decimal places
kw_df1$V1 = round(as.numeric(kw_df1$V1), 3)
kw_df1$V3 = round(as.numeric(kw_df1$V3), 3)

# rename column names
kw_df1 = kw_df1 %>% 
  dplyr::rename(
    test_stat = V1,
    degrees_freedom = V2,
    p_val = V3,
    test_type = V4,
    variables_comp = V5
  )

# year comparisons
# Upcall
# count
a = kruskal.test(up~year, data = df_july)
a = do.call(rbind.data.frame, a)
a=data.table::transpose(a)

# rate
b=kruskal.test(up_per_hr~year, data = df_july)
b = do.call(rbind.data.frame, b)
b=data.table::transpose(b)

# production rate
c=kruskal.test(up_per_hr_per_whale~year, data = df_july)
c = do.call(rbind.data.frame, c)
c=data.table::transpose(c)

# Gunshot
# count
d=kruskal.test(gs~year, data = df_july)
d = do.call(rbind.data.frame, d)
d=data.table::transpose(d)

# rate
e=kruskal.test(gs_per_hr~year, data = df_july)
e = do.call(rbind.data.frame, e)
e=data.table::transpose(e)

# production rate
f=kruskal.test(gs_per_hr_per_whale~year, data = df_july)
f = do.call(rbind.data.frame, f)
f=data.table::transpose(f)

# Mid-freq
# count
g=kruskal.test(mf~year, data = df_july)
g = do.call(rbind.data.frame, g)
g=data.table::transpose(g)

# rate
h=kruskal.test(mf_per_hr~year, data = df_july)
h = do.call(rbind.data.frame, h)
h=data.table::transpose(h)

# production rate
i=kruskal.test(mf_per_hr_per_whale~year, data = df_july)
i = do.call(rbind.data.frame, i)
i=data.table::transpose(i)

# Number of whales
j=kruskal.test(num_sighting~year, data = df_july)
j = do.call(rbind.data.frame, j)
j=data.table::transpose(j)

# ratio
jk=kruskal.test(ratio_male_female~year, data = df_july)
jk = do.call(rbind.data.frame, jk)
jk=data.table::transpose(jk)

# Demographics
# adult male
k=kruskal.test(adult_male~year, data = df_july)
k = do.call(rbind.data.frame, k)
k=data.table::transpose(k)

# adult female
l=kruskal.test(adult_female~year, data = df_july)
l = do.call(rbind.data.frame, l)
l=data.table::transpose(l)

# juvenile male
m=kruskal.test(juvenile_male~year, data = df_july)
m = do.call(rbind.data.frame, m)
m=data.table::transpose(m)

# juvenile female
n=kruskal.test(juvenile_female~year, data = df_july)
n = do.call(rbind.data.frame, n)
n=data.table::transpose(n)

# calf male
na=kruskal.test(calf_male~year, data = df_july)
na = do.call(rbind.data.frame, na)
na=data.table::transpose(na)

# calf female
nb=kruskal.test(calf_female~year, data = df_july)
nb = do.call(rbind.data.frame, nb)
nb=data.table::transpose(nb)

# unknown dempgraphics
mno=kruskal.test(unknown~year, data = df_july)
mno = do.call(rbind.data.frame, mno)
mno=data.table::transpose(mno)

# Behaviours
# foraging rate
o=kruskal.test(foraging_bhv_whale~year, data = df_july)
o = do.call(rbind.data.frame, o)
o=data.table::transpose(o)

# social rate
p=kruskal.test(social_bhv_whale~year, data = df_july)
p = do.call(rbind.data.frame, p)
p=data.table::transpose(p)

# other behaviour rate
q=kruskal.test(other_bhv_whale~year, data = df_july)
q = do.call(rbind.data.frame, q)
q=data.table::transpose(q)

# recording duration
r=kruskal.test(rec_duration~year, data = df_july)
r = do.call(rbind.data.frame, r)
r=data.table::transpose(r)

# deployment duration
s=kruskal.test(dep_duration~year, data = df_july)
s = do.call(rbind.data.frame, s)
s=data.table::transpose(s)

# variablecol = c('Upcall count','Upcall rate','Upcall production rate',
#                 'Gunshot count','Gunshot rate','Gunshot production rate',
#                 'Tonal count','Tonal rate','Tonal production rate',
#                 'Number of whales','Adult male','Adult female','Juvenile male','Juvenile female',
#                 'Foraging rate','Socalizing rate')
# kw_df <- data.frame(variablecol)

# combine all rows into a dataframe
kw_df2 <- rbind(a,b,c,d,e,f,g,h,i,j,k,jk,l,m,n,na,nb,mno,o,p,q,r,s)

# round to 3 decimal places
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

# year comparisons
# Upcall
# count
a = kruskal.test(up~year, data = df_august)
a = do.call(rbind.data.frame, a)
a=data.table::transpose(a)

# rate
b=kruskal.test(up_per_hr~year, data = df_august)
b = do.call(rbind.data.frame, b)
b=data.table::transpose(b)

# production rate
c=kruskal.test(up_per_hr_per_whale~year, data = df_august)
c = do.call(rbind.data.frame, c)
c=data.table::transpose(c)

# Gunshot
# count
d=kruskal.test(gs~year, data = df_august)
d = do.call(rbind.data.frame, d)
d=data.table::transpose(d)

# rate
e=kruskal.test(gs_per_hr~year, data = df_august)
e = do.call(rbind.data.frame, e)
e=data.table::transpose(e)

# production rate
f=kruskal.test(gs_per_hr_per_whale~year, data = df_august)
f = do.call(rbind.data.frame, f)
f=data.table::transpose(f)

# Mid-freq
# count
g=kruskal.test(mf~year, data = df_august)
g = do.call(rbind.data.frame, g)
g=data.table::transpose(g)

# rate
h=kruskal.test(mf_per_hr~year, data = df_august)
h = do.call(rbind.data.frame, h)
h=data.table::transpose(h)

# production rate
i=kruskal.test(mf_per_hr_per_whale~year, data = df_august)
i = do.call(rbind.data.frame, i)
i=data.table::transpose(i)

# Number of whales
j=kruskal.test(num_sighting~year, data = df_august)
j = do.call(rbind.data.frame, j)
j=data.table::transpose(j)

# ratio
jk=kruskal.test(ratio_male_female~year, data = df_august)
jk = do.call(rbind.data.frame, jk)
jk=data.table::transpose(jk)

# Demographics
# adult male
k=kruskal.test(adult_male~year, data = df_august)
k = do.call(rbind.data.frame, k)
k=data.table::transpose(k)

# adult female
l=kruskal.test(adult_female~year, data = df_august)
l = do.call(rbind.data.frame, l)
l=data.table::transpose(l)

# juvenile male
m=kruskal.test(juvenile_male~year, data = df_august)
m = do.call(rbind.data.frame, m)
m=data.table::transpose(m)

# juvenile female
n=kruskal.test(juvenile_female~year, data = df_august)
n = do.call(rbind.data.frame, n)
n=data.table::transpose(n)

# calf male
na=kruskal.test(calf_male~year, data = df_august)
na = do.call(rbind.data.frame, na)
na=data.table::transpose(na)

# calf female
nb=kruskal.test(calf_female~year, data = df_august)
nb = do.call(rbind.data.frame, nb)
nb=data.table::transpose(nb)

# unknown dempgraphics
mno=kruskal.test(unknown~year, data = df_august)
mno = do.call(rbind.data.frame, mno)
mno=data.table::transpose(mno)

# Behaviours
# foraging rate
o=kruskal.test(foraging_bhv_whale~year, data = df_august)
o = do.call(rbind.data.frame, o)
o=data.table::transpose(o)

# social rate
p=kruskal.test(social_bhv_whale~year, data = df_august)
p = do.call(rbind.data.frame, p)
p=data.table::transpose(p)

# other behaviour rate
q=kruskal.test(other_bhv_whale~year, data = df_august)
q = do.call(rbind.data.frame, q)
q=data.table::transpose(q)

# recording duration
r=kruskal.test(rec_duration~year, data = df_august)
r = do.call(rbind.data.frame, r)
r=data.table::transpose(r)

# deployment duration
s=kruskal.test(dep_duration~year, data = df_august)
s = do.call(rbind.data.frame, s)
s=data.table::transpose(s)

# variablecol = c('Upcall count','Upcall rate','Upcall production rate',
#                 'Gunshot count','Gunshot rate','Gunshot production rate',
#                 'Tonal count','Tonal rate','Tonal production rate',
#                 'Number of whales','Adult male','Adult female','Juvenile male','Juvenile female',
#                 'Foraging rate','Socalizing rate')
# kw_df <- data.frame(variablecol)

# combine all rows into a dataframe
kw_df3 <- rbind(a,b,c,d,e,f,g,h,i,j,k,jk,l,m,n,na,nb,mno,o,p,q,r,s)

# round to 3 decimal places
kw_df3$V1 = round(as.numeric(kw_df3$V1), 3)
kw_df3$V3 = round(as.numeric(kw_df3$V3), 3)

# rename column names
kw_df3 = kw_df3 %>% 
  dplyr::rename(
    test_stat = V1,
    degrees_freedom = V2,
    p_val = V3,
    test_type = V4,
    variables_comp = V5
  )

# combine year dataframe and month dataframe by adding on the columns
kw_df = cbind(kw_df1,kw_df2,kw_df3)

# set dataframe to be a table 
setDT(kw_df)

# save data table 
write.csv(kw_df,"data/processed/year_by_month_comparisons_fliptest.csv")
