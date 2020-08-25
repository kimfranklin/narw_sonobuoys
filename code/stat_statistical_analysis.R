## statistical_analysis


# set up ------------------------------------------------------------------

library(ggplot2)
library(viridis)

ifile = readRDS("data/processed/sight_dfs.rds")
df= ifile

# process -----------------------------------------------------------------
# Making the pairs plots

dfs = subset(df, select=c('num_sighting',
                          'up_dur_hr',
                          'gs_dur_hr',
                          'mf_dur_hr',
                          'year',
                          'juvenile_female',
                          'juvenile_male',
                          'adult_male',
                          'adult_female'))
pairs(dfs)
class(dfs) 
par("mar")
par(mar=c(1,1,1,1))

# ratio, is it important?
lm = lm(df$sum_calls~df$ratio_female_male)
anova(lm) #NOT SIGNIFICANT
summary(lm) #Rsquared=0.01425, -0.05147

lm = lm(df$sum_calls_dur~df$year)
anova(lm) #NOT SIGNIFICANT
summary(lm) #Rsquared = 0.02854, -0.03622

#df$ratio_female_male is needed 
lm = lm(df$sum_calls_dur~df$ratio_female_male)
anova(lm) #NOT SIGNIFICANT
summary(lm) #Rsquared=0.008732, -0.05735

lm = lm(df$num_up~df$ratio_female_male)
anova(lm) #NOT SIGNIFICANT
summary(lm) #Rsquared = 0.1245, 0.06609

lm = lm(df$num_mf~df$ratio_female_male)
anova(lm) #NOT SIGNIFICANT
summary(lm) #Rsquared= 0.0006041, -0.06602

# the number of gs depends on the ratio of male to female
lm = lm(df$num_gs~df$ratio_female_male)
anova(lm) #NOT SIGNIFICANT
summary(lm) #Rsquared= 0.1101, 0.05081
plot(lm)

lm = lm(df$gs_dur_hr~df$ratio_female_male)
anova(lm) #NOT SIGNIFICANT
summary(lm) #Rsquared= 0.08556, 0.0246

# i don like this tho, many whales does not imply all each making a single sound
lm = lm(df$sum_calls~df$num_sighting)
anova(lm) #SIGNIFICANT now, wasnt before added the rest of dep 16 data
summary(lm) #Rsquare = 0.2334, 0.1823
cor(x=df$num_sighting, y=df$sum_calls)

lm = lm(df$sum_calls~df$adult_female)
anova(lm) #NOT SIGNIFICANT
summary(lm) #Rsquared = 0.05486, -0.008152

lm = lm(df$sum_calls~df$adult_male)
anova(lm) #SIGNIFICANT!!!!!! 
summary(lm) #Rsquared = 0.3009, 0.2543

lm = lm(df$sum_calls~df$sum_juvenile)
anova(lm) #NOT SIGNIFICANT
summary(lm) #Rsquared = 0.1973, 0.1438

lm = lm(df$num_gs~df$adult_male)
anova(lm) #NOT SIGNIFICANT
summary(lm) #Rsquared = 0.1469, 0.09004

#i think this is the key to saying that the number of males depends on the
# the number of mf calls there are 
lm = lm(df$num_mf~df$adult_male)
anova(lm) #SIGNIFICANT!!!!!
summary(lm) #Rsquared = 0.3419, 0.298
plot(lm)
lm = lm(df$mf_dur_hr~df$adult_male)
anova(lm) #SIGNIFICANT!!!!!

lm = lm(df$num_gs~df$adult_female)
anova(lm) #NOT SIGNIFICANT
summary(lm) #Rsquared = 0.001292, -0.06529

lm = lm(df$num_mf~df$adult_female)
anova(lm) #NOT SIGNIFICANT
summary(lm) #Rsquared = 0.09535, 0.03504

lm = lm(df$up_dur_hr~df$sum_juvenile)
anova(lm) #NOT SIGNIFICANT
summary(lm)

lm = lm(df$num_up~df$sum_juvenile)
anova(lm) #NOT SIGNIFICANT
summary(lm)

lm = lm(df$mf_dur_hr~df$gs_dur_hr)
plot(lm)
anova(lm) #SIGNIFICANT!!!!!!
summary(lm)
plt = ggplot(df)+
  geom_point(aes(x = mf_dur_hr, y = gs_dur_hr, col=year), size = 3)+
  scale_color_manual(values=c("black", "red"))+
  labs(col = 'Year', x = 'mf calls per hour', y = 'gs calls per hour')+
  theme(text = element_text(size=15))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))
plt
lm = lm(df$gs_dur_hr~df$mf_dur_hr)
anova(lm)

# anova code: anova(name_of_thing_want_anova_on)
# linear model
lm = lm(df$mf_dur_hr~df$adult_male+df$juvenile_male+df$num_sighting+df$gs_dur_hr)
anova(lm) #SIGNIFICANT!!!!!!

lm = lm(df$gs_dur_hr~df$sum_male)
anova(lm) 


lm1 = lm(df$num_gs~df$adult_male+df$juvenile_male)
anova(lm1) #NOT SIGNIFICANT
summary(lm1)

lm1a = lm(df$num_gs~df$adult_female+df$juvenile_female)
anova(lm1a) #NOT SIGNIFICANT
summary(lm1a)

lm1b = lm(df$gs_dur_hr~df$adult_female)
anova(lm1b) #NOT SIGNIFICANT
summary(lm1b)

plt = ggplot(df)+
  geom_point(aes(x = adult_male, y = gs_dur_hr, col=year), size = 3)+
  scale_color_manual(values=c("black", "red"))+
  labs(col = 'Year', x = 'Number of NARW Sighted', y = 'Calls per Hour')+
  theme(text = element_text(size=15))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))
plt

lm2 = lm(df$num_mf~df$adult_female)
anova(lm2) #NOT SIGNIFICANT
summary(lm2)

lm2a = lm(df$num_mf~df$adult_male+df$juvenile_male)
anova(lm2a) #BOTH ARE SIGNIFICANT!!!!!
#males are not making midfrequency calls
summary(lm2a) #rsquared = 0.4977, 0.4259 
plot(lm2a)

lm2b = lm(df$mf_dur_hr~df$adult_male)
anova(lm2b) #SIGNIFICANT!!!!!!
summary(lm2b)

plt = ggplot(df)+
  geom_point(aes(x = adult_male, y = mf_dur_hr, col=year), size = 3)+
  scale_color_manual(values=c("black", "red"))+
  labs(col = 'Year', x = 'Number of NARW Sighted', y = 'Calls per Hour')+
  theme(text = element_text(size=15))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(axis.text.y = element_text(colour = "black"))
plt

# all current variables in one model
lm_all = lm(df$num_sighting~
              df$num_up+
              df$num_mf+
              df$num_gs+
              df$duration+
              df$juvenile_female+
              df$juvenile_male+
              df$adult_female+
              df$adult_male+
              df$year)
anova(lm_all)

# stats from the scatterplots (not that it useful cuz lost of things going on)
lm3 = lm(df$up_dur_hr~df$num_sighting)
anova(lm3) #NOT SIGNIFICANT
summary(lm3)

lm4 = lm(df$mf_dur_hr~df$num_sighting)
anova(lm4) #SIGNIFICANT!!!!well duh most of the whales are male
summary(lm4)

lm5 = lm(df$gs_dur_hr~df$num_sighting)
anova(lm5)#NOT SIGNIFICANT
summary(lm5)

#not sure how to group the categroies further
#i.e. combine all calls per hour (call_rate); sex, age (this will have to be 
# categrical), call_type = all calls for that deployment
lm = lm(call_rate~sex+month+total_whales+call_type+year+age)
lm = lm(call_type~sex+month+total_whales)
lm = lm(call_rate~sex+month+sex*month)
