## logistic_models_plots.R ##

# logistc models for sono project

# libraries
library(tidyverse)
library(ggpubr)

# data
df = readRDS("data/processed/proc_acou_photoid.rds")

# data set up
# add new column for estimating probabilty of foraging or socalizing
# forso_bi
df$forso = df$social_bhv_whale/df$foraging_bhv_whale
df[,"forso_bi"] <- 0
df$forso_bi[df$forso > 1] <- 1
df$forso_bi[df$forso == 1] <- 0.5

# plots
# foraging upcall
sightm = glm(foraging_bi ~ up_per_hr, data = df, family = 'binomial')
summary(sightm)

aplt = ggplot(df, aes(x=up_per_hr, y=foraging_bi)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE)
aplt

# foraging gunshot
sightm = glm(foraging_bi ~ gs_per_hr, data = df, family = 'binomial')
summary(sightm)

bplt = ggplot(df, aes(x=gs_per_hr, y=foraging_bi)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE)
bplt

# foraging midfreq
sightm = glm(foraging_bi ~ mf_per_hr, data = df, family = 'binomial')
summary(sightm)

cplt = ggplot(df, aes(x=mf_per_hr, y=foraging_bi)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE)
cplt

# social upcall
sightm = glm(social_bi ~ up_per_hr, data = df, family = 'binomial')
summary(sightm)

dplt = ggplot(df, aes(x=up_per_hr, y=social_bi)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE)
dplt

# social gunshot
sightm = glm(social_bi ~ gs_per_hr, data = df, family = 'binomial')
summary(sightm)

eplt = ggplot(df, aes(x=gs_per_hr, y=social_bi)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE)
eplt

# social midfreq
sightm = glm(social_bi ~ mf_per_hr, data = df, family = 'binomial')
summary(sightm)

fplt = ggplot(df, aes(x=mf_per_hr, y=social_bi)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE)
fplt

# foraging social proabilty is social rate/foraging rate thus in binary if 0 
# then more foraging was occuring in that deployment, if 1 then more social 
# was occuring in that deployment 

# foraging social probabilty up
sightm = glm(forso_bi ~ up_per_hr, data = df, family = 'binomial')
summary(sightm)

gplt = ggplot(df, aes(x=up_per_hr, y=forso_bi)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE)
gplt

# foraging social probabilty gunshot
sightm = glm(forso_bi ~ gs_per_hr, data = df, family = 'binomial')
summary(sightm)

hplt = ggplot(df, aes(x=gs_per_hr, y=forso_bi)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE)
hplt

# foraging social probabilty midfreq
sightm = glm(forso_bi ~ mf_per_hr, data = df, family = 'binomial')
summary(sightm)

iplt = ggplot(df, aes(x=mf_per_hr, y=forso_bi)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE)
iplt

figure <- ggarrange(aplt,
                    bplt,
                    cplt,
                    dplt,
                    eplt,
                    fplt,
                    gplt,
                    hplt,
                    iplt,
                    ncol = 3, nrow = 3)
figure

ggsave(figure, filename = paste0('figures/logistic_plots.png'), 
       units = 'in', dpi = 300)
