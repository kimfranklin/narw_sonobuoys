## logistic_models_plots.R ##

# logistc models for sono project

# libraries
library(tidyverse)
library(ggpubr)

# data
df = readRDS("data/processed/proc_acou_photoid.rds")

# dominant behaviour ------------------------------------------------------

# data set up
# add new column for estimating probabilty of foraging or socalizing
# forso_bi
df$forso = df$social_bhv_whale/df$foraging_bhv_whale
df$forso_bi[df$forso > 1 | is.infinite(df$forso)] = 1
df$forso_bi[df$forso < 1] = 0
df = drop_na(df, forso_bi)

# plots
# foraging upcall
sightm = glm(forso_bi ~ up_per_hr, data = df, family = 'binomial')
summary(sightm)

aplt = ggplot(df, aes(x=up_per_hr, y=forso_bi)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE)
aplt

# foraging gunshot
sightm = glm(forso_bi ~ gs_per_hr, data = df, family = 'binomial')
summary(sightm)

bplt = ggplot(df, aes(x=gs_per_hr, y=forso_bi)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE)
bplt

# foraging midfreq
sightm = glm(forso_bi ~ mf_per_hr, data = df, family = 'binomial')
summary(sightm)

cplt = ggplot(df, aes(x=mf_per_hr, y=forso_bi)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE)
cplt

# combined plot
dfl = df %>%
  rename(`mid-frequency` = mf_per_hr, `gunshot` = gs_per_hr, `upcall` = up_per_hr) %>%
  pivot_longer(c(`mid-frequency`,gunshot,upcall), names_to = 'type', values_to = 'rate') %>%
  select(type,rate,forso_bi)

ggplot(dfl, aes(x=rate, y=forso_bi, group = type, color = type, fill = type)) +
  geom_point(shape = 1) +
  geom_smooth(method = "glm",formula = 'y ~ x',
              method.args = list(family = "binomial"), 
              se = TRUE) +
  labs(x = 'Call rate (calls/h)', y = 'Probability', color = 'Call type', fill = 'Call type') +
  facet_wrap(~type, ncol = 1) +
  theme_bw()

# HDJ STOPPED HERE #

# social and foraging behaviours ------------------------------------------

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
