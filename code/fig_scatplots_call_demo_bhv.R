## fig_scatplots_call_demo_bhv.R ##

# Scatter plots for call production rate, demogrpahics, and behaviour rate by 
# yday categorized by year and grouping. These plots are to compare the time
# and space restrictions set in wrg_photoid.R.

# libraries ---------------------------------------------------------------

library(tidyverse)

# setup -------------------------------------------------------------------

# define input data file
df = readRDS("data/processed/proc_acou_photoid.rds") 

# time in hours
time = 24

# space in km
space = 150

# if directory does not exist make sure it does
if(!dir.exists('figures/time_space_scatplots')){dir.create('figures/time_space_scatplots')}

# figure directory 
fig_dir = 'figures/time_space_scatplots'

# proccess - call production rate -----------------------------------------

# call production by yday categorized by year and call type
pltp = df %>%
  select("yday", "year", "up_per_hr_per_whale", "mf_per_hr_per_whale", "gs_per_hr_per_whale") %>%
  gather(key = "calltype" , value = "cval", up_per_hr_per_whale, mf_per_hr_per_whale, gs_per_hr_per_whale) %>%
  filter(calltype %in% c("up_per_hr_per_whale", "mf_per_hr_per_whale", "gs_per_hr_per_whale")) %>%
  ggplot(aes(x = as.Date(yday,'1970-01-01'), y = cval))+
  geom_point(aes(colour = calltype), size = 3, shape = 1, alpha = 1)+
  # geom_smooth(aes(color = age, fill = age), method = "lm",
  #             se = TRUE, fullrange = TRUE, show.legend = FALSE)+
  scale_colour_manual(values = c("red", "blue", "black"),
                      breaks=c("up_per_hr_per_whale", "mf_per_hr_per_whale", "gs_per_hr_per_whale"),
                      labels=c("Upcall calls per hour per whale", "Mid-freq calls per hour per whale", "Gunshot calls per hour per whale"))+
  scale_shape_manual(values = c(21,22,24))+
  labs(colour = "Call Production Type",
       x = NULL,
       y = NULL,
       shape = "Year")+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,1))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  labs(x = NULL,
       y = NULL,
       title = 'Call Production Type')+ #(expression(paste('b) Age, Adult ',rho, ' = 0.0622, ', 
  #'Juvenile ', rho, ' = 0.288'))))+
  theme(text = element_text(size=14, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(legend.position = "none")+
  ylab('Production Rate (call/hour/whale)')+
  xlab('Time (Week)')+
  facet_wrap(~calltype, ncol = 1)
pltp

# proccess - demographics -------------------------------------------------

# demogrpahics by yday categorized by year and jf, jm, af, am, us, ua
pltd = df %>%
  select("yday", "year", "juvenile_female", "juvenile_male", "adult_female", "adult_male", "calf", "unknown_sex", "unknown_age") %>%
  gather(key = "dem" , value = "cval", juvenile_female, juvenile_male, adult_female, adult_male, calf, unknown_sex, unknown_age) %>%
  filter(dem %in% c("juvenile_female", "juvenile_male", "adult_female", "adult_male")) %>%
  ggplot(aes(x = as.Date(yday,'1970-01-01'), y = cval))+
  geom_point(aes(colour = dem), shape = 1, size = 3, alpha = 1)+
  # geom_smooth(aes(color = age, fill = age), method = "lm",
  #             se = TRUE, fullrange = TRUE, show.legend = FALSE)+
  scale_colour_manual(values = c("red", "blue", "black", "orange", "purple", "yellow", "green"),
                      breaks=c("juvenile_female", "juvenile_male", "adult_female", "adult_male"),
                      labels=c("Juvenile Female", "Juvenile Male", "Adult Female", "Adult Male"))+
  scale_shape_manual(values = c(21,22,24))+
  labs(colour = "Demographics",
       x = NULL,
       y = NULL,
       shape = "Year")+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  scale_y_continuous(expand = c(0,1))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  labs(x = NULL,
       y = NULL,
       title = 'Demographics')+ #(expression(paste('b) Age, Adult ',rho, ' = 0.0622, ', 
  #'Juvenile ', rho, ' = 0.288'))))+
  theme(text = element_text(size=14, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(legend.position = "none")+
  ylab('Count (individual whale)')+
  xlab('Time (Week)')+
  facet_wrap(~dem)
pltd

# proccess - behaviour rate -----------------------------------------------

# behaviour by yday categorized by year and behaviour type
pltb = df %>%
  select("yday", "year", "foraging_bhv_whale", "social_bhv_whale", "other_bhv_whale") %>%
  gather(key = "bhv" , value = "cval", foraging_bhv_whale, social_bhv_whale, other_bhv_whale) %>%
  filter(bhv %in% c("foraging_bhv_whale", "social_bhv_whale", "other_bhv_whale")) %>%
  ggplot(aes(x = as.Date(yday,'1970-01-01'), y = cval))+
  geom_point(aes(colour = bhv), shape = 1, size = 3, alpha = 1)+
  # geom_smooth(aes(color = age, fill = age), method = "lm",
  #             se = TRUE, fullrange = TRUE, show.legend = FALSE)+
  scale_colour_manual(values = c("red", "blue", "black"),
                      breaks=c("foraging_bhv_whale", "social_bhv_whale", "other_bhv_whale"),
                      labels=c("Foraging behaviour per whale", "Social behaviour per whale", "Other behaviour per whale"))+
  scale_shape_manual(values = c(21,22,24))+
  labs(colour = "Behaviour Rate",
       x = NULL,
       y = NULL,
       shape = "Year")+
  scale_x_date(date_breaks="weeks", date_labels="%b-%d")+
  # scale_y_continuous(expand = c(0,1))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position="bottom")+
  theme(axis.text.x = element_text(), 
        legend.position="right")+
  labs(x = NULL,
       y = NULL,
       title = 'Behaviour')+ #(expression(paste('b) Age, Adult ',rho, ' = 0.0622, ', 
  #'Juvenile ', rho, ' = 0.288'))))+
  theme(text = element_text(size=14, family = "serif"))+
  theme(axis.text.y = element_text(colour = "black"))+
  theme(axis.text.x = element_text(colour = "black"))+
  theme(legend.position = "none")+
  ylab('Behaviour Rate (behaviour/whale)')+
  xlab('Time (Week)')+
  facet_wrap(~bhv, ncol = 1)
pltb

# save! -------------------------------------------------------------------

# saving plots
# call production rate
ggsave(pltp, filename = paste0(fig_dir, "/call_production_", time, "hr_", space, "km.png"), 
       height = 7, width = 5, units = 'in', dpi = 300)
# demographics
ggsave(pltd, filename = paste0(fig_dir, "/demographics_", time, "hr_", space, "km.png"), 
       height = 7, width = 7, units = 'in', dpi = 300)
# behaviour rate
ggsave(pltb, filename = paste0(fig_dir, "/behaviour_rate_" , time, "hr_", space, "km.png"), 
       height = 7, width = 5, units = 'in', dpi = 300)
