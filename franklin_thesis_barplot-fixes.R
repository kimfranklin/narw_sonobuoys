## franklin_thesis_barplot ##
# several suggestions for making a barplot with Franklin's thesis data

# read in data
df = readRDS("data/processed/sight_dfs.rds")

# libraries
library(tidyverse)

# wrangle plot data (coercing all data to the same year makes plotting much easier)
dfp = df %>%
  select("yday", "up_dur_hr", "mf_dur_hr", "gs_dur_hr") %>%
  mutate(date = as.Date(yday, origin = '2018-01-01')) %>% 
  gather(key = "call_type" , value = "cval", up_dur_hr, mf_dur_hr, gs_dur_hr)

# plot
plt = ggplot(data = dfp, aes(x=date,y=cval,fill=call_type))+
  geom_col(position = "stack")+
  
  # remove white space around plot
  scale_y_continuous(expand = c(0,0))+
  
  # specify weekly axis labels
  scale_x_date(breaks = '1 week', date_labels = '%b-%d')+
  
  # make pretty
  scale_fill_grey(start = 0.1, end = 0.7,  
                  breaks=c("up_dur_hr", "mf_dur_hr", "gs_dur_hr"),
                  labels=c("Upcall", "Mid-frequency","Gunshot"))+
  labs(fill = "Call Type",
       y = 'Call Rate (call/hour)',
       x = NULL)+
  theme_bw()+
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=14, family = "serif"))

plt
