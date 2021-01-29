# fig_corr_matrix_manuscript.R #

# correlation matrix figure for manuscript

# libraries
library(lubridate)
library(Hmisc)
library(corrplot)
library(RColorBrewer)

# read in data 
df= readRDS("data/processed/proc_acou_photoid.rds")

# figure directory
fig_dir = 'figures/' 

# select variables correlation matrix plot (spearman)
# figure set up
jpeg (filename = paste0(fig_dir,'fig_corrplot_manu?.jpeg'), 
      width = 10, height = 12, units = 'in', res = 200)

# put data in dataframe so that it can be convereted to correlation and p-value matrix
dfs2 = data.frame(df$up_per_hr,df$gs_per_hr,df$mf_per_hr,
                  df$num_sighting,
                  df$ratio_male_female,
                  df$foraging_bhv_whale,df$social_bhv_whale,
                  df$yday)

# rename columns/rows to make it pretty
colnames(dfs2) <- c("Upcall call rate", "Gunshot call rate", "Tonal call rate", 
                    "Whale abundance", 
                    "Male to female ratio", "Foraging rate", "Socalizing rate", "Yday")
rownames(dfs2) <- c("Upcall call rate", "Gunshot call rate", "Tonal call rate", 
                    "Whale abundance", 
                    "Male to female ratio", "Foraging rate", "Socalizing rate", "Yday")

# obtain spearman correlation matrix
sp2 = cor(dfs2, method = 'spearman')

# spearman p-value matrix
test2 <- rcorr(as.matrix(dfs2), type="spearman")
r = round(test2$P, 5)

# change font type
par(family="Times New Roman")

# correlation matrix figure
# corrplot(sp2, method = "color", col = brewer.pal(n = 8, name = "RdBu"),
#          type = "upper", number.cex = .7, 
#          addCoef.col = "black", # Add coefficient of correlation
#          tl.col = "black", tl.srt = 45, # Text label color and rotation
#          # Combine with significance
#          p.mat = test2$P, sig.level = 0.05, insig = "blank", 
#          # hide correlation coefficient on the principal diagonal
#          diag = FALSE)

fig = corrplot(sp2, tl.col = "black", method = 'color', p.mat = test2$P,
                                   sig.level = 0.05, type = "upper", diag = FALSE, tl.srt = 45,
                                   col = brewer.pal(n = 8, name = "RdBu"),addCoef.col = "black",)

# fig = corrplot(sp2, tl.col = "black", method = 'color', p.mat = test2$P, 
#                      sig.level = 0.05, type = "upper", diag = FALSE, tl.srt = 45,
#                      col = brewer.pal(n = 8, name = "RdBu"))

# end figure
dev.off()


# all variables correlation matirx plot (spearman)
# figure set up
jpeg (filename = paste0(fig_dir,'fig_corrplot_full_manu?.jpeg'), 
      width = 10, height = 12, units = 'in', res = 200)

# put data in dataframe so that it can be convereted to correlation and p-value matrix
dfs2 = data.frame(df$up_per_hr,df$gs_per_hr,df$mf_per_hr,
                  df$num_sighting,
                  df$ratio_male_female,
                  df$adult_male,df$adult_female,df$juvenile_male,df$juvenile_male,df$unknown,
                  df$foraging_bhv_whale,df$social_bhv_whale,df$other_bhv_whale,
                  df$yday)

# rename columns/rows to make it pretty
colnames(dfs2) <- c("Upcall call rate", "Gunshot call rate", "Tonal call rate", 
                    "Whale abundance", 
                    "Male to female ratio", 
                    "Adult male","Adult female","Juvenile male","Juvenile female","Unknown demographic",
                    "Foraging rate", "Socalizing rate", "Other behaviour rate",
                    "Yday")
rownames(dfs2) <- c("Upcall call rate", "Gunshot call rate", "Tonal call rate", 
                    "Whale abundance", 
                    "Male to female ratio", 
                    "Adult male","Adult female","Juvenile male","Juvenile female","Unknown demographic",
                    "Foraging rate", "Socalizing rate", "Other behaviour rate",
                    "Yday")

# obtain spearman correlation matrix
sp2 = cor(dfs2, method = 'spearman')

# spearman p-value matrix
test2 <- rcorr(as.matrix(dfs2), type="spearman")
r = round(test2$P, 5)

# change font type
par(family="Times New Roman") 

# correlation matrix figure
fig = corrplot(sp2, tl.col = "black", method = 'color', p.mat = test2$P,
               sig.level = 0.05, type = "upper", diag = FALSE, tl.srt = 45,
               col = brewer.pal(n = 8, name = "RdBu"),addCoef.col = "black",)


# end figure
dev.off()


# # select variables correlation matrix plot (pearson)
# # figure set up
# jpeg (filename = paste0(fig_dir,'fig_corrplot_pearson_manu?.jpeg'), 
#       width = 10, height = 12, units = 'in', res = 200)
# 
# # put data in dataframe so that it can be convereted to correlation and p-value matrix
# dfs2 = data.frame(df$up_per_hr,df$gs_per_hr,df$mf_per_hr,
#                   df$num_sighting,
#                   df$ratio_male_female,
#                   df$foraging_bhv_whale,df$social_bhv_whale,
#                   df$month)
# 
# # rename columns/rows to make it pretty
# colnames(dfs2) <- c("Upcall call rate", "Gunshot call rate", "Tonal call rate", 
#                     "number of whales", 
#                     "Male to female ratio", "Foraging rate", "Socalizing rate", "Month")
# rownames(dfs2) <- c("Upcall call rate", "Gunshot call rate", "Tonal call rate", 
#                     "number of whales", 
#                     "Male to female ratio", "Foraging rate", "Socalizing rate", "Month")
# 
# # obtain spearman correlation matrix
# sp2 = cor(dfs2, method = 'pearson')
# 
# # spearman p-value matrix
# test2 <- rcorr(as.matrix(dfs2), type="pearson")
# r = round(test2$P, 5)
# 
# # correlation matrix figure
# fig = corrplot(sp2, tl.col = "black", method = 'color', p.mat = test2$P,
#                sig.level = 0.05, type = "upper", diag = FALSE, tl.srt = 45,
#                col = brewer.pal(n = 8, name = "RdBu"),addCoef.col = "black",)
# 
# # end figure
# dev.off()


# # test matrix plot
# df$whale_rate = df$num_sighting/(df$dep_duration/60/60)
# 
# # figure set up
# jpeg (filename = paste0(fig_dir,'fig_corrplot_test_manu?.jpeg'), 
#       width = 10, height = 12, units = 'in', res = 200)
# 
# # put data in dataframe so that it can be convereted to correlation and p-value matrix
# dfs2 = data.frame(df$up_per_hr,df$gs_per_hr,df$mf_per_hr,
#                   df$num_sighting,df$whale_rate,
#                   df$ratio_male_female,
#                   df$adult_male,df$adult_female,df$juvenile_male,df$juvenile_male,df$unknown,
#                   df$foraging_bhv_whale,df$social_bhv_whale,df$other_bhv_whale,
#                   df$month)
# 
# # rename columns/rows to make it pretty
# colnames(dfs2) <- c("Upcall call rate", "Gunshot call rate", "Tonal call rate", 
#                     "number of whales", "Rate of whales",
#                     "Male to female ratio", 
#                     "Adult male","Adult female","Juvenile male","Juvenile female","Unknown demographic",
#                     "Foraging rate", "Socalizing rate", "Other behaviour rate",
#                     "Month")
# rownames(dfs2) <- c("Upcall call rate", "Gunshot call rate", "Tonal call rate", 
#                     "number of whales", "Rate of whales",
#                     "Male to female ratio", 
#                     "Adult male","Adult female","Juvenile male","Juvenile female","Unknown demographic",
#                     "Foraging rate", "Socalizing rate", "Other behaviour rate",
#                     "Month")
# 
# # obtain spearman correlation matrix
# sp2 = cor(dfs2, method = 'spearman')
# 
# # spearman p-value matrix
# test2 <- rcorr(as.matrix(dfs2), type="spearman")
# r = round(test2$P, 5)
# 
# # correlation matrix figure
# fig = corrplot(sp2, tl.col = "black", method = 'color', p.mat = test2$P,
#                sig.level = 0.05, type = "upper", diag = FALSE, tl.srt = 45,
#                col = brewer.pal(n = 8, name = "RdBu"),addCoef.col = "black",)
# 
# 
# # end figure
# dev.off()
