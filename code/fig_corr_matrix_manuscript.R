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

# figure set up
jpeg (filename = paste0(fig_dir,'fig_corrplot_manu?.jpeg'), 
      width = 10, height = 12, units = 'in', res = 200)

# put data in dataframe so that it can be convereted to correlation and p-value matrix
dfs2 = data.frame(df$up_per_hr,df$gs_per_hr,df$mf_per_hr,
                  df$num_sighting,
                  df$ratio_male_female,
                  df$foraging_bhv_whale,df$social_bhv_whale,
                  df$month)

# rename columns/rows to make it pretty
colnames(dfs2) <- c("Upcall call rate", "Gunshot call rate", "Tonal call rate", 
                    "number of whales", 
                    "Male to female ratio", "Foraging rate", "Socalizing rate", "Month")
rownames(dfs2) <- c("Upcall call rate", "Gunshot call rate", "Tonal call rate", 
                    "number of whales", 
                    "Male to female ratio", "Foraging rate", "Socalizing rate", "Month")

# obtain spearman correlation matrix
sp2 = cor(dfs2, method = 'spearman')

# spearman p-value matrix
test2 <- rcorr(as.matrix(dfs2), type="spearman")
r = round(test2$P, 5)

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

