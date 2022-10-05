# fig_corr_matrix_manuscript.R #

# correlation matrix figure for manuscript

# libraries ---------------------------------------------------------------

library(lubridate)
library(Hmisc)
library(corrplot)
library(RColorBrewer)

# input -------------------------------------------------------------------

# read in data 
df= readRDS("data/processed/proc_acou_photoid_fliptest.rds")

# figure directory
fig_dir = 'figures/' 

# process -----------------------------------------------------------------

# manuscript figure -------------------------------------------------------

# select variables correlation matrix plot (spearman)
# figure set up
# jpeg (filename = paste0(fig_dir,'fig_corrplot_manu_testing10_sansfont_fliptest_mspretty.jpeg'), 
#       width = 6.5, height = 5.85, units = 'in', res = 300)
pdf (file = paste0(fig_dir,'fig_corrplot_manu_testing10_sansfont_fliptest_mspretty.pdf'),
      width = 6.5, height = 5.85)

# put data in dataframe so that it can be convereted to correlation and p-value matrix
dfs2 = data.frame(df$up_per_hr,df$gs_per_hr,df$mf_per_hr,
                  df$num_sighting,
                  df$ratio_male_female,
                  df$foraging_bhv_whale,df$social_bhv_whale,
                  df$yday)

# rename columns/rows to make it pretty
colnames(dfs2) <- c("Upcall rate", "Gunshot rate", "Tonal rate", 
                    "Whale count", 
                    "Male/female ratio", "Foraging rate", "Socalizing rate", "Day of year")
rownames(dfs2) <- c("Upcall rate", "Gunshot rate", "Tonal rate", 
                    "Whale count", 
                    "Male/female ratio", "Foraging rate", "Socalizing rate", "Day of year")

# obtain spearman correlation matrix
sp2 = cor(dfs2, method = 'spearman')
sp2 = round(sp2, 3)

# spearman p-value matrix
test2 <- rcorr(as.matrix(dfs2), type="spearman")
test2$P = round(test2$P, 3)

# change font type
par(family="Helvetica", cex = 0.9)

fig = corrplot(sp2, tl.col = "black", method = 'color', p.mat = test2$P,
               sig.level = 0.05, type = "upper", diag = FALSE, 
               tl.srt = 45, #colorlegend(colbar = COL2("RdBu"), labels = lgn, 
                                       #at = seq(0.05, 0.95, len = 12), 
                                       #xlim = c(0, 6), ylim = c(1.1, 1.2),
                                       #vertical = TRUE, align = "r"),
               col = brewer.pal(n = 8, name = "RdBu"),addCoef.col = "black")

# add legend
lgn = expression(paste(rho, " value"))
#mtext(text = lgn, side = 4)
text(9.5,5,lgn,srt = 90)


# end figure
dev.off()


# appendix figure - all var -----------------------------------------------

# all variables correlation matirx plot (spearman)
# figure set up
jpeg (filename = paste0(fig_dir,'fig_corrplot_full_manu2_sansfont_fliptest.jpeg'), 
      width = 10, height = 9, units = 'in', res = 200)

# put data in dataframe so that it can be convereted to correlation and p-value matrix
dfs2 = data.frame(df$up_per_hr,df$gs_per_hr,df$mf_per_hr,
                  df$num_sighting,
                  df$ratio_male_female,df$ratio_juvenile_adult,
                  df$adult_male,df$adult_female,df$juvenile_male,df$juvenile_male,df$calf_male,df$calf_female,df$unknown,
                  df$foraging_bhv_whale,df$social_bhv_whale,df$other_bhv_whale,
                  df$yday)

# rename columns/rows to make it pretty
colnames(dfs2) <- c("Upcall rate", "Gunshot rate", "Tonal rate", 
                    "Whale count", 
                    "Male/female ratio", "Juvenile/adult ratio",
                    "Adult male","Adult female","Juvenile male","Juvenile female","Calf male", "Calf female", "Unknown demographic",
                    "Foraging rate", "Socalizing rate", "'Other behavior' rate",
                    "Day of year")
rownames(dfs2) <- c("Upcall rate", "Gunshot rate", "Tonal rate", 
                    "Whale count", 
                    "Male/female ratio", "Juvenile/adult ratio",
                    "Adult male","Adult female","Juvenile male","Juvenile female","Calf male", "Calf female","Unknown demographic",
                    "Foraging rate", "Socalizing rate", "'Other behavior' rate",
                    "Day of year")

# obtain spearman correlation matrix
sp2 = cor(dfs2, method = 'spearman')
sp2 = round(sp2, 3)

# spearman p-value matrix
test2 <- rcorr(as.matrix(dfs2), type="spearman")
test2$P = round(test2$P, 3)

# change font type
par(family="Helvetica") 

# correlation matrix figure
fig = corrplot(sp2, tl.col = "black", method = 'color', p.mat = test2$P,
               sig.level = 0.05, type = "upper", diag = FALSE, tl.srt = 45,
               col = brewer.pal(n = 8, name = "RdBu"),addCoef.col = "black",)

# add legend
lgn = expression(paste(rho, " value"))
text(20,9.5,lgn,srt = 90)

# end figure
dev.off()

