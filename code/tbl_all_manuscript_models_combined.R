## tbl_all_manuscript_models_combined.R ##

# combining all the models in a single table for the manuscript

# libraries
library(tidyverse)
library(plyr)
library(broom)
library(data.table)

# read in the data
sight_coeff = read.csv("data/processed/num_sighting_regression_table.csv")
sight_pval = read.csv("data/processed/num_sighting_anova_table.csv")
cr_coeff = read.csv("data/processed/call_rate_regression_table.csv")
cr_pval = read.csv("data/processed/call_rate_likelihood_table.csv")
cr_reg_coeff = read.csv("data/processed/call_rate_stepregression_coeff_table.csv")
cr_reg_pval = read.csv("data/processed/call_rate_stepwise_vs_null_table.csv")

# process -----------------------------------------------------------------

# formatting characterizing whale count models

# remove columns not needed 
sight_coeff = sight_coeff[,!(names(sight_coeff) %in% c('t.value','Pr...t..'))] 

# separate data into two tables, one with only the intercepts and the other with
# the variables, this is so i can combine them horizontally (they are combined 
# vertically and we do not want this for when we finalize formatting in Excel)
tmp = sight_coeff %>%
  filter(X == "(Intercept)")

tmp2 = subset(sight_coeff, X != "(Intercept)")

# combine the two tables horizontally 
sight_coeff2 = merge(tmp,tmp2,by="X.1")

# remove unnecessary rows since merge did it for every possible combination
sight_coeff2 = sight_coeff2[-c(2, 3, 4, 5, 7, 8, 9, 10, 12, 13, 14, 15), ]

# add in anova p-values
# remove rows with NA, we only want the p-value
tmp = subset(sight_pval, F.value != 'NA')

# remove columns not needed
tmp = tmp[,!(names(tmp) %in% c('Df','Sum.Sq','Mean.Sq','F.value','X.whale.abundance.'))]

# combine the table with the coefficients with the anova p-values
sight_mods = merge(sight_coeff2,tmp, by.x="X.y", by.y="X", all.x = TRUE)

# re-order the columns
sight_mods = sight_mods[c('X.1','Estimate.y','Std..Error.y','X.y','Estimate.x','Std..Error.x','X.x','Pr..F.')]

# rename and add blank columns this will help when we rbind with other models later (bottom of script)
sight_mods = sight_mods %>% 
  dplyr::rename(
    X.1.x = X.1
  )

sight_mods$Estimate = NA
sight_mods$Std..Error = NA
sight_mods$X = NA
sight_mods$Pr.Chi. = NA


# formatting characterizing single variable call rate models

# add an arbitrary index, this will make combining intercept table and variable
# table horizontally easier
cr_coeff$index = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14,15,15)

# remove columns not needed
cr_coeff = cr_coeff[,!(names(cr_coeff) %in% c('z.value','Pr...z..'))]

# separate data into two tables, one with only the intercepts and the other with
# the variables, this is so i can combine them horizontally (they are combined 
# vertically and we do not want this for when we finalize formatting in Excel)
tmp = cr_coeff %>%
  filter(X == "(Intercept)")

tmp2 = subset(cr_coeff, X != "(Intercept)")

# combine the two tables horizontally
cr_coeff2 = merge(tmp, tmp2, by.x="index", by.y="index", all.x = TRUE)

# add in likelihood ratio test p-values
# remove rows with NA, we only want the p-value
tmp = subset(cr_pval, Pr.Chi. != 'NA')

# remove columns not needed
tmp = tmp[,!(names(tmp) %in% c('X','theta','Resid..df','X...2.x.log.lik.','Test','X...df','LR.stat.','call_type'))]

# add index to p-value table - this will help when we merge the variable table 
# with the p-value table
tmp$index = 1:nrow(tmp) 

# combine the table with the coefficients with the likelihood p-values
cr_mods = merge(cr_coeff2,tmp, by.x="index", by.y="index", all.x = TRUE)

# remove columns not needed
cr_mods = cr_mods[,!(names(cr_mods) %in% c('index','X.1.y','Model'))]

# re-order the columns
cr_mods = cr_mods[c('X.1.x','Estimate.y','Std..Error.y','X.y','Estimate.x','Std..Error.x','X.x','Pr.Chi.')]

# add blank columns this will help when we rbind with other models later (bottom of script)
cr_mods$Estimate = NA
cr_mods$Std..Error = NA
cr_mods$X = NA
cr_mods$Pr..F. = NA


# formatting characterizing stepwise produced call rate models

# add an arbitrary index, this will make combining intercept table and variable
# table horizontally easier
cr_reg_coeff$index = c(1,1,1,2,2,3,3,3)

# remove columns not needed
cr_reg_coeff = cr_reg_coeff[,!(names(cr_reg_coeff) %in% c('z.value','Pr...z..'))]

# we cannot separate the intercept and variable terms into two tables like last 
# time because some of the models have 2 variable terms, not just one, so we need
# to copy the data three times so we can merge all three together to get that 
# horizontal (untidy) data frame
cr_reg_coeff2 = cr_reg_coeff

cr_reg_coeff3 = cr_reg_coeff %>%
  filter(X.1 != 'gunshot') # we remove the gunshot rows because this model does not have a 2nd variable

# combine the first and second tables together
tmp = merge(cr_reg_coeff,cr_reg_coeff2, by.x="index", by.y="index", all.x = TRUE)

# combine the table made in the previous line with the table not containing gunshot data
tmp2 = merge(tmp,cr_reg_coeff3, by.x="index", by.y="index", all.x = TRUE)

# because we combined the data 3 times we made a lot of combinations of the 3 models
# so we need to pull out the models in their proper format, make sure to visually
# check which models before running this line of code
cr_reg_mods = tmp2[c(6, 30, 37), ]

# remove columns not needed
cr_reg_mods = cr_reg_mods[,!(names(cr_reg_mods) %in% c('X.1.y','X.1'))]

# add in likelihood ratio test p-values
# remove rows with NA, we only want the p-value
tmp = subset(cr_reg_pval, Pr.Chi. != 'NA')

# remove columns not needed
tmp = tmp[,!(names(tmp) %in% c('X','theta','Resid..df','X...2.x.log.lik.','Test','X...df','LR.stat.','call_type'))]

# add index to p-value table - this will help when we merge the variable table 
# with the p-value table
tmp$index = 1:nrow(tmp)

# combine the table with the coefficients with the likelihood p-values
cr_reg_mods = merge(cr_reg_mods, tmp, by.x="index", by.y="index", all.x = TRUE)

# remove columns not needed
cr_reg_mods = cr_reg_mods[,!(names(cr_reg_mods) %in% c('index','Model'))]

# re-order the columns
cr_reg_mods = cr_reg_mods[c('X.1.x',
                            'Estimate.y','Std..Error.y','X.y',
                            'Estimate','Std..Error','X',
                            'Estimate.x','Std..Error.x','X.x',
                            'Pr.Chi.')]

# add blank column, this will help when we rbind with other models (next step)
cr_reg_mods$Pr..F. = NA


# combine all models in single dataframe
mods = rbind(sight_mods,cr_mods,cr_reg_mods)

# save
write.csv(mods,"data/processed/manuscript_table_of_models.csv")


# attempting formatting 
mods$model = paste0(mods$X.1.x,' = (',mods$Estimate.y,' ± ',mods$Std..Error.y,')* ',mods$X.y,
                     ' + (',mods$Estimate,' ± ',mods$Std..Error,')* ',mods$X,
                     ' + (',mods$Estimate.x,' ± ',mods$Std..Error.x,') + offset(log(duration))')

# rearrange columns
mods = mods[c('X.1.x',
              'Estimate.y','Std..Error.y','X.y',
              'Estimate.x','Std..Error.x','X.x',
              'Estimate','Std..Error','X',
              'model','Pr..F.','Pr.Chi.')]

# if p-values less than 0.05 then change to other format
mods$Pr..F.[mods$Pr..F. <= 0.00] <- "≤ 0.05 **"
mods$Pr.Chi.[mods$Pr.Chi. <= 0.00] <- "≤ 0.05 **"

# remove columns not needed
mods = mods[,!(names(mods) %in% c('X.1.x',
                                  'Estimate.y','Std..Error.y','X.y',
                                  'Estimate.x','Std..Error.x','X.x',
                                  'Estimate','Std..Error','X'))]

# combine p-value columns together to make one column
mods2 = mods %>% 
  mutate(pvalue = coalesce(Pr..F.,Pr.Chi.)) 

# remove columns not needed
mods2 = mods2[,!(names(mods2) %in% c('Pr..F.','Pr.Chi.'))]

# save
write.csv(mods2,"data/processed/manuscript_table_of_models_official.csv")

# note in excel will need to do find and replace for space and less than sign
