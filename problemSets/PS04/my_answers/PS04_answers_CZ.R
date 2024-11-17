#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)
lapply(c("ggplot2", "stargazer"),  pkgTest)



# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##Question 1 ###
# read in data
install.packages("car")
library("car")
data(Prestige)
help(Prestige)

#Part A New Variable
Prestige$professional <- as.factor(ifelse(Prestige$type=="prof", 1, 
    ifelse(Prestige$type=="bc" | Prestige$type=="wc", 0, NA)))

levels(Prestige$professional)
summary(Prestige$professional)

#Part B
prestige_regression <- lm(prestige ~ income + professional + income * professional, data = Prestige)
summary(prestige_regression)

#Part C 
#prestige = 21.1422 + .003*income + 37.781*professional - .002(income*professional)

#Part D
#When not professional, a one unit increase in income ($1), will on average, result in a .003 
#increase in prestige

#Part E
#When income is 0, the change from not-professional (blue or white collar workers) to professional
#will, on average, result in a 37.78 increase in prestige. 
#This is the difference in prestige between the groups (professional vs white/blue collar
#worker) when income is 0.

#Part F
#Prediction equation: prestige = 21.1422 + .003*income + 37.781*professional -. 002(income*professional)
income_change <- 21.1422 + (.003*1000) + (37.781*1) - (.002*1000*1)
income_change
income_change2 <- 21.1422 + (.003*0) + (37.781*1) - (.002*0*1)
income_change2
prestige_change = income_change-income_change2
prestige_change
#There is a 1 unit increase in prestige when income increases by $1000 for professionals

#Part G
professional_change <- 21.1422 + (.003*6000) + (37.781*1) - (.002*6000*1)
professional_change
professional_change2 <- 21.1422 + (.003*6000) + (37.781*0) - (.002*6000*0)
professional_change2
prestige_change2 <- professional_change - professional_change2
prestige_change2
#With an income of $6,000, a change from non-professional to professional will increase prestige
#by 25.781, on average.

##Question 2##
#Part A
#Assumptions:
#Samples from the population are independant and random samples
#Population distribution of the response variable for each group is normal
#Standard deviation of the population distribution is constant

#Null hypothesis: the yard signs have no affect on vote share (beta1 = 0)
#Alternative hypothesis: the yard signs have an affect on vote share (beta1 != 0)

#Find the test statistic
t_stat <- .042/.016
t_stat

#Find the p-value
df <- 131-2-1
p_value <- 2*pt(t_stat, df, lower.tail = FALSE)
p_value

#Conclusion: Sufficient evidence to reject the null hypothesis since the p-value (.0097) is lower than the significance level 
#of .05. Therefore, a one unit increase in yard signs will, on average, increase vote share by .042.

#Part B
t_stat_adjacent <- .042/.013
t_stat_adjacent
p_value_adjacent <- 2*pt(t_stat_adjacent, df, lower.tail = FALSE)
p_value_adjacent
#Conclusion: Sufficient evidence to reject the null hypothesis since the p-value (.0016) is lower than the significance level 
#of .05. Therefore, a one unit increase in yard signs will, on average, increase vote share by .042.

#Part C
#The baseline group, a district with no assigned lawn signs and not adjacent to a lawn sign precinct, is expected to have 
#a vote share of .302 per Cuccinelli. The t-stat for the constant is 27.45, which is large compared to the 
#test statistics calculated for the precinct assigned lawn signs and precinct adjacent to lawn signs coefficients. 
t_stat_constant <- .302/.011
t_stat_constant

#Part D
#The R^2 for this regression is .094. Given that R^2 falls between 0 and 1, this is a very small
#R^2. Only 9.4% the variation in vote share is explained by the yard signs in or adjacent to precincts. 
#There are other factors that need to be accounted for when trying to explain vote share besides just
#yard sign placement. 
