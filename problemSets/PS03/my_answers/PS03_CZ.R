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

# set wd for current folder
setwd("~/Documents/GitHub/StatsI_Fall2024/problemSets/PS03/my_answers")

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")

head(inc.sub)
View(inc.sub)

#Problem 1 
#Part 1: Run the regression
regression_model1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(regression_model1)
#Intrepretation

#Part 2: Scatterplot 
png(file = "votesshare_vs_difflog.png")
plot(inc.sub$difflog, inc.sub$voteshare,
     ylab = "Incumbent's Vote share", 
     xlab = "Difference in spending logged between Incumbent and Challenger", 
     main = "Scatterplot: Incumbent's Vote Share vs Difference in \nSpending Logged")
abline(regression_model1, col=3, lwd = 2)

#Part 3: Residuals
regression_residuals1 <- residuals(regression_model1)
summary(regression_residuals1)

#Part 4: Prediction equation 
#yhat(i) = betahat0 + (betahat1 * x0)
#voteshare = 0.579 + 0.042 * difflog

#Problem 2
#Part 1
regression_model2 <- lm(presvote ~ difflog, data = inc.sub)
summary(regression_model2)
#Interpretation

#Part 2
png(file = "presvote_vs_difflog.png")
plot(inc.sub$difflog, inc.sub$presvote,
     ylab = "# of Incumbent Party's Presidential Candidates", 
     xlab = "Difference in spending logged between Incumbent and Challenger", 
     main = "Scatterplot: # of Presidential Candidates of Incumbent's \n Party vs. Difference in Spending Logged")
abline(regression_model2, col=4, lwd = 2)

#Part 3: Residuals
regression_residuals2 <- residuals(regression_model2)
summary(regression_residuals2)

#Part 4: Prediction equation 
#yhat(i) = betahat0 + (betahat1 * x0)
#presvote = 0.507 + 0.024 * difflog


#Problem 3
#Part 1
regression_model3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(regression_model3)
#Interpretation

#Part 2
png(file = "votesshare_vs_presvote.png")
plot(inc.sub$presvote, inc.sub$voteshare,
     ylab = "Incumbent's Vote share", 
     xlab = "# of Incumbent Party's Presidential Candidates", 
     main = "Scatterplot: Incumbent's Vote Share vs # of \nPresidential Candidates of Incumbent's Party")
abline(regression_model3, col=2, lwd = 2)

#Part 4: Prediction equation 
#yhat(i) = betahat0 + (betahat1 * x0)
#voteshare = 0.441 + 0.388 * presvote


#Problem 4
#Part 1
residual_regression_model <- lm(regression_residuals1 ~ regression_residuals2)
summary(residual_regression_model)
#Interpretation 

#Part 2
png(file = "Q1_vs_Q2.png")
plot(regression_residuals2, regression_residuals1,
     ylab = "Residuals of # of Presidential Candidates of Incumbent's \n Party vs. Difference in Spending Logged", 
     xlab = "Residuals of Incumbent's Vote Share vs Difference in Spending Logged", 
     main = "Scatterplot: Residauls of Regression Model 2 vs Residuals of Regression Model 1")
abline(residual_regression_model, col=6, lwd = 2)

#Part 3
#yhat(i) = betahat0 + (betahat1 * x0)
#residual_model1 = 0 + .257 * residual_model2

#Problem 5
#Part 1
multivariate_regression <- lm(voteshare ~ difflog + presvote , data = inc.sub)
summary(multivariate_regression)
#Interpretation

#Part 2
#yhat(i) = betahat0 + (betahat1 * x0)
#voteshare  = .449 + .036 * difflog + .257 * presvote

#Part 3
#The slope coefficient of the regression_residual2 variable in the residual regression model (.257) is equal to the slope coefficient of the presvote variable in the multivariate formula (.257).
#This is because the slope coefficients both represent the effect of presvote on voteshare after taking out the effects of difflog from both voteshare and presvote. This is the partial effect of presvote. 
#In regression_residuals1, we first found the residual of the linear relationship between votesahre and difflog (regression_model1), which is the part of votesahre that is not linearly related to difflog).
#In regression_residuals2, we found the resiual of the linear relationship between presvote and difflog (regression_model2), which is the part of presvote that is not linearly related to difflog. 
#We then found the linear relationship between the voteshare residual (regression_residual1) and the presvote residual (regression_residual2). The result is the coeffient which represents the effect of presvote on voteshare after taking out the effects of difflog from voteshare and presvote. 

cor(inc.sub)


cor_yx1 <- 0.606086592
cor_yx2 <- 0.453667219
cor_x1x2 <- 0.296565289


par_cor_num <- cor_yx1 - (cor_yx2*cor_x1x2)
par_cor_denom <- sqrt((1-(cor_yx2^2))*(1-(cor_x1x2)^2))
par_cor <- par_cor_num / par_cor_denom
par_cor
par_cor^2
