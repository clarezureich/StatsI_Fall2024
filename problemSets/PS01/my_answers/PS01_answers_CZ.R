#####################
# load libraries
#set wd
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

#Set working directory to answers file path
setwd("/Users/clarezureich/Documents/GitHub/StatsI_Fall2024/problemSets/PS01/my_answers")

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

# Question 1
scores <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
#Step 1: Calculate y_bar

mean_scores <-mean(scores) 

#Step 2: Calculate S and sigma_hat_y
sd_scores <- sd(scores)
se_scores <- sd_scores/sqrt(length(scores))

#Step 3 Area under the curve
#Area to the right: (1-.9)/2
#Area to the left: (.9)/2

#Step 4: Find the T-score associated with confidence level and DF
t90 <- qt((.9+(1-.9)/2), df=length(scores)-1)

#Step 5: Calculate the Confidence Interval
lower_90 <- mean_scores - t90*se_scores
upper_90 <- mean_scores + t90*se_scores

# Question 2

#Hypothesis Testing Step 1 (Sample size and type of data)
sample_size = length(scores)
class(scores)
str(scores)
#Assuming the data is normally distributed, randomly sampled, and continuous quantitative data

#Hypothesis Testing Step 2 (Null and Alternative Hypothesis)
#Null hypothesis: The average IQ scores of the counselor's students is less than or equal to 100, the national IQ score
#Alternative hypothesis: The average IQ scores of the counselor's students is greater than 100, the national IQ score

#Hypothesis Testing Step 3 (Test Statistic)
national_average = 100
test_statistic <- (mean_scores-national_average)/se_scores


#Hypothesis Testing Step 4 (P-Value - one-sided T test)
p_value <- (1 - pt(test_statistic, length(scores)-1))

#Conclusion

#Short-hand way to check results above
t.test(scores, mu = 100, alternative = "greater", conf.level = 0.95)



#####################
# Problem 2
#####################

#Relationships between Y and X1, X2, X3
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
summary(expenditure)
names(expenditure) 
str(expenditure)


#Lines of best fit for all relationships
fit1 <- lm(Y ~ X1, data = expenditure)
fit2 <- lm(Y ~ X2, data = expenditure)
fit3 <- lm(Y ~ X3, data = expenditure)
fit4 <- lm(X2 ~ X1, data = expenditure)
fit5 <- lm(X3 ~ X1, data = expenditure)
fit6 <- lm(X3 ~ X2, data = expenditure)

#Correlations for all relationships
png(file="political_economy_scatter_plot.png",
    width = 500,
    height = 400)
par(mfrow = c(2,3))
plot(expenditure$X1, expenditure$Y,col=1, 
     ylab = "Per Capita Housing Expenditure In State", 
     xlab = "Per Capita Personal Income In State", 
     main = "Housing Expenditure vs\n Personal Income")
abline(fit1, col = 1, lwd = 2)
plot(expenditure$X2, expenditure$Y,col=2, 
     ylab = "Per Capita Housing Expenditure In State", 
     xlab = "Financially Insecure Residents In \nState, per 100,000", 
     main = "Housing Expenditure vs\n Financially Insecure Residents")
abline(fit2, col = 2, lwd = 2)
plot(expenditure$X3, expenditure$Y,col=3, 
     ylab = "Per Capita Housing Expenditure In State", 
     xlab = "Urban Area Residents In State, per 1,000", 
     main = "Housing Expenditure vs\n Urban Area Residents")
abline(fit3, col = 3, lwd = 2)
plot(expenditure$X1, expenditure$X2,col=4, 
     xlab = "Per Capita Personal Income In State", 
     ylab = "Financially Insecure Residents In State, per 100,000", 
     main = "Financially Insecure Residents vs\n Personal Income") 
abline(fit4, col = 4, lwd = 2)
plot(expenditure$X1, expenditure$X3,col=5, 
     xlab = "Per Capita Personal Income In State", 
     ylab = "Urban Area Residents In State, per 1,000", 
     main = "Urban Area Residents vs\n Personal Income")
abline(fit5, col = 5, lwd = 2)
plot(expenditure$X2, expenditure$X3,col=6, 
     xlab = "Financially Insecure Residents In \nState, per 100,000", 
     ylab = "Urban Area Residents In State, per 1,000", 
     main = "Urban Area Residents vs\n Financially Insecure Residents")
abline(fit6, col = 6, lwd = 2)
dev.off()


#Question 2, Part 2 (Housing Expenditure by region)
# Housing expenditure average, by region  
means1 <- mean(expenditure$Y[expenditure$Region == 1])
means2 <- mean(expenditure$Y[expenditure$Region == 2])    
means3 <- mean(expenditure$Y[expenditure$Region == 3])   
means4 <- mean(expenditure$Y[expenditure$Region == 4])   

#Graph between Y and Region 
png(file="housing_expenditure_region__scatter_plot.png")
par(mfrow = c(1,1))
region <- factor(expenditure$Region, 
                 levels = c(1, 2, 3, 4), 
                 labels = c("Northeast", "North Central", "South", "West"))
expenditure$name <- region
plot(region, expenditure$Y, 
     ylab = "Housing expenditure", 
     xlab = "Region", 
     main = "Housing Expenditure by\n Region")
text(1, 45, sprintf("Mean=%s", round(means1, 4)))
text(2, 45, sprintf("Mean=%s", round(means2, 4)))
text(3, 45, sprintf("Mean=%s", round(means3, 4)))
text(4, 45, sprintf("Mean=%s", round(means4, 4)))
dev.off()

#Graph between X1 and Y, by region
png(file="housing_expenditure_personal_income_scatter_plot.png")
plot(expenditure$X1, expenditure$Y,col=expenditure$Region, 
     ylab = "Per Capita Housing Expenditure In State", 
     pch = expenditure$Region,
     xlab = "Per Capita Personal Income In State", 
     main = "Housing Expenditure vs\n Personal Income")
legend("bottomright", legend=unique(expenditure$name),
       col=unique(expenditure$Region),
       pch=unique(expenditure$Region)) 
abline(fit1, col = 1)
dev.off()

