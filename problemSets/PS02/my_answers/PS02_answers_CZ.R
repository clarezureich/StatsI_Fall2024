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
setwd("/Users/clarezureich/Documents/GitHub/StatsI_Fall2024/problemSets/PS02/my_answers")

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################
###
#Part (a)
###
#           Not Stopped   Bribe requested Stopped/given warning
# Upper class     14             6                7
# Lower class     7              7                1

#Create contingency table
bribery_table <- matrix(
                c(14,7,6,7,7,1),
                nrow = 2,
                ncol = 3,
                byrow = FALSE
)
rownames(bribery_table) = c("Upper Class", 
                            "Lower Class")
colnames(bribery_table) = c("Not Stopped", 
                            "Bribe Requested", 
                            "Stopped/Given Warning")
#Hypotheses
#The null hypothesis is that the class of the driver and officers' response are statistically independent
#The alternative hypothesis is that the class of the driver and officers' response are statistically dependent

#Calculate a Test Statistic
row_total <- rowSums(bribery_table)
column_total <- colSums(bribery_table)
table_total <- sum(bribery_table)
df = (nrow(bribery_table)-1)*(ncol(bribery_table)-1)

#Expected table 
expected_table <- matrix(0, nrow = 2, ncol = 3)
for (i in 1:nrow(bribery_table)) {
  for (j in 1:ncol(bribery_table)) {
    expected_table[i, j] <- (row_total[i] * column_total[j]) / table_total
  }
}
rownames(expected_table) <- rownames(bribery_table)
colnames(expected_table) <- colnames(bribery_table)

#Test statistic
chi_square <- sum((bribery_table-expected_table)^2/expected_table)
chi_square

###
#Part (b)
###
p_value = pchisq(chi_square, df = df, lower.tail = FALSE)
p_value
#There is insufficient evidence to reject the null hypothesis that the two variables are independent


###
#Part (c)
###
adjusted_residuals <- matrix(0, nrow = 2, ncol = 3)
row_prop = row_total/table_total
col_prop = column_total/table_total
residuals <- matrix(0, nrow = 2, ncol = 3)

for (i in 1:nrow(bribery_table)) {
  for (j in 1:ncol(bribery_table)) {
    adjusted_residuals[i, j] <- (bribery_table[i, j] - expected_table[i, j]) / 
      sqrt(expected_table[i, j] *
             (1-row_prop[i]) * 
             (1-col_prop[j]))
  }
}
rownames(expected_table) <- rownames(bribery_table)
colnames(expected_table) <- colnames(bribery_table)
adjusted_residuals

###
#Part (d) 
###
#The standardized residuals help us understand how far away each observed value is from "expectation". 
#None of absolute values of the standardized residuals exceed 2, which indicates that the number of cases 
#in each cell is significantly close to the expected number of cases, if the null hypothesis were true
#at a .95 confidence level.
#add more no evidence of lack of fit


#####################
# Problem 2
#####################
women <- read.csv("women.csv")
###
#Part (a)
###
#Null hypothesis: The reservation policy has no effect on the number of new or repaired drinking water facilities in the villages
#Alternative hypothesis: The reservation policy has an effect on the number of new or repaired drinking water facilities in the villages

#Slope test
#Null hypothesis: beta = 0
#Alternative hypothesis: beta != 0

#Correlation test
#Null hypothesis: p = 0
#Alternative hypothesis: P != 0


###
#Part (b)
###

#add assumptions and scatterplot
str(women)
head(women)

regression_model <- lm(water ~ reserved, data = women)
summary(regression_model)
coefficients(regression_model)

png("regression_summary.png", width = 800, height = 600)
plot(summary(regression_model))
dev.off()
###
#Part (c)
###
#There is sufficient evidence to reject the null hypotheses that there is no relationship between the reservation
#policy and the number of new/repaired drinking water #facilities, at a 95% confidence level, as the p-value of 
#the coefficient is .0197. For each additional female leader, the number of new or repaired drinking water 
#facilities will, on average, increase by 9.25. 

