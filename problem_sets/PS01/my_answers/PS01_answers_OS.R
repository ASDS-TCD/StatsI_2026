###############################################################################
# Title:        Applied Statistics 1 - Problem Set 1
# Description:  Problem Set 1 for Trinity College's Applied Stats 1 course
# Author:       Owen Stidman
# R version:    R 4.6.1
###############################################################################


#####################
# load libraries
# set wd
# clear global .envir
#####################

library(tidyverse)
getwd()

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
lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

install.packages("tidyverse")
library(tidyverse)
search()

#####################
# Problem 1, Part 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# Calculating the mean and standard deviation to construct the confidence 
# interval and assigning to objects with descriptive variable names:
mean_iq <- mean(y)
standard_dev_iq <- sd(y)

# Returning the values of the mean and standard deviation:
mean_iq
standard_dev_iq

# The sample size n = 25; therefore, the degrees of freedom df = n-1 = 24.

# The formula for a confidence interval for a mean is y-bar ± t(s / sqrt(n)),
# where y-bar is the sample mean, t is the critical value from the 
# t-distribution, s is the sample standard deviation, and n is the sample size.

# The value s/sqrt(n) is the standard error.

# As we know y-bar (98.44 mean IQ, calculated previously), s (13.09287,
# calculated previously), and n (given), the only remaining unknown value is
# the critical value from the t-distribution. 

# Using Table B (t-distribution critical values) from the Agresti and Finlay
# 2009 textbook, we see that the critical value for a 90% confidence interval
# with degrees of freedom df = 24 is t = 1.711. 

# Therefore, y-bar ± t(s / sqrt(n)) = 98.44 ± 1.711(13.09287/sqrt(25))

#Calculating the lower and upper values of the 90% confidence interval and 
# assigning them to objects:
interval_lower_bound <- 98.44 - 1.711 * (13.09287 / 5)
interval_upper_bound <- 98.44 + 1.711 * (13.09287 / 5)

# Returning the lower and upper values of the interval:
interval_lower_bound
interval_upper_bound

# The 90 percent confidence interval for the mean IQ score is (93.95962, 102.9204).

#####################
# Problem 1, Part 2
#####################

# Although the counselor is wondering whether the mean IQ of students at the school
# is "higher than" (rather than "different than") the nationwide average IQ of
# 100, the sample mean of 98.44 is lower than 100. This suggests that, in spite
# of the counselor's framing, a two-sided significance test, rather than a one-
# sided test, is appropriate.

# 1. ASSUMPTIONS: 

# We can reasonably assume the sample was randomly selected, as the assignment 
# states the counselor "took a random sample." 

# We can also reasonably assume the quantitative variable in question (IQ) has a 
# normal population distribution, as "Most people score near the average range, 
# which is designed to fit into a normal distribution curve" (Kendra Cherry, MSEd).

# 2. HYPOTHESES: 

# Let mean_iq denote the mean IQ.

# Ho: mean_iq = 100

# Ha: mean_iq ≠ 100

# 3. TEST STATISTIC: 

# The n = 25 observations are summarized by a sample mean IQ of 98.44 and sample 
# standard deviation of 13.09287, both calculated previously.

# The degrees of freedom df = n - 1 = 24. 

# The estimated standard error is se = s / sqrt (n) = 13.09287 / 5.
13.09287 / 5
# This computes to 2.618574.

# The value of the test statistic is the difference between the observed sample
# mean (98.44) and the population mean (100) divided by the standard error
# (2.618574).
t <- (mean(y)-100)/(sd(y)/sqrt(length(y)))
t
# This computes to -0.5957439. 

# In other words, the sample mean falls 0.5957439 estimated standard errors
# below the null hypothesis value of the mean. 

# 4. P-VALUE:

# The P-value is the one-tail probability,  presuming Ho is true, that t would
# exceed 0.5957439 in absolute value. 

# From the t-distribution with df = 24, the one-tail probability is
p_value <- pt(-abs(t),df=length(y)-1, lower.tail = FALSE)
p_value
# Using the "pt" command in R, 
# this computes to P = 0.7215383. 
# With our alpha-level of 0.05, P > alpha. 

# We fail to reject the null hypothesis at the alpha = 0.05 significance level.

# The counselor's findings are not statistically significant. 

#####################
# Problem 2, Part 1
#####################

# I import the data file and save a copy:
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)
expenditure_demo <- expenditure

# Exploring the expenditure dataset:

str(expenditure)
summary(expenditure)
head(expenditure)
glimpse(expenditure)

# 1. Plotting housing expenditure Y against per capita income X1

# I create a simple linear regression model and calculate the slope, intercept
# and correlation (R-squared):
regression_y_on_x1 <- lm(Y ~ X1, data = expenditure_demo)
lm(formula = Y ~ X1, data = expenditure_demo)
summary(regression_y_on_x1)

# I create a scatterplot of housing expenditure against per capita income and
# include the regression line I created using the lm function:
library(ggplot2)

ggplot(expenditure_demo, aes(x = X1, y = Y)) +
  geom_point() +
  labs(title = "Per capita housing expenditure by per capita personal income in 
       all 50 U.S. states", x = "Per capita personal income (USD)", 
       y = "Per capita expenditure on housing (USD)") +
  geom_smooth(
    aes(x = X1, y = Y),
    method = "lm",
    formula = y ~ x,
    se = FALSE) 

# 2. Plotting housing expenditure Y against financial insecurity X2

# I create a simple linear regression model and calculate the slope, intercept
# and correlation (R-squared):
regression_y_on_x2 <- lm(Y ~ X2, data = expenditure_demo)
lm(formula = Y ~ X2, data = expenditure_demo)
summary(regression_y_on_x2)

# I create a scatterplot of housing expenditure against per capita income and
# include the regression line I created using the lm function:
library(ggplot2)

ggplot(expenditure_demo, aes(x = X2, y = Y)) +
  geom_point() +
  labs(title = "Per capita housing expenditure by number of residents 
       per 100,000 experiencing financial insecurity", x = "Number of residents 
       per 100,000 experiencing financial insecurity", y = "Per capita 
       expenditure on housing (USD)") +
  geom_smooth(
    aes(x = X2, y = Y),
    method = "lm",
    formula = y ~ x,
    color = "red",
    se = FALSE) 

# 3. Plotting housing expenditure Y against urban population X3

# I create a simple linear regression model and calculate the slope, intercept
# and correlation (R-squared):
regression_y_on_x3 <- lm(Y ~ X3, data = expenditure_demo)
lm(formula = Y ~ X3, data = expenditure_demo)
summary(regression_y_on_x3)

# I create a scatterplot of housing expenditure against per capita income and
# include the regression line I created using the lm function:
library(ggplot2)

ggplot(expenditure_demo, aes(x = X3, y = Y)) +
  geom_point() +
  labs(title = "Per capita housing expenditure by number of residents 
       per thousand living in urban areas", x = "Number of residents 
       per thousand living in urban areas", y = "Per capita 
       expenditure on housing (USD)") +
  geom_smooth(
    aes(x = X3, y = Y),
    method = "lm",
    formula = y ~ x,
    color = "purple",
    se = FALSE) 

#####################
# Problem 2, Part 2
#####################

# Generating summary statistics

head(expenditure_demo)
glimpse(expenditure_demo)
summary(expenditure_demo)

northeast_region <- filter(expenditure_demo, Region == "1")
north_central_region <- filter(expenditure_demo, Region == "2")
south_region <- filter(expenditure_demo, Region == "3")
west_region <- filter(expenditure_demo, Region == "4")

# Since region is a categorical variable and housing expenditure is a
# continuous variable, I use boxplots to compare housing expenditure by region.

expenditure_demo$Region <- as.factor(expenditure_demo$Region)

library(dplyr)
library(ggplot2)

# Converting the "1-4" labeling for regions to descriptive names and adding
# the more descriptive variable "Regions" to the data frame:
expenditure_demo <- expenditure_demo %>%
  mutate(Regions = factor(Region, labels = c("Northeast", "North Central", "South", "West")))

#Creating plots:
ggplot(expenditure_demo, aes(x = Regions, y = Y, fill = Regions)) +
  geom_boxplot() +
  labs(title = "Per capita housing expenditure by region", 
       x = "Region", 
       y = "Per capita expenditure on housing (USD)") 

#Calculating the mean housing expenditures for each region:

mean(subset(expenditure_demo, Regions == "Northeast")$Y)
mean(subset(expenditure_demo, Regions == "North Central")$Y)
mean(subset(expenditure_demo, Regions == "South")$Y)
mean(subset(expenditure_demo, Regions == "West")$Y)

# The West has the highest mean housing expenditure per capita.

#####################
# Problem 2, Part 3
#####################

# I create a simple linear regression model and calculate the slope, intercept
# and correlation (R-squared):
regression_y_on_x1 <- lm(Y ~ X1, data = expenditure_demo)
lm(formula = Y ~ X1, data = expenditure_demo)
summary(regression_y_on_x1)

# I create a scatterplot of housing expenditure against per capita income and
# include the regression line I created using the lm function:
library(ggplot2)

ggplot(expenditure_demo, aes(x = X1, y = Y)) +
  geom_point() +
  labs(title = "Per capita housing expenditure by per capita personal income in 
       all 50 U.S. states", x = "Per capita personal income (USD)", 
       y = "Per capita expenditure on housing (USD)") +
  geom_smooth(
    aes(x = X1, y = Y),
    method = "lm",
    formula = y ~ x,
    se = FALSE) 

# I reproduce the above graph using different colors and symbols for 
# different regions:

regression_y_on_x1 <- lm(Y ~ X1, data = expenditure_demo)
lm(formula = Y ~ X1, data = expenditure_demo)
summary(regression_y_on_x1)

# I create a scatterplot of housing expenditure against per capita income and
# include the regression line I created using the lm function:
library(ggplot2)

ggplot(expenditure_demo, aes(x = X1, y = Y, color = Regions)) +
  geom_point(aes(shape = Regions)) +
  labs(title = "Regional per capita housing expenditure by per capita personal income in 
       all 50 U.S. states", x = "Per capita personal income (USD)", 
       y = "Per capita expenditure on housing (USD)") +
  geom_smooth(
    aes(x = X1, y = Y),
    method = "lm",
    formula = y ~ x,
    se = FALSE) 

### END OF PROBLEM SET