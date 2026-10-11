###############################################################################
# Title:        Applied Statistics 1 - Problem Set 2
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
lapply(c("stringr"),  pkgTest)
lapply(c(),  pkgTest)
lapply(c("readr", "ggplot2", "dplyr", "viridis", "foreign"),  pkgTest)
install.packages("tidyverse")
library(tidyverse)
search()

#####################
# Problem 1, Part A
#####################

# The expected frequency for each cell is equal to 
# ((row total)/(grand total)) * (column total):
    # fe(upper class, not stopped) = 
        fe_upper_nostop <- (27/42)*21 
        # 13.5
    # fe(upper class, bribe requested) = 
        fe_upper_bribe <- (27/42)*13 
        # 8.357
    # fe(upper class, stopped) =
        fe_upper_stop <- (27/42)*8 
        # 5.143
    # fe(lower class, not stopped) =
        fe_lower_nostop <- (15/42)*21
        # 7.5
    # fe(lower class, bribe requested) =
        fe_lower_bribe <- (15/42)*13
        # 4.643
    # fe(lower class, stopped) =
        fe_lower_stop <- (15/42)*8
        # 2.857

# The X^2 statistic is equal to the sum of the squares of the differences 
# between the observed frequencies and the expected frequencies over the
# expected frequencies. 
# i.e., sum[(fo - fe)^2 / fe]

chi_square <- (((14-13.5)^2) / 13.5) + 
              (((6-8.357)^2) / 8.357) + 
              (((7-5.143)^2) / 5.143) + 
              (((7-7.5)^2) / 7.5) + 
              (((7-4.643)^2) / 4.643) +
              (((1-2.857)^2) / 2.857)
chi_square # 3.79067

#####################
# Problem 1, Part B
#####################

# df = (rows-1)(columns - 1) = 1*2 = 2
# X^2 = 3.79067

pchisq(chi_square, df = 2, lower.tail = FALSE) # 0.150268

# Our hypotheses are:

    # Ho: The class of the driver and whether police officers solicit a bribe
    # from the driver are statistically independent. 

    # Ha: The class of the driver and whether police officers solicit a bribe
    # from the driver are statistically dependent. 

# Our P-value of approximately 0.15 (above) is greater than the stated alpha-
# value of 0.1, so at the alpha = 0.1 level, we fail to reject the null 
# hypothesis that the class of the driver and whether police officers solicit a 
# bribe from the driver are statistically independent. 

# Checking my work:
driver_data <- as.table(rbind(c(14,6,7), c(7,7,1)))
dimnames(driver_data) <- list(socialclass = c("upper_class", "lower_class"),
                              bribe = c("not_stopped", "bribe_requested", 
                                        "stopped")) 
driver_data
chisq.test(driver_data)
# The chi-square test function in R returns:
    # X-squared = 3.7912
    # df = 2
    # p-value = 0.1502
# These values are equal or approximately equal to the values I calculated by 
# hand. Any discrepancies can likely be attributed to rounding.

#####################
# Problem 1, Part C
#####################

# The standardized residual for each cell is equal to (the difference in the
# observed and expected frequency) / (the square root of the product of the
# expected frequency, (1-the row proportion), and (1-the column proportion))
# i.e. z = (fo-fe) / sqrt(fe(1-row proportion)(1-column proportion))

# z(upper class, not stopped) = 
(14 - 13.5) / sqrt(13.5 * (1 - (27 / 42)) * (1 - (21 / 42))) # 0.3220306

# z(upper class, bribe requested) = 
(6 - 8.357) / sqrt(8.357 * (1 - (27 / 42)) * (1 - (13 / 42))) # -1.641871

# z(upper class, stopped) =
(7 - 5.143) / sqrt(5.143 * (1 - (27 / 42)) * (1 - (8 / 42))) # 1.522888

# z(lower class, not stopped) = 
(7 - 7.5) / sqrt(7.5 * (1 - (15 / 42)) * (1 - (21 / 42))) # -0.3220306

# z(lower class, bribe requested) =
(7 - 4.643) / sqrt(4.643 * (1 - (15 / 42)) * (1 - (13 / 42))) # 1.641832

# z(lower class, stopped) =
(1 - 2.857) / sqrt(2.857 * (1 - (15 / 42)) * (1 - (8 / 42))) #-1.522947

# Checking my work:
chi <- chisq.test(driver_data)
chi$stdres
# These values are equal or approximately equal to the values I calculated by 
# hand. Any discrepancies can likely be attributed to rounding.

#####################
# Problem 1, Part D
#####################

# The standardized residuals measure the strength of each cells's contribution
# to the chi-square statistic (calculated earlier). Just as z-scores provide an
# indication of how far from the mean a given observed value of a continuous
# variable is, standardized residuals provide an indication of how far a
# categorical variable's observed frequency is from its expected frequency.
# If the chi-square test provided statistically significant evidence to reject
# the null hypothesis that the variables are statistically independent (it did
# not provide any such evidence in this case), the standardized residuals would
# indicate which cells in the contingency table (i.e., which class of driver
# and whether the officer requested a bribe) were driving the two variables'
# statistically dependent relationship. 

#####################
# Problem 2, Part A
#####################

# Loading the data from the URL in the assignment sheet: 
df_not_tidy <- read.csv('https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv')

# As the question is specifically concerned with estimating the effect of the 
# reservation policy on the number of new or repaired drinking water facilities 
# in the villages, the two relevant variables for the bivariate regression are
# "reserved" (binary explanatory variable) and "water" (continuous response 
# variable).

# Hypotheses:

  # Ho: beta1 (slope of regression line) = 0. On average, after adopting the 
  # reservation policy (i.e. going from "reserved = 0" to "reserved = 1"), 
  # there is no effect on the number of new or repaired drinking water 
  # facilities in a village. 

  # Ha: beta1 (slope of regression line) ≠ 0. On average, after adopting the 
  # reservation policy (i.e. going from "reserved = 0" to "reserved = 1"), 
  # there is an effect on the number of new or repaired drinking water 
  # facilities in a village. 

#####################
# Problem 2, Part B
#####################

# First, I use subsetting to clean up the data set to only include the 
# variables "reserved" (explanatory variable) and "water" (response variable).
    # *** In the interest of academic integrity, I used Google (no AI) to find
    # the "select" argument for the subset() function. 
df_tidy <- subset(df_not_tidy, select = c("reserved", "water"))

# Then, I call the "lm" function to regress the response variable ("water") on
# the explanatory variable ("reserved").
lm_model <- lm(df_tidy$water ~ df_tidy$reserved)
summary(lm_model)

# From the output generated above:
  # The slope coefficient estimate beta-hat1 is 9.252. 
  # The standard error is 3.948.
  # The test statistic is 2.344.
  # The P-value is 0.0197.

#####################
# Problem 2, Part C
#####################

# The slope coefficient estimate beta-hat1 = 9.252 can be interpreted as
# meaning that by this model, on average, a village adopting the reservation
# policy (that is, going from "reserved = 0" to "reserved = 1"), would be
# expected to build or repair approximately 9 drinking water facilities. 

# The P-value of 0.0197 is less than alpha = 0.05. At the alpha = 0.05 level,
# we can reject the null hypothesis that after adopting the reservation policy 
# (i.e. going from "reserved = 0" to "reserved = 1"), there is no effect on the 
# number of new or repaired drinking water facilities in a village. The slope
# of the linear regression line is statistically different from zero.

##################### END OF PROBLEM SET #####################