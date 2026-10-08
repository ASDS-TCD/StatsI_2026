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

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#1.1. Find a 90% confidence interval for the average student IQ in the school  
sample_mean <- mean(y) #the mean is 98.44
sample_sd <- sd(y) #the standard deviation is 13.09287
sample_size <- length(y) #the length is 25

conf_level <- 0.90
alpha <- 1 - conf_level #confidence interval: 1 - 0.90 = 0.10
df <- sample_size - 1 #the degrees of freedom is 24

t_critical <- qt(1 - alpha / 2, df) # The sample size is 25 and the population standard deviation is unknown,so a t-test is appropriate.
margin_of_error <- t_critical * sample_sd / sqrt(sample_size)

confidence_interval <- c(
  lower = sample_mean - margin_of_error,
  upper = sample_mean + margin_of_error
)

confidence_interval
t.test(y, conf.level = 0.90) # Check using R's built-in function
# The 90% confidence interval is 93.96 to 102.92, thus we are 90% confident that the population mean IQ of students at the school is between the lower and upper limits of the confidence interval.

#1.2. The counselor wants to know if the school’s average IQ is above 100.
# IQ needs to be greater, thus alternative needs to be greater.
iq_test <- t.test(y, mu = 100, alternative = "greater", conf.level = 0.95)
iq_test
ifelse(iq_test$p.value < 0.05,
       "We reject H0",
       "We fail to reject H0")
#Decision: If the p-value < 0.05, reject H0; otherwise, fail to reject H0. The p-value = 0.7215 thus at the 5% significance level, there is insufficient evidence to conclude that the average student IQ is greater than 100. Fail to reject H0

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)
head(expenditure)
summary(expenditure)
names(expenditure)

#2.1. # Plot relationships and correlations among Y, X1, X2, and X3
pairs(
  expenditure[, c("Y", "X1", "X2", "X3")]
)

expenditure_cor <- cor(expenditure[, c("Y", "X1", "X2", "X3")])
expenditure_cor

expenditure_cor_no_diag <- expenditure_cor
diag(expenditure_cor_no_diag) <- NA
expenditure_cor_no_diag

expenditure_relationship <- matrix(
  ifelse(
    expenditure_cor_no_diag >= 0.7, "Strong Positive",
    ifelse(
      expenditure_cor_no_diag >= 0.3, "Moderate Positive",
      ifelse(
        expenditure_cor_no_diag > 0, "Weak Positive",
        ifelse(
          expenditure_cor_no_diag <= -0.7, "Strong Negative",
          ifelse(
            expenditure_cor_no_diag <= -0.3, "Moderate Negative",
            ifelse(
              expenditure_cor_no_diag < 0, "Weak Negative",
              "No Relationship"
            )
          )
        )
      )
    )
  ),
  nrow = nrow(expenditure_cor_no_diag),
  dimnames = dimnames(expenditure_cor_no_diag)
)
# (Statistics Resources: Correlation, 2026) 
expenditure_relationship
# The scatterplot matrix should show upward-sloping patterns between all pairs of variables, consistent with their positive correlations.
# Statistics Resources: Correlation. (2026, Sep 29). Retrieved from National University: https://resources.nu.edu/statsresources/correlation



#2.2. Which region has the highest per capita expenditure on housing assistance?
expenditure$RegionName <- factor(
  expenditure$Region,
  levels = c(1, 2, 3, 4),
  labels = c("Northeast", "North Central", "South", "West")
)

boxplot(
  Y ~ RegionName,
  data = expenditure,
  xlab = "Region",
  ylab = "Per Capita Expenditure on Housing Assistance",
  main = "Per Capita Expenditure by Region"
)

regional_means <- aggregate(Y ~ RegionName, data = expenditure, mean)
regional_means
regional_means[which.max(regional_means$Y), ]
# The West (Region 4) has the highest average expenditure, with a average mean of 88.31 per capita. The North Central region (Region 2) has the second-highest average at 83.92, followed by the Northeast (Region 1) at 79.44. The South (Region 3) has the lowest average expenditure at 69.19 per capita.


#2.3.1.  Plot the relationship between Y and X1?
plot(expenditure$X1, expenditure$Y,
     xlab = "Per Capita Personal Income (X1)",
     ylab = "Per Capita Expenditure on Housing Assistance (Y)",
     main = "Relationship Between Y and X1",
     pch = 19,
     col = "steelblue")

abline(lm(Y ~ X1, data = expenditure),
       col = "red",
       lwd = 2)

model <- lm(Y ~ X1, data = expenditure)
summary(model)$r.squared
coef(model)

cor_y_x1 <- cor(expenditure$Y, expenditure$X1)
round(cor_y_x1, 3)
# The scatterplot shows a moderate positive relationship between X1 and Y:as per-capita personal income (X1) increases, per-capita expenditure on housing assistance (Y) generally increases. The correlation between the two variables is 0.532, indicating a moderate positive linear association. 
# The abline's formula is Y = 32.546 + 0.02458X_1. Approximately 28.27% of the variation in Y is explained by X1 using this linear regression model.

plot(expenditure$X1, expenditure$Y,
     xlab = "Per Capita Personal Income (X1)",
     ylab = "Per Capita Expenditure on Housing Assistance (Y)",
     main = "Relationship Between Y and X1 by Region",
     pch = c(16, 17, 15, 18)[expenditure$Region],
     col = c("red", "blue", "green", "purple")[expenditure$Region])

legend("topright",
       inset = c(0.725, 0),
       legend = c("Northeast", "North Central", "South", "West"),
       pch = c(16, 17, 15, 18),
       col = c("red", "blue", "green", "purple"),
       title = "Region",
       cex = 0.5,
       xpd = TRUE)

par(xpd = NA)

