# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()

#############
# Basic stats
#############

# create vector y
y <- c(0, 4, 4, 5, 7, 10)

# (1) find sum of y using the built-in R function
print(sum(y))

# (2) find mean of y using your "own" function
# now do the same thing, but faster using the built-in R function
find_mean <- function(l) {
  sum <- 0
  for(i in seq_along(l)){
    sum <- sum + l[i]
  }
  sum / length(l)
}
print(find_mean(y))

print(mean)

# (3) find sum of demeaned values
demean_values <- function(l, mean){
  ans <- NULL
  for(i in seq_along(l)){
    ans[i] <- l[i] - mean
  }
  ans
}

demeaned_y <- demean_values(y, find_mean(y))
print(demeaned_y)

# built-in way (R is so weird)
print(y - mean(y))

# (4) calculate sum of squared error

# this is ridiculous. list * list..?
squared_errors <- demeaned_y * demeaned_y
sum_of_squared_errors <- sum(squared_errors)
print(sum_of_squared_errors)

###########
# Quantiles
###########

# create vector
quantilesVec <- c(55, 84, 65, 54, 61, 67, 80, 59, 81, 82)

# (1) calculate median
quantilesVec_median <- median(quantilesVec)
print(quantilesVec_median)

# (2) calculate quantiles
quantile(quantilesVec)

# (3) make a histogram of state median income
state.x77[,2]
setwd("tutorials/Week 1")
pdf("state_median_income_histogram.pdf")
hist(state.x77[,2])
dev.off()
setwd("../../")

# remember to save your plot as a pdf