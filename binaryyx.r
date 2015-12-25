# BinaryYX.R
# Author      : Jacob Beauchamp
# Date        : 12/24/2015
# Description : This is a program used to predict the binary value of a 
#               dependent variable, y, given the binary value of an
#               independent variable, x. The prediction is based on the
#               probability of y given x as determined by training examples.
# ---------------------------------------------------------------------------

# A BinaryNum is one of
# - 1
# - 0

# A Binary.Y.X is 
# data.frame(c(ListOf BinaryNum),c(ListOf BinaryNum))


### Test Cases ###

# Expected P(y=1|x=1) = .5
y1 <- c(1,1)
x1 <- c(1,0)
df1 <- data.frame(y1,x1)

# Expected P(y=1|x=1) = .75
y2 <- c(1,1,1,1)
x2 <- c(1,0,1,1)
df2 <- data.frame(y2,x2)

# Expected P(y=1|x=1) = .111
y3 <- c(0,0,0,1,1,1,0,0,0)
x3 <- c(1,0,1,0,1,0,1,0,1)
df3 <- data.frame(y3,x3)

# Expected P(y=1|x=1) = .9
y4 <- c(1,1,1,1,0,1,1,1,1,1)
x4 <- c(1,1,1,1,1,1,1,1,1,1)
df4 <- data.frame(y4,x4)


# ---------------------------------------------------------------------------
# prob.y.equals.one : Binary.Y.X
# Returns the probability that y equals 1 given examples of y given x
prob.y.equals.one <- function(data) {
  
  n <- nrow(data)
  
  # Find the number of cases where y=1 and x=1
  num.instances <- length(which(data[,1] == 1 & data[,2] == 1))
  
  num.instances / n
  
}
# ---------------------------------------------------------------------------


# Expected P(y=1|x=1) = .5
prob.y.equals.one(df1)
# Expected P(y=1|x=1) = .75
prob.y.equals.one(df2)
# Expected P(y=1|x=1) = .111
prob.y.equals.one(df3)
# Expected P(y=1|x=1) = .9
prob.y.equals.one(df4)

