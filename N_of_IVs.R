#set.seed(1)
N <- 30

x1 <- rnorm(n=N)
x2 <- rnorm(n=N)
x3 <- rnorm(n=N)
x4 <- rnorm(n=N)
x5 <- rnorm(n=N)
x6 <- rnorm(n=N)
x7 <- rnorm(n=N)
x8 <- rnorm(n=N)
x9 <- rnorm(n=N)

y  <- rnorm(n=N)

IV.matrix <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9)
IV.matrix[1:5,]
round(cor(IV.matrix),2) #intercorrelation between variables in the matrix
round(cor(y,IV.matrix),2) #correlation between y and each of the columns (each variable)

summary(lm(y~x1))$r.squared
summary(lm(y~x1 + x2))$r.squared
summary(lm(y~x1 + x2 + x3))$r.squared
summary(lm(y~x1 + x2 + x3 + x4))$r.squared
summary(lm(y~x1 + x2 + x3 + x4 + x5))$r.squared
summary(lm(y~x1 + x2 + x3 + x4 + x5 + x6))$r.squared
summary(lm(y~x1 + x2 + x3 + x4 + x5 + x6 + x7))$r.squared
summary(lm(y~x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8))$r.squared
summary(lm(y~x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9))$r.squared

#r-squared increases for each variable added. Whether or not the contribution is significant your estimate is biased by the number of independent variables

#This sequence suggests that the randomly created variables account for 53% of the variance in the outcome variable. This is due, in part, to chance assosciations between the variables. In an effort to optimize, the regression equation takes advantage of these chance assoscitions and attempts to explain the variance to the greatest degree possible.

summary(lm(y~x1))$adj.r.squared
summary(lm(y~x1 + x2))$adj.r.squared
summary(lm(y~x1 + x2 + x3))$adj.r.squared
summary(lm(y~x1 + x2 + x3 + x4))$adj.r.squared
summary(lm(y~x1 + x2 + x3 + x4 + x5))$adj.r.squared
summary(lm(y~x1 + x2 + x3 + x4 + x5 + x6))$adj.r.squared
summary(lm(y~x1 + x2 + x3 + x4 + x5 + x6 + x7))$adj.r.squared
summary(lm(y~x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8))$adj.r.squared
summary(lm(y~x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9))$adj.r.squared

#The adjusted r-squared on the other hand takes into account whether the added variable actually meaningfully accounts for the variance in the y variable.

#This is not usually a problem when you have a large sample. Remember the equation and how it, in part, relies on the degrees of freedom AND the n of your whole sample.

# What is the "truth"? Increase N and find out

########## What if the variables are genuinely correlated? ######


common <- rnorm(n=N,sd=0.5)

x1 <- rnorm(n=N) + common
x2 <- rnorm(n=N) + common
x3 <- rnorm(n=N) + common
x4 <- rnorm(n=N) + common
x5 <- rnorm(n=N) + common
x6 <- rnorm(n=N) + common
x7 <- rnorm(n=N) + common
x8 <- rnorm(n=N) + common
x9 <- rnorm(n=N) + common

y  <- rnorm(n=N) + common

IV.matrix <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9)
round(cor(IV.matrix),2)
round(cor(y,IV.matrix),2)

summary(lm(y~x1))$r.squared
summary(lm(y~x1 + x2))$r.squared
summary(lm(y~x1 + x2 + x3))$r.squared
summary(lm(y~x1 + x2 + x3 + x4))$r.squared
summary(lm(y~x1 + x2 + x3 + x4 + x5))$r.squared
summary(lm(y~x1 + x2 + x3 + x4 + x5 + x6))$r.squared
summary(lm(y~x1 + x2 + x3 + x4 + x5 + x6 + x7))$r.squared
summary(lm(y~x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8))$r.squared
summary(lm(y~x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9))$r.squared


summary(lm(y~x1))$adj.r.squared
summary(lm(y~x1 + x2))$adj.r.squared
summary(lm(y~x1 + x2 + x3))$adj.r.squared
summary(lm(y~x1 + x2 + x3 + x4))$adj.r.squared
summary(lm(y~x1 + x2 + x3 + x4 + x5))$adj.r.squared
summary(lm(y~x1 + x2 + x3 + x4 + x5 + x6))$adj.r.squared
summary(lm(y~x1 + x2 + x3 + x4 + x5 + x6 + x7))$adj.r.squared
summary(lm(y~x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8))$adj.r.squared
summary(lm(y~x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9))$adj.r.squared

# What is the "truth"? Increase N and find out

