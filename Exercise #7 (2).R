# Assignment 7
# Terrence Pope

# (1) Set true.slope to 20 to make gre.q positively related to gpa.

population.N <- 100000 
sample.size.in.each.simulated.study <- 20
true.slope <- 20 
gpa <- rnorm(n=population.N,mean=3,sd=0.25)
gre.q <- 150 + true.slope*gpa + rnorm(n=population.N,mean=0,sd=10)

# (2) run the whole code and describe the sampling distribution of correlation.

onestudy <- function(x, y, sampling.distribution.of.size, color, show.lines=F, show.points=F) {
  participants <- sample(length(x),size=sampling.distribution.of.size)
  x <- scale(x[participants])
  y <- scale(y[participants])
  if (show.points) points(x,y, col=color)
  
  lm.result   <- lm(y ~ x)
  intercept   <- lm.result$coef[1]
  slope       <- lm.result$coef[2]
  
  if (show.lines) {
    lines(x=c(min(x),max(x)),
          y=c(intercept+slope*min(x), intercept+slope*max(x)),
          col=color)
  }
  return(lm.result)
}

sampling.distribution.of.r    <- NULL
sampling.distribution.of.p    <- NULL
plot(x=gpa, y=gre.q, type='n',main="Sampling distribution of r",xlim=c(-2,2),ylim=c(-2,2))
for (i in 1:5000) {
  cat("Study",i,"\n")
  onestudy.result <- onestudy(x=gpa, y=gre.q, sampling.distribution.of.size=sample.size.in.each.simulated.study,
                                  color=i, show.lines=T, show.points=F)
  r <- summary(onestudy.result)$coef[2,1]
  p <- summary(onestudy.result)$coef[2,4]
  sampling.distribution.of.r    <- append(sampling.distribution.of.r, r)
  sampling.distribution.of.p    <- append(sampling.distribution.of.p, p)
}

hist(sampling.distribution.of.r, breaks=20)

# ANSWER TO QUESTION 2 ## The sampling distribution of r is negatively skewed

# (3) Roughly what is the probability of obtaining a correlation equal to or greater than 0.3 in this simulated study?

length(sampling.distribution.of.r[sampling.distribution.of.r > .3])
# then we take that and divide it by the total number (in this case, 5000)

length(sampling.distribution.of.r[sampling.distribution.of.r > .3])/5000 #one tailed

(length(sampling.distribution.of.r[sampling.distribution.of.r > .3]) + length(sampling.distribution.of.r[sampling.distribution.of.r < - .3]))/5000 #two tailed

# (4) What is the meaning of this probability? Make sure to refer to the slope in the population.

# ANSWER TO QUESTION 4 ## This probability is know as a p value? It is the likelihood of drawing a sample with this slope from the population given that the null hypothesis is true. Which in this case is pretty high. the p is ~.8 which basically suggests that a slope of .3 would very likely be drawn from a sample of the null population.

# (5) After each simulated study, the R code stores the estimated p-value reported in the study. After all 5000 studies are run, it produces a histogram of 5000 p-values. What proportion of these 5000 p-values are below 0.05? What is this called?

hist(sampling.distribution.of.p, breaks=20)

length(sampling.distribution.of.p[sampling.distribution.of.p < .050001])

length(sampling.distribution.of.p[sampling.distribution.of.p < .050001])/5000

# ANSWER TO QUESTION 5 ## The proportion of the p values beneath p=.05 is .5368. This number represents the power of the study. The likelihood of accepting the null hypothesis when it's true

# (6) Change the sample size to 50. Did that change the proportion of p-values below 0.05? What is the take-home point of this?

population.N <- 100000 
sample.size.in.each.simulated.study <- 50
true.slope <- 20 
gpa <- rnorm(n=population.N,mean=3,sd=0.25)
gre.q <- 150 + true.slope*gpa + rnorm(n=population.N,mean=0,sd=10)

onestudy <- function(x, y, sampling.distribution.of.size, color, show.lines=F, show.points=F) {
  participants <- sample(length(x),size=sampling.distribution.of.size)
  x <- scale(x[participants])
  y <- scale(y[participants])
  if (show.points) points(x,y, col=color)
  
  lm.result   <- lm(y ~ x)
  intercept   <- lm.result$coef[1]
  slope       <- lm.result$coef[2]
  
  if (show.lines) {
    lines(x=c(min(x),max(x)),
          y=c(intercept+slope*min(x), intercept+slope*max(x)),
          col=color)
  }
  return(lm.result)
}

sampling.distribution.of.r    <- NULL
sampling.distribution.of.p    <- NULL
plot(x=gpa, y=gre.q, type='n',main="Sampling distribution of r",xlim=c(-2,2),ylim=c(-2,2))
for (i in 1:5000) {
  cat("Study",i,"\n")
  onestudy.result <- onestudy(x=gpa, y=gre.q, sampling.distribution.of.size=sample.size.in.each.simulated.study,
                              color=i, show.lines=T, show.points=F)
  r <- summary(onestudy.result)$coef[2,1]
  p <- summary(onestudy.result)$coef[2,4]
  sampling.distribution.of.r    <- append(sampling.distribution.of.r, r)
  sampling.distribution.of.p    <- append(sampling.distribution.of.p, p)
}

hist(sampling.distribution.of.p, breaks=20)

length(sampling.distribution.of.p[sampling.distribution.of.p < .050001])

length(sampling.distribution.of.p[sampling.distribution.of.p < .050001])/5000

# ANSWER TO QUESTION 6 ## The proportion of the p values beneath p=.05 is .919. Since this is power you're just saying that the rasing the n increases the likelihood that you find the result when it's there.

# (7) Set true.slope to 0 to make gre.q uncorrelated to gpa.

population.N <- 100000 
sample.size.in.each.simulated.study <- 50
true.slope <- 0
gpa <- rnorm(n=population.N,mean=3,sd=0.25)
gre.q <- 150 + true.slope*gpa + rnorm(n=population.N,mean=0,sd=10)

# ANSWER TO QUESTION 6 ## The proportion of the p values beneath p=.05 is .919. Since this is power you're just saying that the rasing the n increases the likelihood that you find the result when it's there.

# (8) Repeat 2-6 above and observe what the histograms of estimated p-values now look like, and how this changes (or not) as a function of the sample size in the simulated studies.

# (2) run the whole code and describe the sampling distribution of correlation.

onestudy <- function(x, y, sampling.distribution.of.size, color, show.lines=F, show.points=F) {
  participants <- sample(length(x),size=sampling.distribution.of.size)
  x <- scale(x[participants])
  y <- scale(y[participants])
  if (show.points) points(x,y, col=color)
  
  lm.result   <- lm(y ~ x)
  intercept   <- lm.result$coef[1]
  slope       <- lm.result$coef[2]
  
  if (show.lines) {
    lines(x=c(min(x),max(x)),
          y=c(intercept+slope*min(x), intercept+slope*max(x)),
          col=color)
  }
  return(lm.result)
}

sampling.distribution.of.r    <- NULL
sampling.distribution.of.p    <- NULL
plot(x=gpa, y=gre.q, type='n',main="Sampling distribution of r",xlim=c(-2,2),ylim=c(-2,2))
for (i in 1:5000) {
  cat("Study",i,"\n")
  onestudy.result <- onestudy(x=gpa, y=gre.q, sampling.distribution.of.size=sample.size.in.each.simulated.study,
                              color=i, show.lines=T, show.points=F)
  r <- summary(onestudy.result)$coef[2,1]
  p <- summary(onestudy.result)$coef[2,4]
  sampling.distribution.of.r    <- append(sampling.distribution.of.r, r)
  sampling.distribution.of.p    <- append(sampling.distribution.of.p, p)
}

hist(sampling.distribution.of.r, breaks=20)

# ANSWER TO QUESTION 2 ## The sampling distribution of r is normal

# (3) Roughly what is the probability of obtaining a correlation equal to or greater than 0.3 in this simulated study?

length(sampling.distribution.of.r[sampling.distribution.of.r > .3])
# then we take that and divide it by the total number (in this case, 5000)

length(sampling.distribution.of.r[sampling.distribution.of.r > .3])/5000 #one tailed

(length(sampling.distribution.of.r[sampling.distribution.of.r > .3]) + length(sampling.distribution.of.r[sampling.distribution.of.r < - .3]))/5000 #two tailed

# (5) After each simulated study, the R code stores the estimated p-value reported in the study. After all 5000 studies are run, it produces a histogram of 5000 p-values. What proportion of these 5000 p-values are below 0.05? What is this called?

hist(sampling.distribution.of.p, breaks=20)

length(sampling.distribution.of.p[sampling.distribution.of.p < .050001])

length(sampling.distribution.of.p[sampling.distribution.of.p < .050001])/5000

# ANSWER TO QUESTION 5 ## The proportion of the p values beneath p=.05 is .0492. This number representis an accurate reflection of alpha. When there is no effect you'll find significance about 5% of the time.

# (6) Change the sample size to 50. Did that change the proportion of p-values below 0.05? What is the take-home point of this?

population.N <- 100000 
sample.size.in.each.simulated.study <- 50
true.slope <- 0 
gpa <- rnorm(n=population.N,mean=3,sd=0.25)
gre.q <- 150 + true.slope*gpa + rnorm(n=population.N,mean=0,sd=10)

onestudy <- function(x, y, sampling.distribution.of.size, color, show.lines=F, show.points=F) {
  participants <- sample(length(x),size=sampling.distribution.of.size)
  x <- scale(x[participants])
  y <- scale(y[participants])
  if (show.points) points(x,y, col=color)
  
  lm.result   <- lm(y ~ x)
  intercept   <- lm.result$coef[1]
  slope       <- lm.result$coef[2]
  
  if (show.lines) {
    lines(x=c(min(x),max(x)),
          y=c(intercept+slope*min(x), intercept+slope*max(x)),
          col=color)
  }
  return(lm.result)
}

sampling.distribution.of.r    <- NULL
sampling.distribution.of.p    <- NULL
plot(x=gpa, y=gre.q, type='n',main="Sampling distribution of r",xlim=c(-2,2),ylim=c(-2,2))
for (i in 1:5000) {
  cat("Study",i,"\n")
  onestudy.result <- onestudy(x=gpa, y=gre.q, sampling.distribution.of.size=sample.size.in.each.simulated.study,
                              color=i, show.lines=T, show.points=F)
  r <- summary(onestudy.result)$coef[2,1]
  p <- summary(onestudy.result)$coef[2,4]
  sampling.distribution.of.r    <- append(sampling.distribution.of.r, r)
  sampling.distribution.of.p    <- append(sampling.distribution.of.p, p)
}

hist(sampling.distribution.of.p, breaks=20)

length(sampling.distribution.of.p[sampling.distribution.of.p < .050001])

length(sampling.distribution.of.p[sampling.distribution.of.p < .050001])/5000

# No change in p distribution

# (9) What is the overall take-home points of this exercise?

#This demonstrates the concept of power and how it relates to the p curve when there is or isn't an effect. When there is an effect then the pcurve will show a greater likelihood of finding an effect when the sample size is larger (aka the study is powered up). When the null is true the likelihood of making a type one error is .05 and this doesn't change regardless of how large your sample size is. 