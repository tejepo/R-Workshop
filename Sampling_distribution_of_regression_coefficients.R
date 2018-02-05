rm(list = ls())

# setting up a population in which gre.q is positively related to gpa
population.N <- 10000
gpa <- rnorm(n=population.N,mean=3,sd=0.25)
hist(gpa, breaks=20)
gre.q <- 150 + 10*scale(gpa) + rnorm(n=population.N,mean=0,sd=10)
hist(gre.q, breaks=20)
plot(x=gpa, y=gre.q)

# confirming that gre.q is indeed positively related to gpa using lm()
lm.gre.gpa <- lm(gre.q ~ gpa)
intercept  <- lm.gre.gpa$coef[1]
slope      <- lm.gre.gpa$coef[2]
cat("gre.q =",round(intercept,2), "+", round(slope,2),"* gpa\n")

# plotting the regression line using the intercept and slope from the above
lines(x=c(min(gpa),max(gpa)),
      y=c(intercept+slope*min(gpa), intercept+slope*max(gpa)),
      col=2,lw=3)

# defining a new function called onestudy
onestudy <- function(x, y, sample.size, color, show.lines=F, show.points=F) {
  participants <- sample(length(x),size=sample.size)
  x <- x[participants]
  y <- y[participants]
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

# let's run onestudy() and see the result
onestudy.result <- onestudy(x=gpa, y=gre.q, sample.size=10, color=3, show.lines=T, show.points=T)
cat("gre.q =",round(onestudy.result$coef[1],2),
    "+", round(onestudy.result$coef[2],2),"* gpa\n")

# let's do that again
onestudy.result <- onestudy(x=gpa, y=gre.q, sample.size=10, color=5, show.lines=T, show.points=T)
cat("gre.q =",round(onestudy.result$coef[1],2),
    "+", round(onestudy.result$coef[2],2),"* gpa\n")

# let's do that 10 times
# move cursor to the console window and press return
plot(x=gpa, y=gre.q, type='n')
for (i in 1:10) {
  readline(prompt=paste("Round",i,"of 10: Press return when ready:"))
  onestudy(x=gpa, y=gre.q, sample.size=10,color=i,show.lines=T, show.points=T)
}

# let's do that 100 times
plot(x=gpa, y=gre.q, type='n')
for (i in 1:100) {
  cat("Study",i,"\n")
  onestudy(x=gpa, y=gre.q, sample.size=10,color=i,show.lines=T)
}

# let's do that 5000 times, and let's keep track of the intercept and slope from each study
sample.intercepts <- NULL
sample.slopes     <- NULL
plot(x=gpa, y=gre.q, type='n')
for (i in 1:5000) {
  cat("Study",i,"\n")
  onestudy.result <- onestudy(x=gpa, y=gre.q, sample.size=10, color=i, show.lines=T, show.points=F)
  sample.intercepts <- append(sample.intercepts, onestudy.result$coef[1])
  sample.slopes     <- append(sample.slopes, onestudy.result$coef[2])
}

# First, let's remind us of the population intercept and slope
lm.gre.gpa <- lm(gre.q ~ gpa)
intercept  <- lm.gre.gpa$coef[1]
slope      <- lm.gre.gpa$coef[2]
cat("gre.q =",round(intercept,2), "+", round(slope,2),"* gpa\n")

# Now, let's see the sampling distribution of intercepts across the 5000 studies
hist(sample.intercepts, breaks=20)
cat("mean(sample.intercepts) =",mean(sample.intercepts),"\n")
# let's see the sampling distribution of slopes across the 5000 studies
hist(sample.slopes, breaks=20)
cat("mean(sample.slopes) =",mean(sample.slopes),"\n")

#p = probability of getting false positive values given that the null hypothesis is true.
  #one way to find the p value for this given sampling of 5000 is to first determine how many points fall above a given point on this line. Let's say for example our sample has an intercept of 80...

length(sample.slopes[sample.slopes > 80])
# then we take that and divide it by the total number (in this case, 5000)

length(sample.slopes[sample.slopes > 60])/5000 #one tailed

(length(sample.slopes[sample.slopes > 60]) + length(sample.slopes[sample.slopes < - 5]))/5000 #two tailed

# So you can see in the long run, the average of the many, many studies will
# converge on the population value.
# But the result of any given study can be very different from the population value.
# The "standard error" is the standard deviation of the variation of the results
# from one study to another.
cat("The standard error (SE) of the intercepts =",sd(sample.intercepts),"\n")
cat("The standard error (SE) of the slopes =",sd(sample.slopes),"\n")



#-------- NULL HYPOTHESIS TESTING ------------
# Setting up the Null population
gpa <- rnorm(n=population.N,mean=3,sd=0.25)
hist(gpa, breaks=20)
# gre.q <- 150 + 10*scale(gpa) + rnorm(n=population.N,mean=0,sd=10)
gre.q <- 150 + rnorm(n=population.N,mean=0,sd=10)
hist(gre.q, breaks=20)
plot(x=gpa, y=gre.q)

lm.gre.gpa <- lm(gre.q ~ gpa)
intercept  <- lm.gre.gpa$coef[1]
slope      <- lm.gre.gpa$coef[2]
cat("gre.q =",round(intercept,2), "+", round(slope,2),"* gpa\n")

sample.intercepts <- NULL
sample.slopes     <- NULL

for (i in 1:5000) {
  cat("Study",i,"\n")
  onestudy.result <- onestudy(x=gpa, y=gre.q, sample.size=10, color=i, show.lines=F, show.points=F)
  sample.intercepts <- append(sample.intercepts, onestudy.result$coef[1])
  sample.slopes     <- append(sample.slopes, onestudy.result$coef[2])
}

# Now, let's see the sampling distribution of intercepts
hist(sample.intercepts,breaks=50)
# let's see the sampling distribution of slopes
hist(sample.slopes, breaks=50)


# QUESTION: Assuming the population looked like the one we set up above,
# for which the null hypothesis is true, what is the likelihood of obtaining
# a slope that is as steep as the one you found in your study?
#
# ANSWER: Look at the histogram of slopes, and see in how many studies (out of 5000)
# a slope of a given value or greater was obtained.
# Divide that by 5000. That's your p value.
# For a two-tailed test, count the number of studies which produced a slope that is
# the same or steeper than the one you obtained in your study, regardless of sign.
#
# QUESTION: To set up the population above, we also needed to set the amount of
# "noise". How do you choose that value?
#
# ANSWER: Assume it's the same as the one you see in your sample.
# 
# You could really compute p values in the way above. But you don't have to.
# Because in the early 20th century, statisticians (who didn't have computers)
# worked out the math that made it possible to estimate the p value.
#
# The lm() function uses the formula they created.
# Let's see what lm() says about a particular study:


participants <- sample(population.N,size=10)
onestudy.result <- lm(gre.q[participants] ~ gpa[participants])
summary(onestudy.result)

#Now, let's look at how our confidence intervals of a slope estimate from a single study as it compares to our distribution of slopes across 5000 studies. These should approximately map on to the 2.5th and 97.5th percentiles.

confint(onestudy.result)

# In a given strudy, the confidence interval may or may not contain the true value.
# When we say with "95%" confidence, what do we mean?
# Let's demonstrate below.

CI.contains.true.slope <- NULL
TrueSlope <- lm(gre.q ~ gpa)$coef[2]
plot(x=c(-200,200),y=c(0,5000),type='n')

for (i in 1:5000) {
  cat("Study",i,": ")
  onestudy.result <- onestudy(x=gpa, y=gre.q, sample.size=10, color=i, show.lines=F, show.points=F)
  sample.lowerlimit <- confint(onestudy.result)["x","2.5 %"]
  sample.upperlimit <- confint(onestudy.result)["x","97.5 %"]
  lines(x=c(sample.lowerlimit,sample.upperlimit),y=c(i,i),col=i)
  cat("lower=",  round(sample.lowerlimit,2), 
      ", true=", round(TrueSlope,2), 
      ", upper=",round(sample.upperlimit,2),sep="")
 
  if ((sample.lowerlimit < TrueSlope) && (TrueSlope < sample.upperlimit)) {
    cat(", CI contains true? yes\n")
    CI.contains.true.slope <- append(CI.contains.true.slope,1)
  } else {
    cat(", CI contains true? no\n")
    CI.contains.true.slope <- append(CI.contains.true.slope,0)
  }
}

# How many times did the CI of the slope contain the true slope?
hist(CI.contains.true.slope)
mean(CI.contains.true.slope)


#--------- modify the above code a little bit to determine statistical power ------



