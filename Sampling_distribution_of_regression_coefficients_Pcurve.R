# setting up a population in which gre.q is positively related to gpa
population.N <- 100000 # note I increased this to 100,000 so the population
                       # more closely approximates the true 'null' population
sample.size.in.each.simulated.study <- 20
true.slope <- 20 #changing this means that you're no longer looking at a null population
# true.slope <- 0
gpa <- rnorm(n=population.N,mean=3,sd=0.25)
# below creates gre.q such that it is strongly and positively related to gpa
gre.q <- 150 + true.slope*gpa + rnorm(n=population.N,mean=0,sd=10)
# below creates gre.q such that it is is unrelated to gpa
# gre.q <- 150  + rnorm(n=population.N,mean=0,sd=10)
plot(x=gpa, y=gre.q, main=paste("The population: r =",round(cor(gre.q,gpa),2)))


# One simulated study:
participants <- sample(1:length(gpa),size=10) #sampled 10 participants randomly
gpa.sample <- gpa[participants]
gre.q.sample <- gre.q[participants]
summary(lm(gre.q.sample ~ gpa.sample))
confint(lm(gre.q.sample ~ gpa.sample)) #finding the confidence interval
# confint(lm(gre.q.sample ~ gpa.sample), level=.99)



# defining a new function called onestudy
onestudy.std <- function(x, y, sampling.distribution.of.size, color, show.lines=F, show.points=F) {
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


# let's do that 5000 times, and let's keep track of the intercept and slope from each study
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


# let's see the sampling distribution of slopes across the 5000 studies


hist(sampling.distribution.of.r, breaks=20)
# let's see the sampling distribution of p across the 5000 studies

hist(sampling.distribution.of.p, breaks=20)
# Here's how you can think about the above: If you get an effect for which the
# p value is (say) 0.05, that means the probability of getting an effect like that
# or stronger is 0.05. Let's compare this with the effect for which the
# p value is 0.10. That means the likelihood of getting an effect like that
# or stronger is 10%. So what's the likelihood that your effect falls in this
# "not strong enough to be in the top 5% but strong enough in the top 10%"?
# Answer: 5%. 
# Similarly, what's the likelihood of getting an effect that is not strong
# enough to be in the top 10% but strong enough to be in the top 15%"? 
# Answer: 5%.
#

# hist(sampling.distribution.of.p[sampling.distribution.of.p<.05], breaks=5, main="Sampling distribution of estimated p")



