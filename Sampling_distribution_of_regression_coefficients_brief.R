# setting up a population in which gre.q is positively related to gpa
population.N <- 100000 # note I increased this to 100,000 so the population
                       # more closely approximates the true 'null' population
gpa <- rnorm(n=population.N,mean=3,sd=0.25)
# below creates gre.q such that it is strongly and positively related to gpa
# gre.q <- 150 + 10*scale(gpa) + rnorm(n=population.N,mean=0,sd=10)
# below creates gre.q such that it is is unrelated to gpa
gre.q <- 150  + rnorm(n=population.N,mean=0,sd=10)
zgpa <- scale(gpa)
zgre.q <- scale(gre.q)
plot(x=zgpa, y=zgre.q, main=paste("The population. r =",round(cor(zgpa,zgre.q),2)))


# defining a new function called onestudy
onestudy <- function(x, y, sampling.distribution.of.size, color, show.lines=F, show.points=F) {
  participants <- sample(length(x),size=sampling.distribution.of.size)
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


# let's do that 5000 times, and let's keep track of the intercept and slope from each study
sampling.distribution.of.slope     <- NULL
plot(x=zgpa, y=zgre.q, type='n', main=("Sampling distribution of slope"), xlim=c(-4,4),ylim=c(-4,4))
for (i in 1:5000) {
  cat("Study",i,"\n")
  onestudy.result <- onestudy(x=zgpa, y=zgre.q, sampling.distribution.of.size=10, color=i, show.lines=T, show.points=F)
  slope <- summary(onestudy.result)$coef[2,1]
  sampling.distribution.of.slope    <- append(sampling.distribution.of.slope, slope)
}


setwd("c:\\temp")
# pdf("sampling.distribution.of.slope.pdf")
hist(sampling.distribution.of.slope, breaks=20)
# dev.off()
cat("mean(sampling.distribution.of.slope) =",mean(sampling.distribution.of.slope),"\n")
# let's see the sampling distribution of p across the 5000 studies
# pdf("sampling.distribution.of.p.pdf")
