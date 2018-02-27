## Exercise #7
## Terrence Pope

rm(list = ls())

# (1) Change the 3rd line of the code (not counting comments) to make gre.q positively related to gpa

population.N <- 100000
  gpa <- rnorm(n=population.N,mean=3,sd=0.25)
  gre.q <- 150 + 10*scale(gpa) + rnorm(n=population.N,mean=0,sd=10)

# (2) run the whole code and describe what happens to the sampling distribution of slope
  
  zgpa <- scale(gpa)
  zgre.q <- scale(gre.q)
  plot(x=zgpa, y=zgre.q, main=paste("The population. r =",round(cor(zgpa,zgre.q),2)))
  
  
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
  
  
  sampling.distribution.of.slope     <- NULL
  plot(x=zgpa, y=zgre.q, type='n', main=("Sampling distribution of slope"), xlim=c(-4,4),ylim=c(-4,4))
  for (i in 1:5000) {
    cat("Study",i,"\n")
    onestudy.result <- onestudy(x=zgpa, y=zgre.q, sampling.distribution.of.size=10, color=i, show.lines=T, show.points=F)
    slope <- summary(onestudy.result)$coef[2,1]
    sampling.distribution.of.slope    <- append(sampling.distribution.of.slope, slope)
  }
  
  hist(sampling.distribution.of.slope, breaks=20)

  cat("mean(sampling.distribution.of.slope) =",mean(sampling.distribution.of.slope),"\n")
  

# Answer to #2 # : The center is pulled to the right of the x axis, it has become slightly negatively skewed.
  
# (3) Do (5) and (6) of Exercise #6 (about p values for obtaining a slope (or |slope|) equal to or greater than 0.3) with this sampling distribution.

  length(sampling.distribution.of.slope[sampling.distribution.of.slope > .3])
  # then we take that and divide it by the total number (in this case, 5000)
  
  length(sampling.distribution.of.slope[sampling.distribution.of.slope > .3])/5000 #one tailed
  
  (length(sampling.distribution.of.slope[sampling.distribution.of.slope > .3]) + length(sampling.distribution.of.slope[sampling.distribution.of.slope < - .3]))/5000 #two tailed
  
# (4) What is this probability known as? Make sure to refer to the slope in the population.
  
# Answer to #4 # This probability is know as a p value? It is the likelihood of drawing a sample with this slope from the population given that the null hypothesis is true. Which in this case is pretty high. the p is .9 which basically suggests that a slope of .3 would very likely be drawn from a sample of the null population.