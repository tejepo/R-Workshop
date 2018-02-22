library(psych)
# setting up a population in which gre.q is positively related to gpa
population.N <- 10000 #smallish so my laptop can do this
sample.size.in.each.simulated.study <- 20
r.AC.gre.v <- 0.8
r.AC.gre.q <- 0.5
# true.slope <- 0
# AC below stands for "academic competence"
AC <- rnorm(n=population.N,mean=0,sd=1)
# creating gre.v such that cor(gre.v,AC) = intercorrelation
# In case you're interested, here's the rationale behind the formula I used below:
# First, in general, cor(X,Y) = cov(X,Y)/(sd(X),sd(Y)).
# So to make things simple, let's set up X and Y such that sd(X) = sd(Y) = 1,
# Then cor(X,Y) = cov(X,Y).
#
# Let's say we created Y such that Y = b*X + noise
# then cor(X,Y) = cov(X, b*X + noise) = cov(X,b*X) + cov(X,noise)
#               ~ b*cov(X,X) + 0  # let "~" mean "equals in the long run"
#               = b*var(X,X)
#               = b # because sd(X) = 1.
#
# Q: How large should var(noise) be, so that var(Y) = 1?
# A: 1 - b^2.
#
# So, what we shold do is create Y such that Y = b*X + rnorm(sd=sqrt(1-b^2))
# 
population.gre.v <- r.AC.gre.v*AC + sqrt(1 - r.AC.gre.v^2)*rnorm(n=population.N)
# Let's confirm:
cor(AC, population.gre.v)
population.gre.q <- r.AC.gre.q*AC + sqrt(1 - r.AC.gre.q^2)*rnorm(n=population.N)
cor(AC, population.gre.q)
#
# What's the correlation between gre.v and gre.q?
cor(population.gre.v, population.gre.q)#
# Let's linearly transform gre.v and gre.q so that the variables
# have a mean of 150 and sd of 20. Note cor(gre.v,gre.q) will remain
# the same. (Why?)
population.gre.v <- 150 + 20*population.gre.v
population.gre.q <- 150 + 20*population.gre.q
describe(population.gre.v)
describe(population.gre.q)
cor(population.gre.v, population.gre.q)
#
# Now, let's create the DV, Npubs, let's say it's the N of pre-tenure pubs.
population.Npubs <- 20 + 3*scale(population.gre.v) + 2*scale(population.gre.q) + rnorm(n=population.N)
describe(population.Npubs)
hist(population.Npubs)

# Let's do a study:
participantIDs <- sample(1:length(population.Npubs), size=50) #sampling participants by creating a variable called participantID
sample.gre.v <- population.gre.v[participantIDs]
sample.gre.q <- population.gre.q[participantIDs]
sample.Npubs <- population.Npubs[participantIDs]
describe(sample.gre.v)
describe(sample.gre.q)
describe(sample.Npubs)
plot(x=sample.gre.v, y=sample.Npubs)
plot(x=sample.gre.q, y=sample.Npubs)


# Let's say your prediction equation was:
Pred  <- 20 + .3*(scale(sample.gre.v)) + 3*(scale(sample.gre.q)) #this is still a linear transformation as we're multiplying the variables by some value and adding. The number 20 in this example represents the intercept of the regression line and this effects each score. By adding or subtracting the value of the mean residuals you can come closer to zero.

# Let's compute the residuals:
Resid  <- sample.Npubs - Pred
Resid2 <- Resid^2
# Let's see how our prediction fits the actual Npubs:
plot(sample.Npubs, Pred)
sum(Resid) #the sum of residuals should be zero. This means that the residuals are balanced around the best fit line. Some positive values and some negative values similar to how numbers fall on either side of the mean.
mean(Resid) #If the mean is positive then you are underestimating the intercept. If it is negative then you are overestimating the intercept. The intercept can be adjusted up or down
sum(Resid2) #the definition of good fit is when the sum of the squared residuals are minimized
plot(sample.Npubs, Resid) #Plot the residuals against the x axis
plot(Pred,Resid) #looking to see if there is a pattern of over or underestimation. This might be a clue that you have adjust your slope

#If we try this process using the values from separate regression models let's see what happens

#To evaluate how good of a fit 
lm(sample.Npubs ~ sample.gre.v)
lm(sample.Npubs ~ sample.gre.q)

#Then rewrite the eqation
Pred <- -8.5 + .19*sample.gre.v + .19*sample.gre.q
#The 8.5 comes from the mean of the two intercepts

# Let's compute the residuals:
Resid  <- sample.Npubs - Pred
Resid2 <- Resid^2

# Let's see how our prediction fits the actual Npubs:
plot(sample.Npubs, Pred)
sum(Resid) 
mean(Resid) 
sum(Resid2) 
plot(sample.Npubs, Resid) 
plot(Pred,Resid)

#What you see is that it doesn't quite cut it. It's difficult to calculate the formula given the separate linear models that don't account for the degree to which gre v and gre q are correlated

#Let's try a different formula
lm(sample.Npubs ~ sample.gre.v + sample.gre.q)
#This model places both predictos on the same side of the regression equation. This evokes the formula that considers the contribution of both variables at the same time.

#Rewrite the eqation
Pred <- -16.3150 + .1401*sample.gre.v + .1025*sample.gre.q
#The 8.5 comes from the mean of the two intercepts

# Let's compute the residuals:
Resid  <- sample.Npubs - Pred
Resid2 <- Resid^2

# Let's see how our prediction fits the actual Npubs:
plot(sample.Npubs, Pred)
sum(Resid) 
mean(Resid) 
sum(Resid2) 
plot(sample.Npubs, Resid) 
plot(Pred,Resid)

#Now we've reduced the variance while still preserving the balance between our residuals and x and making sure that our residuals are uncorrelated with our predictor.

#Question number 7
#Ostensibly this exercise shows us that multiply regression, while still using the same fundamentals of OLS regression it's not enough to just run separate regressions and plug the slopes and intercepts into the multiple regression formula of yhat = ax1 + bx2 + e. The main problem with this is that the two variables are correlated and the slopes and intercepts are inaccurate estimates of the contribution of x1 on on yhat accounting for the influence odf x2. Instead, running a regression on both variables comboined will give you multiple slopes that have accounted for this influence. 

