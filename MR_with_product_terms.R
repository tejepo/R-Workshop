N <- 10000
G <- runif(n=N,0,1) > 0.5 #creates a uniform disctibution where the n is the same as the premade variable N. We define the lower limit as 0 and the uppre limit as one. Presumeably 
G <- as.numeric(G)
H <- runif(n=N,0,10)
S <- 60 - 5*H - 25*G + 5*H*G + rnorm(n=N,0,5)

plot(H,S,ylim=c(0,100))
plot(H[G==1],S[G==1],ylim=c(0,100))
plot(H[G==0],S[G==0],ylim=c(0,100))

summary(lm(S ~ H + G + H*G))
# Why cant we interpret the h line here as the main effect
G <- 1-G #this is just a way to transform variables to use in the example
summary(lm(S ~ H + G + H*G))
# Generally speaking we don't like it when the results change drastically when researchers make arbitrary decisions. What we see here is that when we change G the t and p values of H change. Meaning that this isn't a good measure of the main effect of H. It's showing that the values of H depend on how the other variables are coded.
#H still has meaning, but it's conditional
H <- H - mean(H)
#this is asking what is the effect of G for those who are at the average hours of sleep. In the plot from line 7 we see that the mean H (middle of the x axis) is a place in the plot where the groups are no different for one another.
summary(lm(S ~ H + G + H*G))

##### Probing the meaning of an interaction
# We want to be mindful of what zero means in our variable
# By recoding/transforming the meaning of a variable such that the meaning of zero changes
# This is thought of as probing the meaning of an interaction. Which is to say that we're exploring what the data tells us as we 
### If I wanted to see what the slope was (or the simple regression equation) for the data when we were looking at G = 1 we can just substitute 1 for wherever G appears in the regression equation. However, this doesn't allow us to do a significance tests in the same way. To work around this we change the DATA such that the meaning of 0 changes. In doing this we're ostensibly creating an situation that serves the same purpose but allows us to see significance.
#####
##########################

A <- runif(n=N,20,50)
S <- 60 - 5*H - 25*G + 5*H*G + 0.1*A + 0.2*H*A + rnorm(n=N,0,3)
plot(A,S)

lm.all      <- lm(S ~ H + G + A + H*G + H*A)
lm.h.g.a.hg <- lm(S ~ H + G + A + H*G)
anova(lm.h.g.a.hg, lm.all)
summary(lm.all)

