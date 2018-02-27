#Exercise #9 (Intro to Hierarchical Multiple Regression)
#Terrence Pope

#rm(list=ls())
library(psych)
options(scipen = 20)

N <- 1000 
age.std       <- rnorm(n=N)

agreeableness <- rnorm(n=N)  + age.std
recklessness  <- rnorm(n=N)  - age.std

accidents    <- (3 + 0.6*recklessness 
                 - 0.4*agreeableness
                 + rnorm(n=N))
accidents    <- round(accidents,0)
accidents[accidents<0] <- 0
hist(accidents)
round(cor(cbind(accidents,agreeableness,recklessness)),2)

lm.acc.agree      <- lm(accidents ~ agreeableness)
lm.acc.reck       <- lm(accidents ~ recklessness)
lm.acc.agree.reck <- lm(accidents ~ agreeableness + recklessness)


# (1)
cat("\nThe increase in the proportion of variance accounted for by adding agree on top of reck =",
    summary(lm.acc.agree.reck)$r.squared - summary(lm.acc.reck)$r.squared, "\n")

# (2)
pred.agree.reck <- lm(agreeableness ~ recklessness)$residuals
cor(accidents, pred.agree.reck)

# # (3)
# The first is interested in the unique contribution of agreeableness, which is to say that if you pulled recklessness out of it, how much is agreeableness affecting the accident rate. What you get is basically R-squared as R squared in bivariate regression is understood. The correlation between agreeableness and accidents can also be shown in a different way that controls for the effect of z by regressing z onto x and then capturing the residuals. The result of correlating the residuals and y is r. Which, when squared will give you the same result as you get from the equation in (1).
