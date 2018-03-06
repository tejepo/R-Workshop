################################################
# Quadratic Effects in Multivariate Regression #
################################################
# Max Halvorson # 
# 3/5/18        # 
# PSYCH 525     # 
################# 

rm(list=ls())
library('ggplot2')
library('psych')
library('tidyverse')
library('rvest')
library('lm.beta')


#######################################
# Quadratics and interaction terms... #
#######################################

data("mtcars")
lmint <- lm(mpg ~ scale(hp, scale=F) + scale(wt, scale=F) + scale(hp, scale=F)*scale(wt, scale=F), data=mtcars)
summary(lmint)

lmquad <- lm(mpg ~ scale(hp, scale=F) + scale(wt, scale=F) + I(scale(hp, scale=F)^2), data=mtcars)
summary(lmquad)

lmint
lmquad

qplot(mtcars$hp, mtcars$mpg) + 
  geom_smooth(method="loess")


################
# Read in data #
################

tibnba <- as.tibble(read.csv("https://raw.githubusercontent.com/mhalvo/teaching/master/NBAcombine.csv"))

# recode some 0 values as NA
tibnba$Height[tibnba$Height==0] <- NA
tibnba$Weight[tibnba$Weight==0] <- NA
tibnba$Body.Fat[tibnba$Body.Fat==0] <- NA
tibnba$Vertical[tibnba$Vertical==0] <- NA
tibnba$Wingspan[tibnba$Wingspan==0] <- NA
tibnba$Standing.Reach[tibnba$Standing.Reach==0] <- NA
tibnba$T.Q..Spring[tibnba$T.Q..Spring==0] <- NA
tibnba$L.A.T.[tibnba$L.A.T.==0] <- NA

# let's just look at players who played at least one game in the NBA (
tibnba <- filter(tibnba, NSeasons > 0)

# correlations
cor(select(tibnba, Weight:L.A.T., NSeasons), use="pairwise.complete.obs") %>% round(., 2)


####################################################################################
# Is there a quadratic effect of player weight, controlling for other athleticism? #
####################################################################################

lm1 <- lm(NSeasons ~ Height + Weight + Body.Fat + Vertical + T.Q..Spring + L.A.T., 
          data=tibnba)
summary(lm1) # base model with linear terms

lm2 <- lm(NSeasons ~ Height + Weight + Body.Fat + Vertical + T.Q..Spring + L.A.T. + I(Weight^2), 
          data=tibnba)
summary(lm2) # base model with quadratic term for weight

anova(lm1, lm2) # is the quadratic model better?


######################
# Any problems here? #
######################

# a hint: how do we interpret these coefficients? what does the intercept represent?
# something more subtle: how do the linear and quadratic terms relate? would this change for gymnasts?

# how do we solve this?


###########################
# Let's center and re-run #
###########################

# center our variables for interpretability
tibnba$cHeight <- tibnba$Height - mean(tibnba$Height, na.rm=T)
tibnba$cWeight <- tibnba$Weight - mean(tibnba$Weight, na.rm=T)
tibnba$cBody.Fat <- tibnba$Body.Fat - mean(tibnba$Body.Fat, na.rm=T)
tibnba$cVertical <- tibnba$Vertical - mean(tibnba$Vertical, na.rm=T)
tibnba$cT.Q..Spring <- tibnba$T.Q..Spring - mean(tibnba$T.Q..Spring, na.rm=T)
tibnba$cL.A.T. <- tibnba$L.A.T. - mean(tibnba$L.A.T., na.rm=T)

# centered models
lm1c <- lm(NSeasons ~ cHeight + cWeight + cBody.Fat + cVertical + cT.Q..Spring + cL.A.T., 
          data=tibnba)
summary(lm1c) # centered model with linear terms

lm2c <- lm(NSeasons ~ cHeight + cWeight + cBody.Fat + cVertical + cT.Q..Spring + cL.A.T. + I(cWeight^2), 
          data=tibnba)
summary(lm2c) # centered model with quadratic term for weight

anova(lm1, lm1c) # does centering change our variance explained by the models?
anova(lm2, lm2c)
anova(lm1c, lm2c)


####################################################
# Let's compare our centered and uncentered models #
####################################################

lm.beta(lm2) 
lm.beta(lm2c)


#########################################################
# Now let's plot! Was it worth adding a quadratic term? #
#########################################################

xi <- seq(min(tibnba$cWeight, na.rm=T), max(tibnba$cWeight, na.rm=T), 5)
predsl <- rep(NA,length(xi))
predsq <- rep(NA,length(xi))

for (i in 1:length(xi)){
  predsl[i] <- predict(lm1c,
                      newdata=data.frame(cHeight=0,
                                      cBody.Fat=0,
                                      cVertical=0,
                                      cT.Q..Spring=0,
                                      cL.A.T.=0,
                                      cWeight=xi[i]))
  predsq[i] <- predict(lm2c,
                       newdata=data.frame(cHeight=0,
                                          cBody.Fat=0,
                                          cVertical=0,
                                          cT.Q..Spring=0,
                                          cL.A.T.=0,
                                          cWeight=xi[i]))
}
df <- data.frame(xi=xi, predsl=predsl, predsq=predsq)

(wscatter <- ggplot(aes(x=cWeight, y=NSeasons), data=tibnba) +
   geom_point() + 
   theme_bw())

(wlin <- wscatter +
    geom_smooth(data=df, aes(x=xi, y=predsl), width=3))

(wquad <- wlin +
    geom_smooth(data=df, aes(x=xi, y=predsq), width=3))