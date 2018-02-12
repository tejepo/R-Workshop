library(psych)
options(scipen = 20)
set.seed(1)

N <- 1000 
age.std       <- rnorm(n=N)
# Creating agreeablenes and recklessness, both a function of age.std plus noise
agreeableness <- rnorm(n=N)  + age.std
recklessness  <- rnorm(n=N)  - age.std
phonedependence <- rnorm(n=N) - 0.5*age.std
# uncorrelated version below:
# agreeableness <- rnorm(n=N) # + age.std
# recklessness  <- rnorm(n=N) # - age.std
# Strongly correlated version below:
# agreeableness <- rnorm(n=N)  + 2*age.std
# recklessness  <- rnorm(n=N)  - 2*age.std
accidents    <- (3 + 0.6*recklessness 
                   - 0.4*agreeableness
                   + 0*phonedependence
                   + rnorm(n=N))
accidents    <- round(accidents,0)
accidents[accidents<0] <- 0
hist(accidents)
round(cor(cbind(accidents,agreeableness,recklessness)),2)
      
######### Data set created. Let's analyze it with MR ##############

lm.acc.agree      <- lm(accidents ~ agreeableness)
lm.acc.reck       <- lm(accidents ~ recklessness)
lm.acc.agree.reck <- lm(accidents ~ agreeableness + recklessness)

# Let's understand what's in lm.acc.agree.reck
# Here are the names of the components of lm.acc.agree.reck
names(lm.acc.agree.reck) 

# here are the coefficients from the last one
lm.acc.agree.reck$coefficients
# Let's use that to compute the predicted number of accidents
pred.acc.agree.reck <- (lm.acc.agree.reck$coefficients[1]
                      + lm.acc.agree.reck$coefficients[2]*agreeableness
                      + lm.acc.agree.reck$coefficients[3]*recklessness)
# By the way, although we calculated pred.acc.agree.reck from the coefficients,
# that was for the purpose of demonstrating what it means.
# We don't actually have to do that by hand. See:
plot(pred.acc.agree.reck,lm.acc.agree.reck$fitted.values)

### Let's understand Multiple-R, a critical concept in Hierarchical MR ###
# It's a part what summary() reports:
summary(lm.acc.agree.reck)
# How does this relate to the variance of the predicted (i.e., variance accounted for)?
# What's the variance of the predicted?
var(pred.acc.agree.reck)
# what's the total variance of the DV?
var(accidents)
# what proportion of the DV variance does the MR-based predictor account for?
var(pred.acc.agree.reck)/var(accidents)
# How does this compare with the r.squared from the MR?
summary(lm.acc.agree.reck)$r.squared
# How does this relate to the correlation between the predicted and actual?
cor(accidents, pred.acc.agree.reck)
cor(accidents, pred.acc.agree.reck)^2

########### Let's understand "hierarchical" MR ##############

# Now, let's see how the r.squared from the three regression runs relate to each other:
round(summary(lm.acc.agree)$r.squared,4)
round(summary(lm.acc.reck)$r.squared,4)
round(summary(lm.acc.agree.reck)$r.squared,4)

cat("\nThe increase in the proportion of variance accounted for by adding reck on top of agree =",
    summary(lm.acc.agree.reck)$r.squared - summary(lm.acc.agree)$r.squared, "\n")
cat("\nThe increase in the proportion of variance accounted for by adding agree on top of reck =",
    summary(lm.acc.agree.reck)$r.squared - summary(lm.acc.reck)$r.squared, "\n")


# Let's see how this relates to semi-partial correlations
# To compute the semi-partial correlation for predicting Accidents from Agreeableness, controlling for Recklessness:
# (1) Regress Agreeableness on Recklessness, and compute the residuals, Agree.reck
# (2) i.e., agree.reck = agreeableness - AgreeablenessPredictedByRecklessness
# (3) Compute the correlation of agree.reck with accidents. That is the semi-partial correlation.

# pred.agree.reck <- lm(agreeableness ~ recklessness)$fitted.values
# resid.agree.reck <- agreeableness - pred.agree.reck
# cor(accidents, resid.agree.reck)
resid.agree.reck <- lm(agreeableness ~ recklessness)$residuals
cor(accidents, resid.agree.reck)
cor(accidents, resid.agree.reck)^2


# Below is how you test if the increase is significant, but we'll get to that later.
# anova(lm.acc.agree, lm.acc.agree.reck)
# 



######### What if agree and recklessness are UNcorrelated? #############

# Go back to the top, and create agreeableness and recklessness again but this
# time without any influence of age.

########################### The 'Ballantine' diagram ##############

# install.packages('VennDiagram')
library(VennDiagram)

grid.newpage()
draw.pairwise.venn(area1 = 1, 
                   area2 = 1, 
                   cross.area = round(summary(lm.acc.agree)$r.squared,2), 
                   category = c("accidents","agreeableness"),
                   fill = c("light blue","pink"))

grid.newpage()
draw.pairwise.venn(area1 = 1, 
                   area2 = 1, 
                   cross.area = round(summary(lm.acc.reck)$r.squared,2), 
                   category = c("accidents","agreeableness"),
                   fill = c("light blue","pink"))

# Now let's compute the areas of a three-way ballantine
area12  = round(100*summary(lm.acc.agree)$r.squared,2)
cat("\n% of var(accidents) accounted for by agreeableness =",area12,"\n")

area13  = round(100*summary(lm.acc.reck)$r.squared,2)
cat("\n% of var(accidents) accounted for by recklessness =",area13,"\n")

area12and13 = round(100*summary(lm.acc.agree.reck)$r.squared,2)
cat("\n% of var(accidents) accounted for by agreeableness AND recklessness =",area12and13,"\n")

# Based on this
cat("\n% of var(accidents) accounted for by agreeableness on above and beyond recklessness =",
    area12and13 - area13,"\n")
cat("\n% of var(accidents) accounted for by recklessness on above and beyond agreeableness =",
    area12and13 - area12,"\n")

# The area of the intersection of A and B would have to be (A + B) - (A and B):
area123 = round(100*( summary(lm.acc.agree)$r.squared
                      + summary(lm.acc.reck)$r.squared
                      - summary(lm.acc.agree.reck)$r.squared),2)
cat("\n% of var(accidents) in the 3-way intersection =",area123,"\n")

# The analogy works pretty well for dividing var(DV), but it breaks down for other parts:
# grid.newpage()
# draw.triple.venn( area1 = 1, 
#                   area2 = 1,
#                   area3 = 1,
#                   n12 = area12,
#                   n13 = area13,
#                   n23 = 0.8,  # can't use the real value; venn diagram is not perfect analogy
#                   n123 = area123,
#                   category = c("accidents","agreeableness","recklessness"),
#                   fill = c("skyblue","pink1","mediumorchid"))
#                   rotation.degree=180)


###### Three IVs ##########
lm.acc.agree      <- lm(accidents ~ agreeableness)
lm.acc.reck       <- lm(accidents ~ recklessness)
lm.acc.agree.reck <- lm(accidents ~ agreeableness + recklessness)
lm.acc.agree.reck.phone <- lm(accidents ~ agreeableness + recklessness + phonedependence)

round(summary(lm.acc.agree)$r.squared,4)
round(summary(lm.acc.agree.reck)$r.squared,4)
round(summary(lm.acc.agree.reck.phone)$r.squared,4)

anova(lm.acc.agree, lm.acc.agree.reck, lm.acc.agree.reck.phone)

# Here's an excellent tutorial page: http://data.library.virginia.edu/hierarchical-linear-regression/

