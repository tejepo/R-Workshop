##########################################################
# Categorical Predictors in Bivariate Regression (in R!) #
##########################################################
# Max Halvorson #
# 2/1/18        # 
# PSYCH 525     # 
#################

rm(list=ls())
library('ggplot2') #same function as require
library('psych')

#####################################################
# What do categorical data look like in regression? #
#####################################################

# Let's generate some data
set.seed(1337) # this will ensure we all get the same result across runs

rural<-rbinom(n=100,1, prob = .6) #binomial distribution. Probably of getting a 1 versus a 0 is .6
bpm<- 50 + 3*as.vector(scale(rural)) + rnorm(length(rural),mean=0, sd=8) #rnorm generates a normal distribution

df<-as.data.frame(cbind(rural,bpm))
df_urban<-df[which(df$rural=="0"),] #which is a way of selecting things from our data this is assigning 0 to urban
df_rural<-df[which(df$rural=="1"),]

View(df)

# Let's see our data as a boxplot
ggplot() +
  geom_boxplot(aes(x=factor(rural),y=bpm), size = .5) +
  theme_bw()

# "Point-biserial correlation": a correlation between a binary category and a continuous variable
ggplot() +
  geom_point(aes(x=rural,y=bpm), size = .5) +
  xlim(c(-.5,1.5)) +
   annotate(geom = "point", x=0, y=mean(df_urban$bpm), color = "red", size=4) +
   annotate(geom = "point", x=1, y=mean(df_rural$bpm), color = "red", size=4) +
   geom_hline(yintercept=mean(df$bpm), color="blue") + #blue line is grand mean, this is anova
   geom_smooth(data=df,aes(x=rural,y=bpm),method="lm", se=F) +
  theme_bw()

# Points could overlap, so hard to tell density. Let's make some histograms
hist(df_urban$bpm)
hist(df_rural$bpm)

# They're hard to compare; let's display them on top of one another
ggplot(df, aes(x=bpm)) +  
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  facet_grid(rural ~ .) +
  geom_vline(xintercept=mean(df_urban$bpm), color="red") +  
  geom_vline(xintercept=mean(df_rural$bpm), color="red") +
  geom_vline(xintercept=mean(df$bpm), color="blue") +
  theme_bw()

#this is the same as the box blot if you rotate it 90 degrees. The first red line is the mean of the top, the blue is the grand mean, and the second red line is the mean of the bottom group

# Okay, they look a little different; but how different are they?


####################################################
# How would we have tested this before regression? #
####################################################

psych::describe(df)
mean(df$bpm[df$rural==0])
mean(df$bpm[df$rural==1])

# A t-test! Having flashbacks?
t.test(df_urban$bpm,df_rural$bpm, alternative="two.sided", var.equal=T)

# What about correlation?
cor(df$rural, df$bpm)
cor.test(df$rural, df$bpm) # this looks familiar...

# And what about regression?
summary(lm(df$bpm~df$rural, data=df)) 

###
# Let's pause for a second: 
#   What does the slope mean? the slope is the difference between the group when x = 0 and when x = 1
#   What does the intercept mean? It's your reference group, it's what zero means
#   Why is it important to name our variables in an informative way? 
#   Do we need to know both categories to interpret this coefficient?
###

# Just for funsies, what about ANOVA?
oneway.test(df$bpm~df$rural, var.equal=T)

# What do we notice about these results? Can we be convinced that our analyses match our data?


########################################################
# Why does this matter, why don't we just do a t-test? #
########################################################

# Well, for bivariate regression (just one predictor), there's no distinct difference or advantage. But as we add more predictors - reviewers often ask you to control for gender or another categorical predictor - using regression allows us to continue to quantify the effect of a categorical X on a continuous Y. This can also be nice in quantifying interactions...


#############################################################
# Let's look at some real data, with multiple categories... #
#############################################################

setwd("~/Desktop/UW/Winter '18/Linear Models & Data Analysis/523")
load("nbadata.RData") # note the "load" function for RData files

psych::describe(nbadata)
#warnings mean you can run the function but makes it difficult to interpret the output. Errors wont run.

# One thing we might do is dichotimize a multiple-category predictor into a binary predictor
nbadata$OrangeJ <- 1*(nbadata$col=="orange") #true when col is black and false when color is non (multiply by 1 to assign the true to one)
summary(lm(nbadata$W.L. ~ factor(nbadata$OrangeJ), data=nbadata))

# What if we want to know about more than just a single group comparison? 
by(nbadata$W.L., nbadata$col, mean) #mean of first group organized by means of the second groups
ftable(factor(nbadata$col))

# What does R do if we don't think carefully about this?
summary(lm(nbadata$W.L. ~ factor(nbadata$col), data=nbadata))
#the effect of each of these compared to black.

### What do you think these coefficients mean? ###
### Aside: remember contrasts? ###
