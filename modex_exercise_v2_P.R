# Testing and Probing Interactions using Multiple Regression
# 
# The goal of this lab exercise is to practice testing interactions.
# 
# Dataset:   A sample of 245 individuals participating in an exercise study. Participants are asked to report their current age and the number of years they have been exercising vigorously. Then, participants are asked to jog on a treadmill for as long as possible.
# 
# Variable descriptions: 
#   
# case – Participant case ID
# xage – Self-reported age of participant
# zexer – number of years of vigorous physical exercise
# yendu – minutes run on an endurance test
# 
# We will examine the interaction between age and years of exercise as it predicts performance on running endurance.

#This will upload data for you:

df<-read.csv("https://raw.githubusercontent.com/connorjmccabe/Regression_523.525_2018/master/modexdata.csv")

#For your convenience, here are functions that will help us in probing the interactions later.

#Check out this first function. It takes a variable in vector and transforms it in a particular way:

center <- function(MOD) {
  centered <- MOD - mean(MOD,na.rm=T)
  return(centered)
}

#Q1: What does the above function do for us?

############

# This function allows you to put an object into the brackets. The values in the object will have the mean subtracted from each of them. the na.rm=T line just means that na values will be treated appropriately by R. Which is to say that they will be removed.

############


#Now, look at this function:

probe.low<-function(MOD) {
  low <- (MOD - mean(MOD,na.rm=T)) + sd(MOD, na.rm=T)
  return(low)
}

#Q2: What does the above function do instead?

############

# The above function, rather than taking the mean and subtracting it from each value in the object, this function grabs the standard deviation of the values in the object and add that value to the mean. The sum of the mean and one standard deviation will be used to center your data at one standard deviation below the mean

############


#This is the last one we'll be using.

probe.high <- function(MOD) {
  high <- (MOD - mean(MOD, na.rm=T)) - sd(MOD, na.rm=T)
  return(high)
}

#If you need help with the above, use this code as a hint:
mean(center(df$zexer))
mean(probe.low(df$zexer))
mean(probe.high(df$zexer))

#Let's start with estimating a model in which I have NOT transformed my variables:

m1.raw<- yendu ~ xage + zexer + xage:zexer
lm.raw<-lm(m1.raw,data=df)
summary(lm.raw)

#Q3: What is the regression formula for this model? Use the format provided below and replace each "##" with the approrate coefficients:

############

# yendu = 51.18 - .77*xage - 1.25*zexer + .04xage*zexer

############

#Q4: What is the interpretation of the coefficient of xage in this model?

############

# The coefficient xage in this model shows you what the rate of change is assosciated with a one unit increase in the self reported age of the participant given that they have zero years of vigorous physical exercise.

############

#Now, let's change this by subtracting the mean from each observation of zexer (that is, centering about the mean). We can do this using our center() function.

#Q5: this is a multiple choice question. Which of the following options is the correct way to change my formula such that zexer is centered about the mean? Comment out the options that are INCORRECT.

#option A:
#m1.cenexer<- yendu ~ xage + center(zexer) + xage:zexer

#option B:
#m1.cenexer<- yendu ~ xage + zexer + xage:center(zexer)

#option C:
m1.cenexer<- yendu ~ xage + center(zexer) + xage:center(zexer)
#it might be easier to just center the variable beforehand and call it zexer.cen

#Now, let's estimate this model and examine the results.

lm.cenexer<-lm(m1.cenexer,data=df)
summary(lm.cenexer)

#Q6: With zexer centered at the mean, what is the interpretation of the coefficient of xage in THIS model? How is it different than our model with raw variables?

############

#the interpretation here is that for people with an average number of years of physical exercise, each year of age (a one unit increase in xage) predicts .26 fewer minutes run on an endurance test.

############

#Now, let's examine this model with two other scenarios: one in which exercise is 1 SD BELOW its mean and one in which it is 1 SD ABOVE its mean. We will use the probe.low() and probe.high() functions to do this.

#Q7: specify the models for each below.

############
m1.exer.1sdbelow<- yendu ~ xage + probe.low(zexer) + xage:probe.low(zexer)
m1.exer.1sdabove<- yendu ~ xage + probe.high(zexer) + xage:probe.high(zexer)
############

lm.exer.1sdbelow<-lm(m1.exer.1sdbelow,df)
lm.exer.1sdabove<-lm(m1.exer.1sdabove,df)

#Let's now examine the results for each model. We'll look at the mean-centered version here as well.
summary(lm.exer.1sdbelow)
summary(lm.cenexer)
summary(lm.exer.1sdabove)

#To help in this, here is code that compares all 4 models we've examined.

coefs<-rbind(lm.raw$coefficients,
             lm.exer.1sdbelow$coefficients,
             lm.cenexer$coefficients,
             lm.exer.1sdabove$coefficients)
rownames(coefs)<-c("Raw","Centered 1SD Below","Mean-centered","Centered 1SD Above")

#Q8: Looking at the summary tables above, describe the effect of age on endurance when your level of exercise is relatively LOW (1 SD below the mean). Then, describe it when exercise is at the SAMPLE AVERAGE (at the mean). Then, describe it when exercise is relatively HIGH (1 SD above the mean).


############

# When your level of exercise is relatively low the effect of age on endurance is such that an increase in age is assosciated with a degrease in endurance. this relationship is significant (b = -.48, p < .001) The same is true for those with a relatively high level of exercise, just to a lesser degree. In this latter case the effect is not significant (b = -.04, p = .69).

############

#Q9: If you were to describe what these effects suggest, which would be the BEST description?

#A: No matter how much you exercise, your endurance will go down as you get older.

# B: At low and mean levels of exercise, age is associated with lower endurance, but among those exercising at relatively high levels, this effect is buffered/diminished.

#C: There is an interaction between age and exercise here.

############

#B: At low and mean levels of exercise, age is associated with lower endurance, but among those exercising at relatively high levels, this effect is buffered/diminished.

############



#################
#REGIONS OF SIGNIFICANCE
#################

# Below is a function for extracting the coefficient, standard error, and confidence interval of a given regressor from a given model
extract <- function(MOD, regressor) {
  tmp = numeric()
  tmp[1] <- MOD$coef[regressor] #run it, and put the coefficient in cell [i,2] of the table I made
  tmp[2] <- summary(MOD)$coef[regressor,2] #run it, and put the standard error in cell [i,3] of the table I made
  tmp[3] <- confint(MOD)[regressor,1] #run it, and put the lower limit of my 95% CI in cell [i,4] of the table I made
  tmp[4] <- confint(MOD)[regressor,2] #run it, and
  return(tmp)
}

r.zexer <- sort(c(-sd(df$zexer,na.rm=TRUE),sd(df$zexer,na.rm=TRUE),mean(center(df$zexer), na.rm=TRUE),seq(min(center(df$zexer)), max(center(df$zexer)), .1*sd(df$zexer,na.rm=TRUE))))
r.zexer<-round(r.zexer,3)
r.zexer

#Next, to stay organized, I'll make a data frame that will take the values from the loop I'm about to write and put them where I need them.

ros <- sapply(r.zexer, function(i) {
  n.zexer <- center(df$zexer) - i
  m <- yendu ~ xage + n.zexer + xage:n.zexer
  mod <- lm(m, df)
  tmp <- c(i, extract(mod, "xage"))
})
ros <- data.frame(t(ros))
colnames(ros) <- c("r.zexer","pub.PE","SE","LL","UL")


#meaningless extropolation 