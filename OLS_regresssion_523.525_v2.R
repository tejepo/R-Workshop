########Linear Models in R#############

#Now that we have the basics down and know how to work our data, we're ready for the fun part - running models.

#Here are the functions you will need for this tutorial:


?lm()
?summary()
?coef()
?confint()
?lm.beta()

####Standard stuff I put at the top of my code########

rm(list = ls())
options(scipen=20) #penalize scientific notation e.g. -2E134123.3
options(digits=4) #show 4 digits when displaying results (e.g., 0.000 or 00.00)

#One example of a home-baked function:

center<-function(MOD)
{
  centered<-MOD-mean(MOD,na.rm=TRUE)
  return(centered)
}

#Run the following line ONLY if you have not installed the below packages
# install.packages(c("psych","QuantPsyc","foreign"))

require(psych)
require(QuantPsyc)
require(foreign)

gre.gpa<-read.csv("https://raw.githubusercontent.com/connorjmccabe/Regression_523.525_2018/master/GPA_GRE_Q.csv") #You can read in csv files directly from github

View(gre.gpa) #the capitol in V is important

#Create summary tables
desc<-psych::describe(gre.gpa) #looking for the describe function from within the psych package. In this case it's because there are multiple programs that have the describe function. This may make it possible to circumvent loading the package itself.

#class(desc) will tell you what class of object this is. ?typeof will list out the types of objects. 

#a dataframe is a specific type of list. typeof(desc) will show list as the type.

#examine specific variables
desc[1,3] 
desc["GRE.Q","mean"] #mean here is just referencing 

#this is a generic list so we can call object from within a list using the "$" (e.g. - mean(gre.gpa$GRE.Q)

#there are a number of ways to check and see what type of data you're working with. You can just type in the object. You can use typeof() to see. Or you can look at is.numeric() and it will return a true/false response.

#lapply is a function that teaches you to apply something to a list. (e.g. - lapply(gre.gpa,is.numeric) would apply the is.numeric function to both of your variables. You can do this with any function)

desc[2,] #return row 2 of desc data. 

#examine variable names
names(gre.gpa)

#working with individual variables
#OK:
gre.gpa[,2]
#Better:
gre.gpa$GRE.Q

gre.gpa[gre.gpa==9999]<-NA #For all values in this data.frame that equal 9999 assign them to NA

#computing means, SDs
mean(gre.gpa$GRE.Q) #if you have even one NA these kinds of functions wont 
mean(gre.gpa$GRE.Q, na.rm = TRUE)#Removes NAs from variable
sd(gre.gpa$GRE.Q, na.rm = TRUE)

#Standardizing variables
scale(gre.gpa$GRE.Q)
scale(gre.gpa$GRE.Q, scale = FALSE)

#Can we check if scale() is working?
mean(scale(gre.gpa$GRE.Q)) #this shows part of the central limit theorem. Adding all of these values should result in zero
sd(scale(gre.gpa$GRE.Q)) # this should be 1
mean(scale(gre.gpa$GRE.Q, scale = FALSE)) #this centers your data at zero without standardizing it
sd(scale(gre.gpa$GRE.Q, scale = FALSE)) #as a result the standard deviation here will not change
#these latter two are examples of linear transformations.

#My steps for estimating a linear model
#1. define the model!

m1<- GRE.Q ~ College.GPA # ~ this means "regressed onto". At this stage the things packed into m1 could be referrnig to anything in any data set you have loaded.

#2. use lm() to estimate the model

lm.gre.gpa<-lm(m1,data = gre.gpa)

#3. Examine results
lm.gre.gpa
summary(lm.gre.gpa)
names(lm.gre.gpa) #Really handy way to get a sense of an object and what objects are in the list

#Fun stuff you can do

cor(lm.gre.gpa$residuals,lm.gre.gpa$fitted.values) #What value will this be? Why? Ordinary least squares ensures that the values on the best fit line are not correlated with the residuals

mean(lm.gre.gpa$residuals) #This value?
var(lm.gre.gpa$residuals) + var(lm.gre.gpa$fitted.values) #What does this equal?
var(gre.gpa$GRE.Q)

###The code below proves that when all my predictors are centered at the mean, the value of my intercept is ________.###

m2<- GRE.Q ~ center(College.GPA)
lm.gre.gpa.center<-lm(m2,data = gre.gpa)

#What changes when I center my predictor?
rbind(lm.gre.gpa$coefficients,
      lm.gre.gpa.center$coefficients) #the mean of x becomes the y intercept and also the mean of y.

#What if I standardized my variable(s)?
m3<- GRE.Q ~ scale(College.GPA) #standardizing predictor only
m4<- scale(GRE.Q) ~ scale(College.GPA) #standardizing BOTH outcome and predictor
lm.gre.gpa.scale<-lm(m3,data = gre.gpa)
lm.gre.gpa.scaleall<-lm(m4,data = gre.gpa)

#What changes here?
coefs<-rbind(lm.gre.gpa$coefficients,
      lm.gre.gpa.center$coefficients,
      lm.gre.gpa.scale$coefficients,
      lm.gre.gpa.scaleall$coefficients)

rownames(coefs)<-c("unstandardized",
                   "centered GPA",
                   "standardized GPA",
                   "standardized GREQ and GPA")

coefs

#What value does the below corrrespond with? Why? (HINT: CCWA p. 33, equation 2.4.3 and 2.4.4)
cor(gre.gpa$College.GPA,gre.gpa$GRE.Q)
