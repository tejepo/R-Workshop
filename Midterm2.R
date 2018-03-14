#Midterm 

#Example: Above and beyond the ffects of age, fgender, and alcohol use, a 1 unit increase in planingfulness was assosciated with a XXX (unstandardized coefficient aka estimate directly from the table output) unit decrease in depression (SE = XX, 95% CI (use confint) = [XX, XX], B (use lm.beta) = XX).

#remember that the goal of centering is to make the zero point meaningful. With continuous data you want to center your data on it's mean.

#Example 2: Gender was not significantly assosciated with depression (b = XX, SE = XX, 95% CI = BLAH, B). 

#b and B are intercepts and Beta is standardized coefficient

#Use a centered function for categorical variables. Always use zero as a reference point. If you're covarying for gender your goal is to make the line representative of each person in your sample which will be .5 if gender is coded as 0 and 1

#treat the alcohol variable as continuous. Though it doesn't matter. R just wont let you grab a mean from a categorical variable. Even if it's categorical making it a numeric variable will allow you to center it.

rm(list=ls())
require(RCurl) #this allows me to pull data from a URL (see below)
require(car) #a package I use to recode
require(dplyr)
require(lm.beta) #optional package. could also use QuantPsyc
require(ggplot2) 
require(pander)

#reading the data file in from github
data <-read.csv(text=getURL("https://raw.githubusercontent.com/tejepo/R-Workshop/master/Data/midtermDataPope.csv"))

View(data)

#using the 'car' function to recode variables and then rename the column.
data$BIO_SEX<-car::recode(data$BIO_SEX,"'(1) Male'=0") 
data$BIO_SEX<-car::recode(data$BIO_SEX,"'(2) Female'=1") 
colnames(data)[colnames(data)=="BIO_SEX"] <- "sex"

#trying another way that might be simpler
data$H1TO15 <- as.numeric(data$H1TO15)

#defining this value as NA so that the missing data can be accounted for later.
colnames(data)[colnames(data)=="H1TO15"] <- "alcoholUse"

data$H1FS1<-car::recode(data$H1FS1,"'(0) Never/rarely'=0")
data$H1FS2<-car::recode(data$H1FS2,"'(0) Never/rarely'=0")
data$H1FS3<-car::recode(data$H1FS3,"'(0) Never/rarely'=0")
data$H1FS4<-car::recode(data$H1FS4,"'(0) Never/rarely'=3") #the lines with a '#' symbol below were reverse coded.
data$H1FS5<-car::recode(data$H1FS5,"'(0) Never/rarely'=0")
data$H1FS6<-car::recode(data$H1FS6,"'(0) Never/rarely'=0")
data$H1FS7<-car::recode(data$H1FS7,"'(0) Never/rarely'=0")
data$H1FS8<-car::recode(data$H1FS8,"'(0) Never/rarely'=3") #
data$H1FS9<-car::recode(data$H1FS9,"'(0) Never/rarely'=0")

data$H1FS1<-car::recode(data$H1FS1,"'(1) Sometimes'=1")
data$H1FS2<-car::recode(data$H1FS2,"'(1) Sometimes'=1")
data$H1FS3<-car::recode(data$H1FS3,"'(1) Sometimes'=1")
data$H1FS4<-car::recode(data$H1FS4,"'(1) Sometimes'=2") #
data$H1FS5<-car::recode(data$H1FS5,"'(1) Sometimes'=1")
data$H1FS6<-car::recode(data$H1FS6,"'(1) Sometimes'=1")
data$H1FS7<-car::recode(data$H1FS7,"'(1) Sometimes'=1")
data$H1FS8<-car::recode(data$H1FS8,"'(1) Sometimes'=2") #
data$H1FS9<-car::recode(data$H1FS9,"'(1) Sometimes'=1")

data$H1FS1<-car::recode(data$H1FS1,"'(2) A lot of the time'=2")
data$H1FS2<-car::recode(data$H1FS2,"'(2) A lot of the time'=2")
data$H1FS3<-car::recode(data$H1FS3,"'(2) A lot of the time'=2")
data$H1FS4<-car::recode(data$H1FS4,"'(2) A lot of the time'=1") #
data$H1FS5<-car::recode(data$H1FS5,"'(2) A lot of the time'=2")
data$H1FS6<-car::recode(data$H1FS6,"'(2) A lot of the time'=2")
data$H1FS7<-car::recode(data$H1FS7,"'(2) A lot of the time'=2")
data$H1FS8<-car::recode(data$H1FS8,"'(2) A lot of the time'=1") #
data$H1FS9<-car::recode(data$H1FS9,"'(2) A lot of the time'=2")

data$H1FS1<-car::recode(data$H1FS1,"'(3) Most/all of the time'=3")
data$H1FS2<-car::recode(data$H1FS2,"'(3) Most/all of the time'=3")
data$H1FS3<-car::recode(data$H1FS3,"'(3) Most/all of the time'=3")
data$H1FS4<-car::recode(data$H1FS4,"'(3) Most/all of the time'=0") #
data$H1FS5<-car::recode(data$H1FS5,"'(3) Most/all of the time'=3")
data$H1FS6<-car::recode(data$H1FS6,"'(3) Most/all of the time'=3")
data$H1FS7<-car::recode(data$H1FS7,"'(3) Most/all of the time'=3")
data$H1FS8<-car::recode(data$H1FS8,"'(3) Most/all of the time'=0") #
data$H1FS9<-car::recode(data$H1FS9,"'(3) Most/all of the time'=3")

data$H1FS8[data$H1FS8=="(8) Don't know"] <- NA

#There's probably a faster way to do the above. I did it this way. New skills!

#using this coding method the variables are numbers but R is reading them as factors. need to change to a numeric value in order to run analysis.
data$sex <- as.numeric(data$sex)
#for some reason the as.numeric function doesn't like 0. It turned my gender code fom 0 and 1 to '1' and '2'. The following two lines change them back
data$sex[data$sex==1] <- 0
data$sex[data$sex==2] <- 1

#repeating the above
data$alcoholUse[data$alcoholUse==1] <- 0
data$alcoholUse[data$alcoholUse==2] <- 1
data$alcoholUse[data$alcoholUse==3] <- 2
data$alcoholUse[data$alcoholUse==4] <- 3
data$alcoholUse[data$alcoholUse==5] <- 4
data$alcoholUse[data$alcoholUse==6] <- 5
data$alcoholUse[data$alcoholUse==7] <- 6
data$alcoholUse[data$alcoholUse==8] <- 7
data$H1FS1 <- as.numeric(data$H1FS1)
data$H1FS2 <- as.numeric(data$H1FS2)
data$H1FS3 <- as.numeric(data$H1FS3)
data$H1FS4 <- as.numeric(data$H1FS4)
data$H1FS5 <- as.numeric(data$H1FS5)
data$H1FS6 <- as.numeric(data$H1FS6)
data$H1FS7 <- as.numeric(data$H1FS7)
data$H1FS8 <- as.numeric(data$H1FS8)
data$H1FS9 <- as.numeric(data$H1FS9)

#after learning the faster way to do this without using the car function, I've changed methods. The more you know...
data$H1FS1 <- recode(data$H1FS1, "1"=0, "2"=1, "3"=2, "4"=3)
data$H1FS2 <- recode(data$H1FS2, "1"=0, "2"=1, "3"=2, "4"=3)
data$H1FS3 <- recode(data$H1FS3, "1"=0, "2"=1, "3"=2, "4"=3)
data$H1FS4 <- recode(data$H1FS4, "1"=0, "2"=1, "3"=2, "4"=3)
data$H1FS5 <- recode(data$H1FS5, "1"=0, "2"=1, "3"=2, "4"=3)
data$H1FS6 <- recode(data$H1FS6, "1"=0, "2"=1, "3"=2, "4"=3)
data$H1FS7 <- recode(data$H1FS7, "1"=0, "2"=1, "3"=2, "4"=3)
data$H1FS8 <- recode(data$H1FS8, "1"=0, "2"=1, "3"=2, "4"=3)
data$H1FS9 <- recode(data$H1FS9, "1"=0, "2"=1, "3"=2, "4"=3)

#This is important. I've created an object containing the depression scale items and am asking R to add a new column to my data set with the sum of those values. In particular the na.rm line helps account for the NA values. It will mean that the sum is still calculated as long as among the variables there is at least one number. Otherwise it returns an NA for the entire row.
data$depression <- rowSums(y <- cbind(data$H1FS1,data$H1FS2,data$H1FS3,data$H1FS4,data$H1FS5,data$H1FS6,data$H1FS7,data$H1FS8,data$H1FS9), na.rm = T, dims = 1)

#more recoding
data$H1PF18<-car::recode(data$H1PF18,"'(1) Strongly agree'=0")
data$H1PF19<-car::recode(data$H1PF19,"'(1) Strongly agree'=0")
data$H1PF20<-car::recode(data$H1PF20,"'(1) Strongly agree'=0")
data$H1PF21<-car::recode(data$H1PF21,"'(1) Strongly agree'=0")

data$H1PF18<-car::recode(data$H1PF18,"'(2) Agree'=1")
data$H1PF19<-car::recode(data$H1PF19,"'(2) Agree'=1")
data$H1PF20<-car::recode(data$H1PF20,"'(2) Agree'=1")
data$H1PF21<-car::recode(data$H1PF21,"'(2) Agree'=1")

data$H1PF18<-car::recode(data$H1PF18,"'(3) Neither agree nor disagree'=2")
data$H1PF19<-car::recode(data$H1PF19,"'(3) Neither agree nor disagree'=2")
data$H1PF20<-car::recode(data$H1PF20,"'(3) Neither agree nor disagree'=2")
data$H1PF21<-car::recode(data$H1PF21,"'(3) Neither agree nor disagree'=2")

data$H1PF18<-car::recode(data$H1PF18,"'(4) Disagree'=3")
data$H1PF19<-car::recode(data$H1PF19,"'(4) Disagree'=3")
data$H1PF20<-car::recode(data$H1PF20,"'(4) Disagree'=3")
data$H1PF21<-car::recode(data$H1PF21,"'(4) Disagree'=3")

data$H1PF18<-car::recode(data$H1PF18,"'(5) Strongly disagree'=4")
data$H1PF19<-car::recode(data$H1PF19,"'(5) Strongly disagree'=4")
data$H1PF20<-car::recode(data$H1PF20,"'(5) Strongly disagree'=4")
data$H1PF21<-car::recode(data$H1PF21,"'(5) Strongly disagree'=4")

data$H1PF18[data$H1PF18=="(8) Don't know"] <- NA
data$H1PF19[data$H1PF19=="(8) Don't know"] <- NA
data$H1PF20[data$H1PF20=="(8) Don't know"] <- NA
data$H1PF21[data$H1PF21=="(8) Don't know"] <- NA

data$H1PF18 <- as.numeric(data$H1PF18)
data$H1PF19 <- as.numeric(data$H1PF19)
data$H1PF20 <- as.numeric(data$H1PF20)
data$H1PF21 <- as.numeric(data$H1PF21)

data$H1PF18 <- recode(data$H1PF18, "1"=0, "2"=1, "3"=2, "4"=3, "5"=4)
data$H1PF19 <- recode(data$H1PF19, "1"=0, "2"=1, "3"=2, "4"=3, "5"=4)
data$H1PF20 <- recode(data$H1PF20, "1"=0, "2"=1, "3"=2, "4"=3, "5"=4)
data$H1PF21 <- recode(data$H1PF21, "1"=0, "2"=1, "3"=2, "4"=3, "5"=4)

data$planfulness <- rowMeans(x <- cbind(data$H1PF18,data$H1PF19,data$H1PF20,data$H1PF21), na.rm = T, dims = 1)

#This pulls the scales I've created from my data set into their own objects. Don't need to do this but it makes analysis easier to type
DEP <- data$depression
aUSE <- data$alcoholUse
PLAN <- data$planfulness
SEX <- data$sex

########### This is a deviation. This is how I found out that the way I had NA coded before wasn't working when I tried to run correlations and other tests.
lm(DEP ~ PLAN)
lm(scale(DEP) ~ scale(PLAN))
test <- lm(DEP ~ PLAN)
lm.beta(test) #this is a function intoduced by Connor... more on that later

mean(scale(DEP, scale = T))
mean(scale(PLAN, scale = T))
mean(scale(PLAN, scale = T), na.rm = T)

cor(DEP, PLAN, use = "complete.obs") #the complete.obs part of this line is how you run a correlation while accounting for the missing data coded as NA. If you don't do this it won't let you run a correlation on variables that have missing data.
###########


####### This is another deviation. It's an explanation for how lm.beta works. It was introduced as a way to add standardized coeffieints to your model summary. The only problem (I discovered while working with Yuichi, Connor, and Max) is that it handles missing data in a funky way. It makes listwise exemptions before calculating means and standard deviations which means that the way means and standard deviations are calculated in the regression equation are different from when you use scale(). TL:DR Yuichi suggests you use scale() if you have missing data.

x <- c(1,3,2,NA,5)
y <- c(4,2,NA,4,1)

xory.is.na <- is.na(x) | is.na(y)
good.x <- x[!xory.is.na]
good.y <- y[!xory.is.na]

# (1) Below is what happens if we scale using all available data
lm(scale(y) ~ scale(x))
plot(scale(x),scale(y))

# (2) Below shows what happens if we scale only those data poits non-missing for both x and y
lm(scale(good.y) ~ scale(good.x))
plot(scale(good.x),scale(good.y))

# The results below matches (2), not (1).
lm1 <- lm(y ~ x)
lm.beta(lm1)

#########End tangent##########

##creating the models for the analysis using an if then argument. This way I can get either standardized or unstandardized output by changing the T value of 'standardize' in the first line

standardize = F

if(standardize) {
  
  depSex <- lm(scale(DEP) ~ scale(SEX))
  depAlcohol <- lm(scale(DEP) ~ scale(aUSE))
  depSexAlcohol <- lm(scale(DEP) ~ scale(SEX) + scale(aUSE))
  depSexAlcoholPlan <- lm(scale(DEP) ~ scale(SEX) + scale(aUSE) + scale(PLAN))
  depPlan <- lm(scale(DEP) ~ scale(PLAN))

  #m1 <- lm(DEP ~ SEX)
  #m1.5 <- lm(DEP ~ aUSE)
  #m2 <- lm(DEP ~ SEX + aUSE)
  #m3 <- lm(DEP ~ SEX + aUSE + PLAN)
  #m4 <- lm(DEP ~ PLAN)
  
  #depSex <- lm.beta(m1)
  #depAlcohol <- lm.beta(m1.5)
  #depSexAlcohol <- lm.beta(m2)
  #depSexAlcoholPlan <- lm.beta(m3)
  #depPlan <- lm.beta(m4)
  
} else {
  
#depSex <- lm.beta(m1)
#depAlcohol <- lm.beta(m1.5)
#depSexAlcohol <- lm.beta(m2)
#depSexAlcoholPlan <- lm.beta(m3)
#depPlan <- lm.beta(m4)

  depSex <- lm(DEP ~ SEX)
  depAlcohol <- lm(DEP ~ aUSE)
  depSexAlcohol <- lm(DEP ~ SEX + aUSE)
  depSexAlcoholPlan <- lm(DEP ~ SEX + aUSE + PLAN)
  depPlan <- lm(DEP ~ PLAN)
  
}

summary(depSex)
summary(depAlcohol)
summary(depSexAlcohol)
summary(depSexAlcoholPlan)
summary(depPlan)


confint(depSex)
confint(depAlcohol)
confint(depSexAlcohol)
confint(depSexAlcoholPlan)
confint(depPlan)

#After looking at these statistics something becomes apparent. Though Alcohol Use doen't predict depression, it does seem to be a confounding variable for planning. 

#In this model planingfulness significantly predicts depression
summary(lm(DEP ~ SEX + PLAN))

#however, in this model planingfulness is not significant. Suggesting that planingfulness is confounded/mediated by alcohol use...
summary(lm(DEP ~ aUSE + PLAN))

#What we see is that there's a significant interaction between alcohol use and planning
depAlcoholPlan.cen<-lm(DEP~scale(aUSE,scale=F) +
                         scale(PLAN,scale=F) +
                         scale(aUSE,scale=F)*scale(PLAN,scale=F), data)
pander(summary(depAlcoholPlan.cen))

#Looking at both interactions
depAlcoholPlan.cen<-lm(DEP~scale(aUSE,scale=F) +
                         SEX +
                         scale(PLAN,scale=F) +
                         scale(aUSE,scale=F)*scale(PLAN,scale=F) +
                         SEX*scale(PLAN,scale=F), data)
pander(summary(depAlcoholPlan.cen))

#Difficult explore with my current level of knowledge but, there's something at least.

#for ease of use I'm grabbing the r squared values and putting them in their own data sets
m1r2 <- summary(depSex)$r.squared
m1.5r2 <- summary(depAlcohol)$r.squared
m2r2 <- summary(depSexAlcohol)$r.squared
m3r2 <- summary(depSexAlcoholPlan)$r.squared
m4r2 <- summary(depPlan)$r.squared

(m3r2 - m2r2)*100 #looking to see what proportion of the variance in y is accounted for by x1 above and beyond the variance explained by x2 and x3
(m4r2 - m3r2)*100 

#anova(depSexAlcohol, depSexAlcoholPlan) #is the above difference significant

#############Onto the actual graphing######

#inital graph looking at the regression of depression onto planfulness not controlling for any other variables. This will show up on the final graph as a blue dotted line.
ggplot(data = data, aes(x=planfulness, y= depression)) +
  geom_point(size = .4, alpha = .5) +
  geom_smooth(method = "lm", se = F) +
  # geom_abline(slope = lm1$coefficients["apt"],intercept = lm1$coefficients["(Intercept)"]) + #hint
  xlim(min(data$planfulness), max(data$planfulness)) +
  ylim(min(data$depression), max(data$depression)) +
  theme_bw()
  
#############

xi<-seq(min(data$planfulness, na.rm = T),max(data$planfulness, na.rm = T),.2) #this line creates a variable called xi that is a sequence of numbers beginning with the lowest planfulness value and ending at the highest planfulness value. The na.rm = T line is important because I have missing value

preds<-matrix(NA,length(xi),3) #this creates an object that is a matrix with 3 columns, the lenth of my xi variable, and filled with NA values. It's a placeholder for data I'll generate later
preds<-as.data.frame(preds) #this reclassifies the object as a dataframe
names(preds)<-c("ypred","lower","upper") #this names the columns

#preds2<-preds #this is useful for the commented out section below, otherwise it's not necessary

for (i in 1:length(xi)){
  
  data$planfulness.temp<-data$planfulness - xi[i] #this creates an object in the data set that is the     result of subtracting the first value of xi from each value in planfulness. It's centering the       predictor on it's lowest value

  lm<-lm(depression ~ planfulness.temp + alcoholUse + sex, data=data) #regression of depression onto     my centered predictor variable. This will allow me to see the effect of x1 on y controlling for      x2 and x3 later
  
  preds[i,"ypred"]<-lm$coefficients["(Intercept)"] #adds y predicted for each value in preds
  preds[i,"lower"]<-confint(lm)["(Intercept)","2.5 %"] #adds lower confidence interval
  preds[i,"upper"]<-confint(lm)["(Intercept)","97.5 %"] #adds upper confidence interval
  
}

(baseplot<-ggplot() +
    #We can plot our points as points...
    # geom_point(aes(x=xi,preds$ypred)) +
    ggtitle("The Effect of Planfulness on Depression") +
    #But we can easily just say we want a line instead.
    geom_line(aes(x=xi,preds$ypred)) +
    
    #Keep axes limits the same as previous graph to facilitate comparison:
    xlim(min(data$planfulness), max(data$planfulness)) +
    ylim(min(data$depression), max(data$depression)) +
    labs(x="Planfulness",y="Depression") +
    theme(plot.title = element_text(hjust = 0.5))
)

#What about those "lower" and "upper" values?
(predsplot<-baseplot + 
    
    geom_point(aes(x=xi,preds$lower), size = .5) +
    geom_point(aes(x=xi,preds$upper), size = .5)
)

#What if I don't want points?
(predsplot<-baseplot + 
    
    geom_ribbon(aes(x=xi,ymin=preds$lower, ymax=preds$upper),alpha = .25)
)

#full plot adds everything together which shows the effect of x1 on y controlling for the other variables. This also shows the confidence intervals but I've turned the standard error lines off (se = F) because I don't need them for this analysis.
(fullplot<-predsplot +
    geom_line(aes(x=xi,preds$ypred)) +
    geom_point(data = data, aes(x=planfulness, y= depression),size = .4, alpha = .5) +
    geom_smooth(data = data, aes(x=planfulness, y= depression),method = "lm", se = F, linetype = "dashed", size = .5)
)

#The black line is the best fit line of depression regressed onto planfulness controlling for alcohol use and sex. The blue line is depression regressed onto planfulness without controlling for the other variables.

cor(data, use = "complete.obs")
require("Hmisc")
rcorr(as.matrix(data))

data.cor <- data.frame(DEP, SEX, aUSE, PLAN)
rcorr(as.matrix(data.cor))
