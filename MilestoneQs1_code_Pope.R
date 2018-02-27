### Milestone Questions 1
### Terrence Pope

rm(list = ls())
require(psych)

setwd("~/Desktop/UW/Winter '18/Linear Models & Data Analysis/525")
milestone <- read.csv("milestone1_data.csv")

# Question 1: Understanding (Unstandardized) Model Results

m1<- exam_score ~ study_time #creating an object containing the regression

lm.milestone<- lm(m1, data = milestone) #creating an object containing the model

summary(lm.milestone) #view the model
plot(milestone, main=paste("The population. r =",round(cor(milestone, method="pearson"),2))) #quick plot of the model with the regression as part of the output

# Question 2: Understanding the Correlation Coefficient
psych::describe(milestone) #grabbing the easy stats
var(milestone$study_time) #variances
var(milestone$exam_score)

mean(lm.milestone$fitted.values) #finding the stats for the predicted scores
sd(lm.milestone$fitted.values)
var(lm.milestone$fitted.values)

mean(lm.milestone$residuals) #finding the stats for the residuals
sd(lm.milestone$residuals)
var(lm.milestone$residuals)

res2<-lm.milestone$residuals^2 #creating an object that contains the squared residuals
mean(res2)

#2b and 2c: quick math calculating the proportion of variance explained and not explained by x
var(lm.milestone$fitted.values)/var(milestone$exam_score)
1-(var(lm.milestone$fitted.values)/var(milestone$exam_score))

#2d: correlation formula
.88*(sd(milestone$study_time)/sd(milestone$exam_score))

#2i creating the plot with the requested parameters
apatheme<-theme_bw()+
  theme(
    panel.grid.major=element_blank(), 
    panel.grid.minor=element_blank(), 
    panel.border=element_blank(), 
    axis.line=element_line(), 
    legend.title=element_blank()
  )

milestoneplot<-function(df, xvar, yvar){
  ggplot(data=df, aes(x=xvar,y=yvar)) +
    geom_point(shape=16, size = 1) +
    ylim(min(yvar,na.rm=T) - sd(yvar,na.rm=T),max(yvar,na.rm=T) + sd(yvar,na.rm=T)) + 
    xlim(min(xvar,na.rm=T) - sd(xvar,na.rm=T),max(xvar,na.rm=T) + sd(xvar,na.rm=T)) + 
    apatheme +
    labs(x = "Hours of Study", y= "Exam \n Scores") +
    ggtitle("Milestone Question 2i", subtitle = "Regression of Exam Score on Study Time")
}

nicescatter<-milestoneplot(df=milestone,xvar=milestone$study_time,yvar=milestone$exam_score) +
  stat_smooth(method = "lm", formula = y ~ x, color = "black", se=T) 

nicescatter
