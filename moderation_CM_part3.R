# setwd("~/Dropbox/Regression_2018_teaching/Moderation")

#This is a user-defined mean centering function. I will use this later.
center <- function(MOD) {
  centered <- MOD - mean(MOD,na.rm=T)
  return(centered)
}

#Below is a function for probing interactions at low levels of a moderator by centering a the variable at one standard deviation above the mean.
probe.low<-function(MOD) {
  low <- (MOD - mean(MOD,na.rm=T)) + sd(MOD, na.rm=T)
  return(low)
}

#Below is a function for probing interactions at high levels of a moderator by centering a the variable at one standard deviation below the mean.
probe.high <- function(MOD) {
  high <- (MOD - mean(MOD, na.rm=T)) - sd(MOD, na.rm=T)
  return(high)
}

#functions to probe at more extreme levels of my moderator
probe.2sdlow<-function(MOD) {
  low <- (MOD - mean(MOD,na.rm=T)) + 2*sd(MOD, na.rm=T)
  return(low)
}

probe.2.4sdlow<-function(MOD) {
  low <- (MOD - mean(MOD,na.rm=T)) + 2.4*sd(MOD, na.rm=T)
  return(low)
}

#Below is a function for probing interactions at high levels of a moderator by centering a the variable at one standard deviation below the mean.
probe.2sdhigh <- function(MOD) {
  high <- (MOD - mean(MOD, na.rm=T)) - 2*sd(MOD, na.rm=T)
  return(high)
}

df<-read.csv("https://raw.githubusercontent.com/connorjmccabe/InterActive/master/Simulated%20Data/ex1_n150_AMPPS.csv")

names(df)

# names(df)<-c("ss","pre","alccon")

lm.raw<-lm(y ~ X + Z + X:Z, df)
lm.centered<-lm(y ~ center(X) + center(Z) + center(X):center(Z), df)
lm.zlow1sd<-lm(y ~ center(X) + probe.low(Z) + center(X):probe.low(Z), df)
lm.zhigh1sd<-lm(y ~ center(X) + probe.high(Z) + center(X):probe.high(Z), df)
lm.zlow2sd<-lm(y ~ center(X) + probe.2sdlow(Z) + center(X):probe.2sdlow(Z), df)
lm.zlow2.4sd<-lm(y ~ center(X) + probe.2.4sdlow(Z) + center(X):probe.2.4sdlow(Z), df)
lm.zhigh2sd<-lm(y ~ center(X) + probe.2sdhigh(Z) + center(X):probe.2sdhigh(Z), df)

(t1.raw<-cbind(c(0,lm.beta(lm.raw)),summary(lm.raw)$coefficients[,1:2],confint(lm.raw), summary(lm.raw)$coefficients[,3:4]))

(t1.centered<-cbind(c(0,lm.beta(lm.centered)),summary(lm.centered)$coefficients[,1:2],confint(lm.centered), summary(lm.centered)$coefficients[,3:4]))

(t1.zlow1sd<-cbind(c(0,lm.beta(lm.zlow1sd)),summary(lm.zlow1sd)$coefficients[,1:2],confint(lm.zlow1sd), summary(lm.zlow1sd)$coefficients[,3:4]))

(t1.zhigh1sd<-cbind(c(0,lm.beta(lm.zhigh1sd)),summary(lm.zhigh1sd)$coefficients[,1:2],confint(lm.zhigh1sd), summary(lm.zhigh1sd)$coefficients[,3:4]))

(t1.zlow2sd<-cbind(c(0,lm.beta(lm.zlow2sd)),summary(lm.zlow2sd)$coefficients[,1:2],confint(lm.zlow2sd), summary(lm.zlow2sd)$coefficients[,3:4]))

(t1.zlow2.4sd<-cbind(c(0,lm.beta(lm.zlow2.4sd)),summary(lm.zlow2.4sd)$coefficients[,1:2],confint(lm.zlow2.4sd), summary(lm.zlow2.4sd)$coefficients[,3:4]))

(t1.zhigh2sd<-cbind(c(0,lm.beta(lm.zhigh2sd)),summary(lm.zhigh2sd)$coefficients[,1:2],confint(lm.zhigh2sd), summary(lm.zhigh2sd)$coefficients[,3:4]))

write.csv(round(t1.raw,3),"t1.raw.csv")
write.csv(round(t1.centered,3),"t1.centered.csv")
write.csv(round(t1.zlow1sd,3),"t1.zlow1sd.csv")
write.csv(round(t1.zhigh1sd,3),"t1.zhigh1sd.csv")
write.csv(round(t1.zlow2sd,3),"t1.zlow2sd.csv")
write.csv(round(t1.zhigh2sd,3),"t1.zhigh2sd.csv")

write.csv(round(t1.zlow2.4sd,3),"t1.zlow2.4sd.csv")
  