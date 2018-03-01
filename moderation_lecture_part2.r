require(QuantPsyc)
require(dplyr)

df<-read.csv("https://raw.githubusercontent.com/connorjmccabe/Regression_523.525_2018/master/salary_ex.csv")

#change variable names to lowercase
names(df)<-tolower(names(df))

#Define a version of department 
df$departfac<-recode(df$depart,"1"="psych","2"="soc","3"="hist")

df$psych<-recode(df$depart, "1"=1,"2"=0,"3"=0)
df$soc<-recode(df$depart, "1"=0,"2"=1,"3"=0)
df$hist<-recode(df$depart, "1"=0,"2"=0,"3"=1)

m2<-salary ~ psych + pub + psych:pub
lm2<-lm(m2,data=df)

(table1.psych1<-cbind(c(0,lm.beta(lm2)),confint(lm2),summary(lm2)$coefficients))
# write.csv(table1.psych1,"/Users/cmccabe/Dropbox/Regression_2018_teaching/Moderation/table1.psych1.csv")


#Let's reverse the coding scheme of my psych variable.

df$nopsych<-recode(df$psych,"0"=1,"1"=0)

m3<-salary ~ nopsych + pub + nopsych:pub
lm3<-lm(m3,data=df)

(table2.nopsych1<-cbind(c(0,lm.beta(lm3)),confint(lm3),summary(lm3)$coefficients))
# write.csv(table2.nopsych1,"/Users/cmccabe/Dropbox/Regression_2018_teaching/Moderation/table2.nopsych1.csv")


center<-function(MOD)
{
  centered<-MOD-mean(MOD,na.rm=TRUE)
  return(centered)
}

m4 <- salary ~ psych + center(pub) + psych:center(pub)
lm4<-lm(m4,data=df)

(table3.psych1.cpub<-cbind(c(0,lm.beta(lm4)),confint(lm4),summary(lm4)$coefficients))
# write.csv(table3.psych1.cpub,"/Users/cmccabe/Dropbox/Regression_2018_teaching/Moderation/table1.psych1.cpub.csv")


m5 <- salary ~ nopsych + center(pub) + nopsych:center(pub)
lm5<-lm(m5,data=df)

(table4.nopsych1.cpub<-cbind(c(0,lm.beta(lm5)),confint(lm5),summary(lm5)$coefficients))
# write.csv(table4.nopsych1.cpub,"/Users/cmccabe/Dropbox/Regression_2018_teaching/Moderation/table4.nopsych1.cpub.csv")

