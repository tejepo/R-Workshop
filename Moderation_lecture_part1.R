#First, let's read in the data we'll be using today.
df<-read.csv("/Users/cmccabe/Dropbox/Regression_2018_teaching/Moderation/salary_ex.csv")

#change variable names to lowercase
names(df)<-tolower(names(df))

#Define a version of department 
df$departfac<-recode(df$depart,"1"="psych","2"="soc","3"="hist")

df$psych<-recode(df$depart, "1"=1,"2"=0,"3"=0)
df$soc<-recode(df$depart, "1"=0,"2"=1,"3"=0)
df$hist<-recode(df$depart, "1"=0,"2"=0,"3"=1)

# write.csv(df,"/Users/cmccabe/Dropbox/Regression_2018_teaching/Moderation/salary_ex.v2.csv")
psych::describe(df)

#Intuitively, If I'm interested in testing whether the effect for publications on salary differs for psychology versus other departments, i might just split my data and look at the coefficients, like this:
m1<-salary~pub #regressing salary on publications
df.psych<-df[which(df$psych==1),] #define a dataset for psychology ONLY
df.other<-df[which(df$psych==0),] #define a dataset for non-psychology ONLY

#estimate the effect of pub on salary for psych
(salary.psych<-lm(m1,data=df.psych)) 

#estimate the effect of pub on salary for non-psych
(salary.other<-lm(m1,data=df.other))

#Does this tell me that there is an interaction present?
#What are the problems with doing this?

summary(salary.psych)
summary(salary.other)

confint(salary.psych)
confint(salary.other)

#Here, I test the interaction instead. What is the same/different here?
#What is my pub coefficient telling me?

m2<-salary ~ psych + pub + psych:pub
(lm2<-lm(m2,data=df))

#let's compare the summary table for each of our approaches. what is different (hint: there are at least 4 (related) differences)? what's the same?
summary(lm2)
summary(salary.other)

#Let's reverse the coding scheme of my psych variable.

df$nopsych<-recode(df$psych,"0"=1,"1"=0)

m3<-salary ~ nopsych + pub + nopsych:pub
(lm3<-lm(m3,data=df))

#Same questions as above. What is different (hint: there are at least 4 (related) differences)? what's the same?
summary(lm3)
summary(salary.psych)

#If time: flip to interActive to replicate et al.