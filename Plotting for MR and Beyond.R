#Plotting linear effects (and confidence regions) for multiple regression


#Let's take our supression example from the other day

set.seed(1)
N <- 500
TRUE.APTITUDE      <- rnorm(n=N)
TEST.TAKING.SKILLS <- rnorm(n=N,sd=2)

# tt.skills.measure <- 5 + TEST.TAKING.SKILLS + rnorm(n=N)
aptitude.measure  <- TRUE.APTITUDE + TEST.TAKING.SKILLS + rnorm(n=N)
performance       <- TRUE.APTITUDE + .5*(TRUE.APTITUDE^2)+ rnorm(n=N)

#Run this for "invariant" example
# aptitude.measure  <- .5*TEST.TAKING.SKILLS + TRUE.APTITUDE + rnorm(n=N)
# performance       <- TRUE.APTITUDE + .5*(TRUE.APTITUDE^2)+ rnorm(n=N)

#Run this for "confound" example
# aptitude.measure  <- .5*TEST.TAKING.SKILLS + TRUE.APTITUDE + rnorm(n=N)
# performance       <- .2*TRUE.APTITUDE + .5*TEST.TAKING.SKILLS + rnorm(n=N)

df<-as.data.frame(cbind(aptitude.measure,tt.skills.measure,performance))
names(df)<-c("apt","tt.skills","performance")

lm1<-lm(performance ~ apt + tt.skills, data=df)

#Problem: what does this give me?
require(ggplot2)
ggplot(data = df, aes(x=apt, y= performance)) +
  geom_point(size = .4, alpha = .5) +
  geom_smooth(method = "lm", se = F) +
  # geom_abline(slope = lm1$coefficients["apt"],intercept = lm1$coefficients["(Intercept)"]) + #hint
  xlim(min(df$apt), max(df$apt)) +
  ylim(min(df$performance), max(df$performance)) +
  theme_bw()

#What is wrong with this plot?

#Solution (the "clever-dumb" way)

xi<-seq(min(df$apt),max(df$apt),.2)

preds<-matrix(NA,length(xi),3)
preds<-as.data.frame(preds)
names(preds)<-c("ypred","lower","upper")

preds2<-preds

for (i in 1:length(xi)){

  df$apt.temp<-df$apt - xi[i]
  # lm<-lm(performance~apt.temp + tt.skills, data=df)
  lm<-lm(performance~apt.temp + scale(tt.skills), data=df)
  
  #Test the following line at the end for fun:
  # lm<-lm(performance~apt.temp + tt.skills + I(apt.temp^2), data=df)
  
  preds[i,"ypred"]<-lm$coefficients["(Intercept)"]
  preds[i,"lower"]<-confint(lm)["(Intercept)","2.5 %"]
  preds[i,"upper"]<-confint(lm)["(Intercept)","97.5 %"]
  
  # Run later. What's different about this?
  # lm2<-lm(performance~apt.temp + scale(tt.skills), data=df)
  # preds2[i,"ypred"]<-lm2$coefficients["(Intercept)"]
  # preds2[i,"lower"]<-confint(lm2)["(Intercept)","2.5 %"]
  # preds2[i,"upper"]<-confint(lm2)["(Intercept)","97.5 %"]
}

(baseplot<-ggplot() +
  #We can plot our points as points...
  # geom_point(aes(x=xi,preds$ypred)) +
  
  #But we can easily just say we want a line instead.
  geom_line(aes(x=xi,preds$ypred)) +
  
  # geom_line(aes(x=xi,preds2$ypred), linetype="dotted") +
    
  #Keep axes limits the same as previous graph to facilitate comparison:
  xlim(min(df$apt), max(df$apt)) +
  ylim(min(df$performance), max(df$performance)) +
  labs(x="Aptitude",y="Performance") +
  geom_hline(yintercept = 2.5, linetype = "dashed") +
  annotate(geom = "text", label="Passing",x = 7, y = 1.9) +
  theme_bw()
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

#mystery of the confidence region solved.

#Now, let's pull in the elements from our first plot and compare.

(fullplot<-predsplot +
  geom_point(data = df, aes(x=apt, y= performance),size = .4, alpha = .5) +
  geom_smooth(data = df, aes(x=apt, y= performance),method = "lm", se = T, linetype = "dashed", size = .5)
)

#Question: in what sense is the idea of "suppression" illustrated here?
#Remember that my blue dashed line is the bivariate regression line.

#Question: What would this look like if I had a confounding, rather than suppressor, variable? What about a covariate that did nothing to muck up my results?

#Key points to keep in your heart of hearts:

#-The confidence region corresponds with the confidnece interval of the predicted value of Y across all levels of a given X (holding all other predictors constant).

#-If a reviewer asks the question "How would your effects change if you controlled for X?", you could show them this plot to be more convincing.

#-Clever code can make complex problems easy. Keep in mind the logic of this code. We can use this same logic to:

###-make polynomial plots (spoiler alert: uncomment line 43 to do this)
###-probe interactions with the pick-a-point approach
###-Conduct regions-of significance analysis
###-Extend this to generalized linear models (not covered in this class)
