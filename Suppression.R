#Suppresor means "mysterious booster"

set.seed(1)
N <- 1000
TRUE.APTITUDE      <- rnorm(n=N)
TEST.TAKING.SKILLS <- rnorm(n=N,sd=2) #by increasing the variance here you make the measure worse later

aptitude.measure  <- TRUE.APTITUDE + TEST.TAKING.SKILLS + rnorm(n=N)
tt.skills.measure <- TEST.TAKING.SKILLS + rnorm(n=N)
performance       <- TRUE.APTITUDE + rnorm(n=N)

round(cor(performance,cbind(aptitude.measure, tt.skills.measure)),2)

lm.perf.apt       <- lm(performance ~ aptitude.measure)
summary(lm.perf.apt)$r.squared

lm.perf.tts       <- lm(performance ~ tt.skills.measure)
summary(lm.perf.tts)$r.squared

lm.perf.tts.apt   <- lm(performance ~ tt.skills.measure + aptitude.measure)
summary(lm.perf.tts.apt)$r.squared #This is an example of suppresion. It the otherwise unimportant tt.skills.measure increases the total variance accounted for by the first variable.

cor(tt.skills.measure, aptitude.measure) #These two things are strongly correlated even though tt skills is not related to performance in this sample

cat("Adding tt.skills.measure accounts for",
    (summary(lm.perf.tts.apt)$r.squared - summary(lm.perf.apt)$r.squared)*100,
    "additional % of var(performance)\n above and beyond the proportion",
    "of variance accounted for by aptitude.measure alone.\n")

anova(lm.perf.apt, lm.perf.tts.apt)

# Let's gain some insight into the notion of "purifying" aptitude.measure
# by removing the part of it that is related to tt.skills.measure,
# and see how this "purified" version predicts performance.

apt.tt.resid <- lm(aptitude.measure ~ tt.skills.measure)$resid #the residuals are the part of the aptitude measure that are not accounted for by tt.skills. It is the same as SEMI-PARTIAL CORRELATION

summary(lm(performance ~ apt.tt.resid))$r.squared
# compare the above with the increase in R2 due to
# aptitude.measure above and beyond tt.skills.measure
summary(lm.perf.tts.apt)$r.squared - summary(lm.perf.tts)$r.squared

#semi-partial correlations have two meanings and this code allows us to look at what they mean. This method is a way to control for/remove the effect of confounding variables among other things.

#Suppresor varibles obscure the true relationship between a predictor and an outcome. When unaccounted for they can lead to type two errors, that is it can make it seem like there is no relationship. It's a kind of blind spot or confound. If you neglect to include these in your regression model then you're not controlling for their effects. As a result, identifying and adding suppresing variables to your multiple regression will allow you to better approximate the true relationship between your predictor and your outcome.
