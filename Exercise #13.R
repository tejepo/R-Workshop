#Assignment #13
#Terrence Pope

set.seed(1)
N <- 1000
TRUE.APTITUDE      <- rnorm(n=N)
TEST.TAKING.SKILLS <- rnorm(n=N)

aptitude.measure  <- TRUE.APTITUDE + TEST.TAKING.SKILLS + rnorm(n=N)
tt.skills.measure <- TEST.TAKING.SKILLS + rnorm(n=N)
performance       <- TRUE.APTITUDE + rnorm(n=N)

round(cor(performance,cbind(aptitude.measure, tt.skills.measure)),2) 

cor(aptitude.measure, tt.skills.measure)

lm.perf.apt       <- lm(performance ~ aptitude.measure) 
summary(lm.perf.apt)$r.squared

lm.perf.tts       <- lm(performance ~ tt.skills.measure)
summary(lm.perf.tts)$r.squared

lm.perf.apt.tts <- lm(performance ~ aptitude.measure + tt.skills.measure)
summary(lm.perf.apt.tts)$r.squared

anova(lm.perf.apt, lm.perf.apt.tts)

(summary(lm.perf.apt.tts)$r.squared - summary(lm.perf.tts)$r.squared)*100

#From what I'm able to gather, the aptitude measure is highly correlated with the test taking skills measure. As a result, variance in aptitude that is due to test taking skills is suppresing the relationships between aptitude and performance. Initial analysis suggests that aptitude accounts for 15% of the variance in perfomance. However, when measuring the variance in performance due to aptitude while controlling for the variance in aptitude due to test taking skills aptitude accounts for 18% of the variance in performance. This difference is significant at the p<.001 level.
