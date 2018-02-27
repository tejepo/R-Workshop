# Terrence Pope
# Exercise #11

N <- 100
SES <- rnorm(n=N) # I'm using upper case variables names for unobserved ones
PSYCHOPATHOLOGY <- rnorm(n=N)
education  <- 0.9*SES + rnorm(n=N)
occupation <- 0.7*SES + rnorm(n=N)
income     <- 0.5*SES + rnorm(n=N)
depression <- 0.3*PSYCHOPATHOLOGY + rnorm(n=N)
anxiety    <- 0.7*PSYCHOPATHOLOGY + rnorm(n=N)
CP         <- PSYCHOPATHOLOGY - SES + rnorm(n=N) #CP: civic participation

lm.m1 <- lm(CP ~ education + occupation + income)
lm.m2 <- lm(CP ~ education + occupation + income + depression + anxiety)

summary(lm.m1)$r.squared
summary(lm.m2)$r.squared

summary(lm.m1)$r.squared - summary(lm.m2)$r.squared

anova(lm.m1, lm.m2)

lm.m3 <- lm(CP ~ depression + anxiety)
lm.m4 <- lm(CP ~ depression + anxiety + education + occupation + income)

summary(lm.m3)$r.squared
summary(lm.m4)$r.squared

summary(lm.m3)$r.squared - summary(lm.m4)$r.squared
anova(lm.m3, lm.m4)
