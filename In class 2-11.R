N <- 100
age        <- runif(n=N,min=18,max=35)
genderMale <- round(rnorm(n=N))
friends    <- runif(n=N,min=5,max=20)
pets       <- runif(n=N,min=0,max=10)
happiness  <- (6 - 0.11*age - 0.14*genderMale + 0.17*friends + 0.36*pets
               + rnorm(n=N,sd=10))


set.seed(1)
N <- 100
SES <- rnorm(n=N) # I'm using upper case variables names for unobserved ones
education  <- SES + rnorm(n=N)
occupation <- SES + rnorm(n=N)
income     <- SES + rnorm(n=N)
DV         <- 0.3*education + 0.4*occupation + 0.5*income + rnorm(n=N,sd=2)

lm.DV.edu         <- lm(DV ~ education) #bi-ravariate
lm.DV.edu.occ     <- lm(DV ~ education + occupation) #Multiple regression. 
lm.DV.edu.occ.inc <- lm(DV ~ education + occupation + income)

summary(lm.DV.edu)$r.squared
summary(lm.DV.edu.occ)$r.squared
summary(lm.DV.edu.occ.inc)$r.squared

anova(lm.DV.edu, lm.DV.edu.occ, lm.DV.edu.occ.inc)
# First row is a starting point where you compare other groups to it
# The second row is the contribution of each unique variable to variance in the DV
