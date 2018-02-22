# In light of the wildly varying formats people use for reporting results of hierarchical regression in published papers, we (YS, CM, and MH) came up with a format that we'd like to recommend. Please follow the format in this table: Hierarchical regression predicting happiness_v3.docx

# Below is an example R code that would produce the numbers you need to make this table. Please run the code (without set.seed(), so everyone gets a somewhat different result), and revise the table, using your results. Please submit your R output and a revised table that reflects the results of your analysis.

# First, create a data set
N <- 100
age <- runif(n=N,min=18,max=35)
genderMale <- round(rnorm(n=N))
friends <- runif(n=N,min=5,max=20)
pets <- runif(n=N,min=0,max=10)
happiness <- (6 - 0.11*age - 0.14*genderMale + 0.17*friends + 0.36*pets + rnorm(n=N,sd=10))

# Let's analyze the data
standardize <- TRUE

if (standardize) {
  m1 <- scale(happiness) ~ scale(age) + scale(genderMale)
  m2 <- scale(happiness) ~ scale(age) + scale(genderMale) + scale(friends)
  m3 <- scale(happiness) ~ scale(age) + scale(genderMale) + scale(friends) + scale(pets)
} else {
  m1 <- happiness ~ age + genderMale
  m2 <- happiness ~ age + genderMale + friends
  m3 <- happiness ~ age + genderMale + friends + pets
}

# now analyze, and save the results
lm.m1 <- lm(m1)
lm.m2 <- lm(m2)
lm.m3 <- lm(m3)

# get the statistics out of the saved results
summary(lm.m1)
confint(lm.m1)

summary(lm.m2)
confint(lm.m2)

summary(lm.m3)
confint(lm.m3)

summary(lm.m2)$r.squared - summary(lm.m1)$r.squared
summary(lm.m3)$r.squared - summary(lm.m2)$r.squared

anova(lm.m1, lm.m2, lm.m3)

# For a list of helpful functions you can apply to lm() output objects,
# see: https://www.statmethods.net/stats/regression.html
# for example:
# summary()
# confint()
# coefficients()
# fitted()
# residuals()
# anova()