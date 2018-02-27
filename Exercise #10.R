#Terrence Pope
#Exercise #10

N <- 100
age        <- runif(n=N,min=18,max=35)
genderMale <- round(rnorm(n=N))
friends    <- runif(n=N,min=5,max=20)
pets       <- runif(n=N,min=0,max=10)
happiness  <- (6 - 0.11*age - 0.14*genderMale + 0.17*friends + 0.36*pets
               + rnorm(n=N,sd=10))

standardize <- T

if(standardize) {
m1<- lm(scale(happiness) ~ scale(age) + scale(genderMale))
m2<- lm(scale(happiness) ~ scale(age) + scale(genderMale) + scale(friends))
m3<- lm(scale(happiness) ~ scale(age) + scale(genderMale) + scale(friends) + scale(pets))

} else {
m1<- lm(happiness ~ age + genderMale)
m2<- lm(happiness ~ age + genderMale + friends)
m3<- lm(happiness ~ age + genderMale + friends + pets)
}


summary(m1)
summary(m2)
summary(m3)

anova(m2, m3)