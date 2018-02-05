library(psych)
# setting up a population in which gre.q is positively related to gpa
population.N <- 10000 #smallish so my laptop can do this
sample.size.in.each.simulated.study <- 20
r.AC.gre.v <- 0.8
r.AC.gre.q <- 0.5
# true.slope <- 0
# AC below stands for "academic competence"
AC <- rnorm(n=population.N,mean=0,sd=1)
# creating gre.v such that cor(gre.v,AC) = intercorrelation
# In case you're interested, here's the rationale behind the formula I used below:
# First, in general, cor(X,Y) = cov(X,Y)/(sd(X),sd(Y)).
# So to make things simple, let's set up X and Y such that sd(X) = sd(Y) = 1,
# Then cor(X,Y) = cov(X,Y).
#
# Let's say we created Y such that Y = b*X + noise
# then cor(X,Y) = cov(X, b*X + noise) = cov(X,b*X) + cov(X,noise)
#               ~ b*cov(X,X) + 0  # let "~" mean "equals in the long run"
#               = b*var(X,X)
#               = b # because sd(X) = 1.
#
# Q: How large should var(noise) be, so that var(Y) = 1?
# A: 1 - b^2.
#
# So, what we shold do is create Y such that Y = b*X + rnorm(sd=sqrt(1-b^2))
# 
population.gre.v <- r.AC.gre.v*AC + sqrt(1 - r.AC.gre.v^2)*rnorm(n=population.N)
# Let's confirm:
cor(AC, population.gre.v)
population.gre.q <- r.AC.gre.q*AC + sqrt(1 - r.AC.gre.q^2)*rnorm(n=population.N)
cor(AC, population.gre.q)
#
# What's the correlation between gre.v and gre.q?
cor(population.gre.v, population.gre.q)#
# Let's linearly transform gre.v and gre.q so that the variables
# have a mean of 150 and sd of 20. Note cor(gre.v,gre.q) will remain
# the same. (Why?)
population.gre.v <- 150 + 20*population.gre.v
population.gre.q <- 150 + 20*population.gre.q
describe(population.gre.v)
describe(population.gre.q)
cor(population.gre.v, population.gre.q)
#
# Now, let's create the DV, Npubs, let's say it's the N of pre-tenure pubs.
population.Npubs <- 20 + 3*scale(population.gre.v) + 2*scale(population.gre.q) + rnorm(n=population.N)
describe(population.Npubs)
hist(population.Npubs)

# Let's do a study:
participantIDs <- sample(1:length(population.Npubs), size=50) #sampling participants by creating a variable called participantID
sample.gre.v <- population.gre.v[participantIDs]
sample.gre.q <- population.gre.q[participantIDs]
sample.Npubs <- population.Npubs[participantIDs]
describe(sample.gre.v)
describe(sample.gre.q)
describe(sample.Npubs)
plot(x=sample.gre.v, y=sample.Npubs)
plot(x=sample.gre.q, y=sample.Npubs)
