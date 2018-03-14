# Terrence Pope
# MR With Product Terms

rm(list=ls())

set.seed(5)

N <- 10000
G <- runif(n=N,0,1) > 0.5
G <- as.numeric(G)
H <- runif(n=N,0,10)
S <- 60 - 5*H - 25*G + 5*H*G + rnorm(n=N,0,5)

# (1) Regress S on H, G, and H*G. What are the coefficients, and p-values for them?

summary(lm(S ~ H + G + H*G))

# Variable   Coefficient   P-value 
# H          -4.99         <.001
# G          -25.03        <.001
# H:G        4.99          <.001

# (2) Recode G, such that if the original G is 1, now it's 0, and vice versa.

G <- 1-G 

# (3) regress S on H, G, and H*G. This is the same as (1) except that we're now using the recoded G.

summary(lm(S ~ H + G + H*G))

# Variable   Coefficient   P-value 
# H          -0.00         .97
# G          25.03         <.001
# H:G        -4.99         <.001

# (4) Describe what changed, and didn't change, from the result from (1).

# First off the sign of the G and H:G coefficients are reversed. Similarly for these two variables the t value has a reversed sign. The absolute values for all of the above are the same however and the p value has not changed. 

#On the other hand the intercept coefficient has shifted as has the coefficient of the variable H. Perhaps as a result the p value has shifted as well (though it looks like the intercept p value is virtually the same. I assume that this is incidental).

# (5) What is the meaning of the coefficient for H now? What was the meaning of the coefficient for H before recoding?

# In the latter equation the coefficient for H represents the the effect of H on S when G = 0. The important thing to remember is that in the latter example what used to be G = 1 is now G = 0. Before recoding we were seeing the effect of H on S when the G truly equaled 0. After recoding we see the effect of H on S for what used to be G = 1.

# (6) Center H. Run the regression above. What is the meaning of the coefficient for G now?

H <- H - mean(H)

summary(lm(S ~ H + G + H*G))

#The meaning of the coefficient for G is the change in stress between G = 0 and G = 1 when participants are at the average of H. To use the example from in class, it would be the change in stress between graduate students and non-graduate students who get an average amount of sleep