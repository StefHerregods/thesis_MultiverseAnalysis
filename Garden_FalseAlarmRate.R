# False alarm rate across different garden of forking paths approaches 
# Based on Simmons et al. (2011) - False Positive Psychology


# Set-up ----

library(ggplot2)
library(MASS)

simulations = 100000
pb = txtProgressBar(min = 0, max = simulations, initial = 0, style = 3) 


# From correlation matrix to covariance matrix ----

R <- rbind(c(1,.8,.8,0), c(.8,1,.8,0), c(.8,.8,1,0), c(0,0,0,1))  # Correlation matrix
sd <- c(15, 15, 15, 1)  # Standard deviation vector
sigma <- diag(sd) %*% R %*% diag(sd)  # Covariance matrix
mu <- c(100,100,100,12)  # Mean vector


# Simulate data and compute p values ----

set.seed(0)
p_values <- setNames(as.data.frame(matrix(nrow = simulations, ncol = 3)),
                     c('testA', 'testB', 'testC'))

for (i in 1:simulations){
  df <- as.data.frame(mvrnorm(n=100, mu=mu, Sigma=sigma))
  p <- c(summary(lm(V1 ~ V4, df))$coefficients[2,4], summary(lm(V2 ~ V4, df))$coefficients[2,4], summary(lm(V3 ~ V4, df))$coefficients[2,4])
  p_values[i,] <- p
  
  setTxtProgressBar(pb,i)
}
close(pb)

ggplot(data = p_values, aes(x = testA)) + geom_histogram()
ggplot(data = p_values, aes(x = testB)) + geom_histogram()
ggplot(data = p_values, aes(x = testC)) + geom_histogram()


# Rigorous approach - interpret separately ----

nrow(p_values[p_values$testA < 0.05,]) / nrow(p_values)  # As expected, 5 % false positives 
nrow(p_values[p_values$testB < 0.05,]) / nrow(p_values)  # As expected, 5 % false positives 
nrow(p_values[p_values$testC < 0.05,]) / nrow(p_values)  # As expected, 5 % false positives 


# One significant coefficient is enough ----

nrow(p_values[p_values$testA < 0.05 | p_values$testB < 0.05 | p_values$testC < 0.05,]) / nrow(p_values)  # As expected, 5 % false positives 


# Conservative interpretation ----

nrow(p_values[p_values$testA < 0.05 & p_values$testB < 0.05 & p_values$testC < 0.05,]) / nrow(p_values)  # As expected, 5 % false positives 





