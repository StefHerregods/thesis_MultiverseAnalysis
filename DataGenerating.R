# Simulate data for illustration multiverse analysis


# Set-up ----

library(ggplot2)
library(MASS)

windowsFonts(font = windowsFont("Times New Roman"))

strength = .34  # predictor ~ outcome variable covariance


# From correlation matrix to covariance matrix ----

set.seed(1)
R <- rbind(c(1,.9,strength,strength,.1,.15), c(.9,1,strength,strength,.1,.15), c(strength,strength,1,.85,.05,.15), c(strength,strength,.85,1,.05,.15), c(.1,.1,.05,.05,1,.1), c(.15,.15,.15,.15,.1,1))  # Correlation matrix
sd <- c(15, 20, 3, 7, 10, 11)  # Standard deviation vector
sigma <- diag(sd) %*% R %*% diag(sd)  # Covariance matrix
mu <- c(10,30,20,0,15,15)  # Mean vector

# Create data set ----

df <- as.data.frame(mvrnorm(n=100, mu=mu, Sigma=sigma))
names(df) <- c('outcome_1', 'outcome_2', 'predictor_1', 'predictor_2', 'confounding_1', 'confounding_2')
df$predictor_3 <- df$predictor_1 + df$predictor_2
df_1 <- df
df_1$outcome <- df_1$outcome_1
df_2 <- df
df_2$outcome <- df_2$outcome_2

# Outlier removal ----

Q1 <- quantile(df_1$outcome, .25)  # IQR outcome_1
Q3 <- quantile(df_1$outcome, .75)
IQR <- IQR(df_1$outcome)
df_3 <- df_1[df_1$outcome > (Q1 - 1.5*IQR) & df_1$outcome < (Q3 + 1.5*IQR),]
nrow(df_3) 

Q1 <- quantile(df_2$outcome, .25)  # IQR outcome_2
Q3 <- quantile(df_2$outcome, .75)
IQR <- IQR(df_2$outcome)
df_4 <- df_2[df_2$outcome > (Q1 - 1.5*IQR) & df_2$outcome < (Q3 + 1.5*IQR),]
nrow(df_4) 

# Create multiverse ----

outlier_detection <- c('none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR')
predictor_variable <- c('1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum')
confounding_variable <- c('none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction')
outcome_variable <- c('1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2')

## p_values ----

### df_1 - no outlier removal, outcome_1

p_values <- c(
summary(lm(outcome ~ predictor_1, df_1))$coefficients[2,4],
summary(lm(outcome ~ predictor_2, df_1))$coefficients[2,4],
summary(lm(outcome ~ predictor_3, df_1))$coefficients[2,4],

summary(lm(outcome ~ predictor_1 + confounding_1, df_1))$coefficients[2,4],
summary(lm(outcome ~ predictor_2 + confounding_1, df_1))$coefficients[2,4],
summary(lm(outcome ~ predictor_3 + confounding_1, df_1))$coefficients[2,4],

summary(lm(outcome ~ predictor_1 * confounding_1, df_1))$coefficients[2,4],
summary(lm(outcome ~ predictor_2 * confounding_1, df_1))$coefficients[2,4],
summary(lm(outcome ~ predictor_3 * confounding_1, df_1))$coefficients[2,4],

summary(lm(outcome ~ predictor_1 + confounding_2, df_1))$coefficients[2,4],
summary(lm(outcome ~ predictor_2 + confounding_2, df_1))$coefficients[2,4],
summary(lm(outcome ~ predictor_3 + confounding_2, df_1))$coefficients[2,4],

summary(lm(outcome ~ predictor_1 * confounding_2, df_1))$coefficients[2,4],
summary(lm(outcome ~ predictor_2 * confounding_2, df_1))$coefficients[2,4],
summary(lm(outcome ~ predictor_3 * confounding_2, df_1))$coefficients[2,4],

### df_2 - no outlier removal, outcome_2

summary(lm(outcome ~ predictor_1, df_2))$coefficients[2,4],
summary(lm(outcome ~ predictor_2, df_2))$coefficients[2,4],
summary(lm(outcome ~ predictor_3, df_2))$coefficients[2,4],

summary(lm(outcome ~ predictor_1 + confounding_1, df_2))$coefficients[2,4],
summary(lm(outcome ~ predictor_2 + confounding_1, df_2))$coefficients[2,4],
summary(lm(outcome ~ predictor_3 + confounding_1, df_2))$coefficients[2,4],

summary(lm(outcome ~ predictor_1 * confounding_1, df_2))$coefficients[2,4],
summary(lm(outcome ~ predictor_2 * confounding_1, df_2))$coefficients[2,4],
summary(lm(outcome ~ predictor_3 * confounding_1, df_2))$coefficients[2,4],

summary(lm(outcome ~ predictor_1 + confounding_2, df_2))$coefficients[2,4],
summary(lm(outcome ~ predictor_2 + confounding_2, df_2))$coefficients[2,4],
summary(lm(outcome ~ predictor_3 + confounding_2, df_2))$coefficients[2,4],

summary(lm(outcome ~ predictor_1 * confounding_2, df_2))$coefficients[2,4],
summary(lm(outcome ~ predictor_2 * confounding_2, df_2))$coefficients[2,4],
summary(lm(outcome ~ predictor_3 * confounding_2, df_2))$coefficients[2,4],

### df_3 - outlier removal, outcome_1

summary(lm(outcome ~ predictor_1, df_3))$coefficients[2,4],
summary(lm(outcome ~ predictor_2, df_3))$coefficients[2,4],
summary(lm(outcome ~ predictor_3, df_3))$coefficients[2,4],

summary(lm(outcome ~ predictor_1 + confounding_1, df_3))$coefficients[2,4],
summary(lm(outcome ~ predictor_2 + confounding_1, df_3))$coefficients[2,4],
summary(lm(outcome ~ predictor_3 + confounding_1, df_3))$coefficients[2,4],

summary(lm(outcome ~ predictor_1 * confounding_1, df_3))$coefficients[2,4],
summary(lm(outcome ~ predictor_2 * confounding_1, df_3))$coefficients[2,4],
summary(lm(outcome ~ predictor_3 * confounding_1, df_3))$coefficients[2,4],

summary(lm(outcome ~ predictor_1 + confounding_2, df_3))$coefficients[2,4],
summary(lm(outcome ~ predictor_2 + confounding_2, df_3))$coefficients[2,4],
summary(lm(outcome ~ predictor_3 + confounding_2, df_3))$coefficients[2,4],

summary(lm(outcome ~ predictor_1 * confounding_2, df_3))$coefficients[2,4],
summary(lm(outcome ~ predictor_2 * confounding_2, df_3))$coefficients[2,4],
summary(lm(outcome ~ predictor_3 * confounding_2, df_3))$coefficients[2,4],

### df_4 - outlier removal, outcome_2

summary(lm(outcome ~ predictor_1, df_4))$coefficients[2,4],
summary(lm(outcome ~ predictor_2, df_4))$coefficients[2,4],
summary(lm(outcome ~ predictor_3, df_4))$coefficients[2,4],

summary(lm(outcome ~ predictor_1 + confounding_1, df_4))$coefficients[2,4],
summary(lm(outcome ~ predictor_2 + confounding_1, df_4))$coefficients[2,4],
summary(lm(outcome ~ predictor_3 + confounding_1, df_4))$coefficients[2,4],

summary(lm(outcome ~ predictor_1 * confounding_1, df_4))$coefficients[2,4],
summary(lm(outcome ~ predictor_2 * confounding_1, df_4))$coefficients[2,4],
summary(lm(outcome ~ predictor_3 * confounding_1, df_4))$coefficients[2,4],

summary(lm(outcome ~ predictor_1 + confounding_2, df_4))$coefficients[2,4],
summary(lm(outcome ~ predictor_2 + confounding_2, df_4))$coefficients[2,4],
summary(lm(outcome ~ predictor_3 + confounding_2, df_4))$coefficients[2,4],

summary(lm(outcome ~ predictor_1 * confounding_2, df_4))$coefficients[2,4],
summary(lm(outcome ~ predictor_2 * confounding_2, df_4))$coefficients[2,4],
summary(lm(outcome ~ predictor_3 * confounding_2, df_4))$coefficients[2,4]
)

## Coefficient estimates ----

estimates <- c(
  ### df_1 - no outlier removal, outcome_1
  summary(lm(outcome ~ predictor_1, df_1))$coefficients[2,1],
  summary(lm(outcome ~ predictor_2, df_1))$coefficients[2,1],
  summary(lm(outcome ~ predictor_3, df_1))$coefficients[2,1],
  
  summary(lm(outcome ~ predictor_1 + confounding_1, df_1))$coefficients[2,1],
  summary(lm(outcome ~ predictor_2 + confounding_1, df_1))$coefficients[2,1],
  summary(lm(outcome ~ predictor_3 + confounding_1, df_1))$coefficients[2,1],
  
  summary(lm(outcome ~ predictor_1 * confounding_1, df_1))$coefficients[2,1],
  summary(lm(outcome ~ predictor_2 * confounding_1, df_1))$coefficients[2,1],
  summary(lm(outcome ~ predictor_3 * confounding_1, df_1))$coefficients[2,1],
  
  summary(lm(outcome ~ predictor_1 + confounding_2, df_1))$coefficients[2,1],
  summary(lm(outcome ~ predictor_2 + confounding_2, df_1))$coefficients[2,1],
  summary(lm(outcome ~ predictor_3 + confounding_2, df_1))$coefficients[2,1],
  
  summary(lm(outcome ~ predictor_1 * confounding_2, df_1))$coefficients[2,1],
  summary(lm(outcome ~ predictor_2 * confounding_2, df_1))$coefficients[2,1],
  summary(lm(outcome ~ predictor_3 * confounding_2, df_1))$coefficients[2,1],
  
  ### df_2 - no outlier removal, outcome_2
  summary(lm(outcome ~ predictor_1, df_2))$coefficients[2,1],
  summary(lm(outcome ~ predictor_2, df_2))$coefficients[2,1],
  summary(lm(outcome ~ predictor_3, df_2))$coefficients[2,1],
  
  summary(lm(outcome ~ predictor_1 + confounding_1, df_2))$coefficients[2,1],
  summary(lm(outcome ~ predictor_2 + confounding_1, df_2))$coefficients[2,1],
  summary(lm(outcome ~ predictor_3 + confounding_1, df_2))$coefficients[2,1],
  
  summary(lm(outcome ~ predictor_1 * confounding_1, df_2))$coefficients[2,1],
  summary(lm(outcome ~ predictor_2 * confounding_1, df_2))$coefficients[2,1],
  summary(lm(outcome ~ predictor_3 * confounding_1, df_2))$coefficients[2,1],
  
  summary(lm(outcome ~ predictor_1 + confounding_2, df_2))$coefficients[2,1],
  summary(lm(outcome ~ predictor_2 + confounding_2, df_2))$coefficients[2,1],
  summary(lm(outcome ~ predictor_3 + confounding_2, df_2))$coefficients[2,1],
  
  summary(lm(outcome ~ predictor_1 * confounding_2, df_2))$coefficients[2,1],
  summary(lm(outcome ~ predictor_2 * confounding_2, df_2))$coefficients[2,1],
  summary(lm(outcome ~ predictor_3 * confounding_2, df_2))$coefficients[2,1],
  
  ### df_3 - outlier removal, outcome_1
  summary(lm(outcome ~ predictor_1, df_3))$coefficients[2,1],
  summary(lm(outcome ~ predictor_2, df_3))$coefficients[2,1],
  summary(lm(outcome ~ predictor_3, df_3))$coefficients[2,1],
  
  summary(lm(outcome ~ predictor_1 + confounding_1, df_3))$coefficients[2,1],
  summary(lm(outcome ~ predictor_2 + confounding_1, df_3))$coefficients[2,1],
  summary(lm(outcome ~ predictor_3 + confounding_1, df_3))$coefficients[2,1],
  
  summary(lm(outcome ~ predictor_1 * confounding_1, df_3))$coefficients[2,1],
  summary(lm(outcome ~ predictor_2 * confounding_1, df_3))$coefficients[2,1],
  summary(lm(outcome ~ predictor_3 * confounding_1, df_3))$coefficients[2,1],
  
  summary(lm(outcome ~ predictor_1 + confounding_2, df_3))$coefficients[2,1],
  summary(lm(outcome ~ predictor_2 + confounding_2, df_3))$coefficients[2,1],
  summary(lm(outcome ~ predictor_3 + confounding_2, df_3))$coefficients[2,1],
  
  summary(lm(outcome ~ predictor_1 * confounding_2, df_3))$coefficients[2,1],
  summary(lm(outcome ~ predictor_2 * confounding_2, df_3))$coefficients[2,1],
  summary(lm(outcome ~ predictor_3 * confounding_2, df_3))$coefficients[2,1],
  
  ### df_4 - outlier removal, outcome_2
  summary(lm(outcome ~ predictor_1, df_4))$coefficients[2,1],
  summary(lm(outcome ~ predictor_2, df_4))$coefficients[2,1],
  summary(lm(outcome ~ predictor_3, df_4))$coefficients[2,1],
  
  summary(lm(outcome ~ predictor_1 + confounding_1, df_4))$coefficients[2,1],
  summary(lm(outcome ~ predictor_2 + confounding_1, df_4))$coefficients[2,1],
  summary(lm(outcome ~ predictor_3 + confounding_1, df_4))$coefficients[2,1],
  
  summary(lm(outcome ~ predictor_1 * confounding_1, df_4))$coefficients[2,1],
  summary(lm(outcome ~ predictor_2 * confounding_1, df_4))$coefficients[2,1],
  summary(lm(outcome ~ predictor_3 * confounding_1, df_4))$coefficients[2,1],
  
  summary(lm(outcome ~ predictor_1 + confounding_2, df_4))$coefficients[2,1],
  summary(lm(outcome ~ predictor_2 + confounding_2, df_4))$coefficients[2,1],
  summary(lm(outcome ~ predictor_3 + confounding_2, df_4))$coefficients[2,1],
  
  summary(lm(outcome ~ predictor_1 * confounding_2, df_4))$coefficients[2,1],
  summary(lm(outcome ~ predictor_2 * confounding_2, df_4))$coefficients[2,1],
  summary(lm(outcome ~ predictor_3 * confounding_2, df_4))$coefficients[2,1]
)

## upper limit confidence interval ----

confint_upper <- c(
  
  ### df_1 - no outlier removal, outcome_1
  confint(lm(outcome ~ predictor_1, df_1))[2,2],
  confint(lm(outcome ~ predictor_2, df_1))[2,2],
  confint(lm(outcome ~ predictor_3, df_1))[2,2],
  
  confint(lm(outcome ~ predictor_1 + confounding_1, df_1))[2,2],
  confint(lm(outcome ~ predictor_2 + confounding_1, df_1))[2,2],
  confint(lm(outcome ~ predictor_3 + confounding_1, df_1))[2,2],
  
  confint(lm(outcome ~ predictor_1 * confounding_1, df_1))[2,2],
  confint(lm(outcome ~ predictor_2 * confounding_1, df_1))[2,2],
  confint(lm(outcome ~ predictor_3 * confounding_1, df_1))[2,2],
  
  confint(lm(outcome ~ predictor_1 + confounding_2, df_1))[2,2],
  confint(lm(outcome ~ predictor_2 + confounding_2, df_1))[2,2],
  confint(lm(outcome ~ predictor_3 + confounding_2, df_1))[2,2],
  
  confint(lm(outcome ~ predictor_1 * confounding_2, df_1))[2,2],
  confint(lm(outcome ~ predictor_2 * confounding_2, df_1))[2,2],
  confint(lm(outcome ~ predictor_3 * confounding_2, df_1))[2,2],
  
  ### df_2 - no outlier removal, outcome_2
  confint(lm(outcome ~ predictor_1, df_2))[2,2],
  confint(lm(outcome ~ predictor_2, df_2))[2,2],
  confint(lm(outcome ~ predictor_3, df_2))[2,2],
  
  confint(lm(outcome ~ predictor_1 + confounding_1, df_2))[2,2],
  confint(lm(outcome ~ predictor_2 + confounding_1, df_2))[2,2],
  confint(lm(outcome ~ predictor_3 + confounding_1, df_2))[2,2],
  
  confint(lm(outcome ~ predictor_1 * confounding_1, df_2))[2,2],
  confint(lm(outcome ~ predictor_2 * confounding_1, df_2))[2,2],
  confint(lm(outcome ~ predictor_3 * confounding_1, df_2))[2,2],
  
  confint(lm(outcome ~ predictor_1 + confounding_2, df_2))[2,2],
  confint(lm(outcome ~ predictor_2 + confounding_2, df_2))[2,2],
  confint(lm(outcome ~ predictor_3 + confounding_2, df_2))[2,2],
  
  confint(lm(outcome ~ predictor_1 * confounding_2, df_2))[2,2],
  confint(lm(outcome ~ predictor_2 * confounding_2, df_2))[2,2],
  confint(lm(outcome ~ predictor_3 * confounding_2, df_2))[2,2],
  
  ### df_3 - outlier removal, outcome_1
  confint(lm(outcome ~ predictor_1, df_3))[2,2],
  confint(lm(outcome ~ predictor_2, df_3))[2,2],
  confint(lm(outcome ~ predictor_3, df_3))[2,2],
  
  confint(lm(outcome ~ predictor_1 + confounding_1, df_3))[2,2],
  confint(lm(outcome ~ predictor_2 + confounding_1, df_3))[2,2],
  confint(lm(outcome ~ predictor_3 + confounding_1, df_3))[2,2],
  
  confint(lm(outcome ~ predictor_1 * confounding_1, df_3))[2,2],
  confint(lm(outcome ~ predictor_2 * confounding_1, df_3))[2,2],
  confint(lm(outcome ~ predictor_3 * confounding_1, df_3))[2,2],
  
  confint(lm(outcome ~ predictor_1 + confounding_2, df_3))[2,2],
  confint(lm(outcome ~ predictor_2 + confounding_2, df_3))[2,2],
  confint(lm(outcome ~ predictor_3 + confounding_2, df_3))[2,2],
  
  confint(lm(outcome ~ predictor_1 * confounding_2, df_3))[2,2],
  confint(lm(outcome ~ predictor_2 * confounding_2, df_3))[2,2],
  confint(lm(outcome ~ predictor_3 * confounding_2, df_3))[2,2],
  
  ### df_4 - outlier removal, outcome_2
  confint(lm(outcome ~ predictor_1, df_4))[2,2],
  confint(lm(outcome ~ predictor_2, df_4))[2,2],
  confint(lm(outcome ~ predictor_3, df_4))[2,2],
  
  confint(lm(outcome ~ predictor_1 + confounding_1, df_4))[2,2],
  confint(lm(outcome ~ predictor_2 + confounding_1, df_4))[2,2],
  confint(lm(outcome ~ predictor_3 + confounding_1, df_4))[2,2],
  
  confint(lm(outcome ~ predictor_1 * confounding_1, df_4))[2,2],
  confint(lm(outcome ~ predictor_2 * confounding_1, df_4))[2,2],
  confint(lm(outcome ~ predictor_3 * confounding_1, df_4))[2,2],
  
  confint(lm(outcome ~ predictor_1 + confounding_2, df_4))[2,2],
  confint(lm(outcome ~ predictor_2 + confounding_2, df_4))[2,2],
  confint(lm(outcome ~ predictor_3 + confounding_2, df_4))[2,2],
  
  confint(lm(outcome ~ predictor_1 * confounding_2, df_4))[2,2],
  confint(lm(outcome ~ predictor_2 * confounding_2, df_4))[2,2],
  confint(lm(outcome ~ predictor_3 * confounding_2, df_4))[2,2]
)

## Lower limit confidence interval ----

confint_lower <- c(
  
  ### df_1 - no outlier removal, outcome_1
  confint(lm(outcome ~ predictor_1, df_1))[2,1],
  confint(lm(outcome ~ predictor_2, df_1))[2,1],
  confint(lm(outcome ~ predictor_3, df_1))[2,1],
  
  confint(lm(outcome ~ predictor_1 + confounding_1, df_1))[2,1],
  confint(lm(outcome ~ predictor_2 + confounding_1, df_1))[2,1],
  confint(lm(outcome ~ predictor_3 + confounding_1, df_1))[2,1],
  
  confint(lm(outcome ~ predictor_1 * confounding_1, df_1))[2,1],
  confint(lm(outcome ~ predictor_2 * confounding_1, df_1))[2,1],
  confint(lm(outcome ~ predictor_3 * confounding_1, df_1))[2,1],
  
  confint(lm(outcome ~ predictor_1 + confounding_2, df_1))[2,1],
  confint(lm(outcome ~ predictor_2 + confounding_2, df_1))[2,1],
  confint(lm(outcome ~ predictor_3 + confounding_2, df_1))[2,1],
  
  confint(lm(outcome ~ predictor_1 * confounding_2, df_1))[2,1],
  confint(lm(outcome ~ predictor_2 * confounding_2, df_1))[2,1],
  confint(lm(outcome ~ predictor_3 * confounding_2, df_1))[2,1],
  
  ### df_2 - no outlier removal, outcome_2
  confint(lm(outcome ~ predictor_1, df_2))[2,1],
  confint(lm(outcome ~ predictor_2, df_2))[2,1],
  confint(lm(outcome ~ predictor_3, df_2))[2,1],
  
  confint(lm(outcome ~ predictor_1 + confounding_1, df_2))[2,1],
  confint(lm(outcome ~ predictor_2 + confounding_1, df_2))[2,1],
  confint(lm(outcome ~ predictor_3 + confounding_1, df_2))[2,1],
  
  confint(lm(outcome ~ predictor_1 * confounding_1, df_2))[2,1],
  confint(lm(outcome ~ predictor_2 * confounding_1, df_2))[2,1],
  confint(lm(outcome ~ predictor_3 * confounding_1, df_2))[2,1],
  
  confint(lm(outcome ~ predictor_1 + confounding_2, df_2))[2,1],
  confint(lm(outcome ~ predictor_2 + confounding_2, df_2))[2,1],
  confint(lm(outcome ~ predictor_3 + confounding_2, df_2))[2,1],
  
  confint(lm(outcome ~ predictor_1 * confounding_2, df_2))[2,1],
  confint(lm(outcome ~ predictor_2 * confounding_2, df_2))[2,1],
  confint(lm(outcome ~ predictor_3 * confounding_2, df_2))[2,1],
  
  ### df_3 - outlier removal, outcome_1
  confint(lm(outcome ~ predictor_1, df_3))[2,1],
  confint(lm(outcome ~ predictor_2, df_3))[2,1],
  confint(lm(outcome ~ predictor_3, df_3))[2,1],
  
  confint(lm(outcome ~ predictor_1 + confounding_1, df_3))[2,1],
  confint(lm(outcome ~ predictor_2 + confounding_1, df_3))[2,1],
  confint(lm(outcome ~ predictor_3 + confounding_1, df_3))[2,1],
  
  confint(lm(outcome ~ predictor_1 * confounding_1, df_3))[2,1],
  confint(lm(outcome ~ predictor_2 * confounding_1, df_3))[2,1],
  confint(lm(outcome ~ predictor_3 * confounding_1, df_3))[2,1],
  
  confint(lm(outcome ~ predictor_1 + confounding_2, df_3))[2,1],
  confint(lm(outcome ~ predictor_2 + confounding_2, df_3))[2,1],
  confint(lm(outcome ~ predictor_3 + confounding_2, df_3))[2,1],
  
  confint(lm(outcome ~ predictor_1 * confounding_2, df_3))[2,1],
  confint(lm(outcome ~ predictor_2 * confounding_2, df_3))[2,1],
  confint(lm(outcome ~ predictor_3 * confounding_2, df_3))[2,1],
  
  ### df_4 - outlier removal, outcome_2
  confint(lm(outcome ~ predictor_1, df_4))[2,1],
  confint(lm(outcome ~ predictor_2, df_4))[2,1],
  confint(lm(outcome ~ predictor_3, df_4))[2,1],
  
  confint(lm(outcome ~ predictor_1 + confounding_1, df_4))[2,1],
  confint(lm(outcome ~ predictor_2 + confounding_1, df_4))[2,1],
  confint(lm(outcome ~ predictor_3 + confounding_1, df_4))[2,1],
  
  confint(lm(outcome ~ predictor_1 * confounding_1, df_4))[2,1],
  confint(lm(outcome ~ predictor_2 * confounding_1, df_4))[2,1],
  confint(lm(outcome ~ predictor_3 * confounding_1, df_4))[2,1],
  
  confint(lm(outcome ~ predictor_1 + confounding_2, df_4))[2,1],
  confint(lm(outcome ~ predictor_2 + confounding_2, df_4))[2,1],
  confint(lm(outcome ~ predictor_3 + confounding_2, df_4))[2,1],
  
  confint(lm(outcome ~ predictor_1 * confounding_2, df_4))[2,1],
  confint(lm(outcome ~ predictor_2 * confounding_2, df_4))[2,1],
  confint(lm(outcome ~ predictor_3 * confounding_2, df_4))[2,1]
)


## Effect size ----

adj.r.squared <- c(
  
  ### df_1 - no outlier removal, outcome_1
  summary(lm(outcome ~ predictor_1, df_1))$adj.r.squared,
  summary(lm(outcome ~ predictor_2, df_1))$adj.r.squared,
  summary(lm(outcome ~ predictor_3, df_1))$adj.r.squared,
  
  summary(lm(outcome ~ predictor_1 + confounding_1, df_1))$adj.r.squared,
  summary(lm(outcome ~ predictor_2 + confounding_1, df_1))$adj.r.squared,
  summary(lm(outcome ~ predictor_3 + confounding_1, df_1))$adj.r.squared,
  
  summary(lm(outcome ~ predictor_1 * confounding_1, df_1))$adj.r.squared,
  summary(lm(outcome ~ predictor_2 * confounding_1, df_1))$adj.r.squared,
  summary(lm(outcome ~ predictor_3 * confounding_1, df_1))$adj.r.squared,
  
  summary(lm(outcome ~ predictor_1 + confounding_2, df_1))$adj.r.squared,
  summary(lm(outcome ~ predictor_2 + confounding_2, df_1))$adj.r.squared,
  summary(lm(outcome ~ predictor_3 + confounding_2, df_1))$adj.r.squared,
  
  summary(lm(outcome ~ predictor_1 * confounding_2, df_1))$adj.r.squared,
  summary(lm(outcome ~ predictor_2 * confounding_2, df_1))$adj.r.squared,
  summary(lm(outcome ~ predictor_3 * confounding_2, df_1))$adj.r.squared,
  
  ## df_2 - no outlier removal, outcome_2
  summary(lm(outcome ~ predictor_1, df_2))$adj.r.squared,
  summary(lm(outcome ~ predictor_2, df_2))$adj.r.squared,
  summary(lm(outcome ~ predictor_3, df_2))$adj.r.squared,
  
  summary(lm(outcome ~ predictor_1 + confounding_1, df_2))$adj.r.squared,
  summary(lm(outcome ~ predictor_2 + confounding_1, df_2))$adj.r.squared,
  summary(lm(outcome ~ predictor_3 + confounding_1, df_2))$adj.r.squared,
  
  summary(lm(outcome ~ predictor_1 * confounding_1, df_2))$adj.r.squared,
  summary(lm(outcome ~ predictor_2 * confounding_1, df_2))$adj.r.squared,
  summary(lm(outcome ~ predictor_3 * confounding_1, df_2))$adj.r.squared,
  
  summary(lm(outcome ~ predictor_1 + confounding_2, df_2))$adj.r.squared,
  summary(lm(outcome ~ predictor_2 + confounding_2, df_2))$adj.r.squared,
  summary(lm(outcome ~ predictor_3 + confounding_2, df_2))$adj.r.squared,
  
  summary(lm(outcome ~ predictor_1 * confounding_2, df_2))$adj.r.squared,
  summary(lm(outcome ~ predictor_2 * confounding_2, df_2))$adj.r.squared,
  summary(lm(outcome ~ predictor_3 * confounding_2, df_2))$adj.r.squared,
  
  ## df_3 - outlier removal, outcome_1
  summary(lm(outcome ~ predictor_1, df_3))$adj.r.squared,
  summary(lm(outcome ~ predictor_2, df_3))$adj.r.squared,
  summary(lm(outcome ~ predictor_3, df_3))$adj.r.squared,
  
  summary(lm(outcome ~ predictor_1 + confounding_1, df_3))$adj.r.squared,
  summary(lm(outcome ~ predictor_2 + confounding_1, df_3))$adj.r.squared,
  summary(lm(outcome ~ predictor_3 + confounding_1, df_3))$adj.r.squared,
  
  summary(lm(outcome ~ predictor_1 * confounding_1, df_3))$adj.r.squared,
  summary(lm(outcome ~ predictor_2 * confounding_1, df_3))$adj.r.squared,
  summary(lm(outcome ~ predictor_3 * confounding_1, df_3))$adj.r.squared,
  
  summary(lm(outcome ~ predictor_1 + confounding_2, df_3))$adj.r.squared,
  summary(lm(outcome ~ predictor_2 + confounding_2, df_3))$adj.r.squared,
  summary(lm(outcome ~ predictor_3 + confounding_2, df_3))$adj.r.squared,
  
  summary(lm(outcome ~ predictor_1 * confounding_2, df_3))$adj.r.squared,
  summary(lm(outcome ~ predictor_2 * confounding_2, df_3))$adj.r.squared,
  summary(lm(outcome ~ predictor_3 * confounding_2, df_3))$adj.r.squared,
  
  ## df_4 - outlier removal, outcome_2
  summary(lm(outcome ~ predictor_1, df_4))$adj.r.squared,
  summary(lm(outcome ~ predictor_2, df_4))$adj.r.squared,
  summary(lm(outcome ~ predictor_3, df_4))$adj.r.squared,
  
  summary(lm(outcome ~ predictor_1 + confounding_1, df_4))$adj.r.squared,
  summary(lm(outcome ~ predictor_2 + confounding_1, df_4))$adj.r.squared,
  summary(lm(outcome ~ predictor_3 + confounding_1, df_4))$adj.r.squared,
  
  summary(lm(outcome ~ predictor_1 * confounding_1, df_4))$adj.r.squared,
  summary(lm(outcome ~ predictor_2 * confounding_1, df_4))$adj.r.squared,
  summary(lm(outcome ~ predictor_3 * confounding_1, df_4))$adj.r.squared,
  
  summary(lm(outcome ~ predictor_1 + confounding_2, df_4))$adj.r.squared,
  summary(lm(outcome ~ predictor_2 + confounding_2, df_4))$adj.r.squared,
  summary(lm(outcome ~ predictor_3 + confounding_2, df_4))$adj.r.squared,
  
  summary(lm(outcome ~ predictor_1 * confounding_2, df_4))$adj.r.squared,
  summary(lm(outcome ~ predictor_2 * confounding_2, df_4))$adj.r.squared,
  summary(lm(outcome ~ predictor_3 * confounding_2, df_4))$adj.r.squared
)


multiverse <- data.frame(outlier_detection, confounding_variable, predictor_variable, outcome_variable, p_values, estimates, confint_upper, confint_lower, adj.r.squared)



# Multiverse plots ----

## Outcome histogram
ggplot(multiverse, aes(x = p_values)) +
  geom_histogram(bins=9, fill = 'white', color="black", size = .75) +
  xlab('') +
  ylab('') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 11))

## Outcome scatterplot
multiverse$index <- 1:nrow(multiverse)
ggplot(multiverse, aes(x = p_values, y = index, colour = outcome_variable)) +
  geom_point() +
  xlab('') +
  ylab('') +
  scale_color_manual(values = c("#007991","#E6AF2E")) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 11))

## Outcome boxplots
ggplot(multiverse, aes(x = p_values, y = confounding_variable)) +
  geom_boxplot() +
  xlab('') +
  ylab('') +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 11))

## Specification curve
multiverse$index <- 1:nrow(multiverse)

multiverse$index = factor(multiverse$index, levels=multiverse[order(multiverse$estimates), "index"])

ggplot(multiverse, aes(x = index, y = estimates)) +
  geom_point(color = "#007991") +
  theme_bw() +
  geom_errorbar(aes(ymin=confint_lower, ymax=confint_upper), width=.3, color = "#007991") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 11))

## Vibration of Effects Plot
ggplot(multiverse, aes(x = adj.r.squared, y = p_values, colour = predictor_variable)) +
  geom_jitter(width = .005, height = .01, size = 3, alpha = .5) +
  xlab('') +
  ylab('') +
  theme_bw() +
  geom_hline(yintercept = .05, size = 1) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 11))

# Descriptives ----

## Relative effect size

quantile(adj.r.squared, probs = c(0.99)) - quantile(adj.r.squared, probs = c(0.01))

## Relative p value

quantile(-1*log10(p_values), probs = c(0.99)) - quantile(-1*log10(p_values), probs = c(0.01))


