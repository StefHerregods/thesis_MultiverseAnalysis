# Specification curve analysis on simulated data set


# Set-up ----

library(ggplot2)
library(MASS)

windowsFonts(font = windowsFont("Times New Roman"))

simulations = 1000

strength = .34 # predictor ~ outcome variable covariance


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

df_original <- df

# Inferential statistics specification curve analysis ----

estimate_median <- NULL
p_value_share <- NULL

## Loop through zero-effect multiverses
for (i in 1:simulations){
  
  estimates <- NULL
  
  print(paste0('Start simulation multiverse ', i))
  df <- transform(df_original, outcome_1 = sample(outcome_1))  # Randomly shuffle the dependent variables
  #df <- transform(df, outcome_2 = sample(outcome_2))  # Randomly shuffle the dependent variables
  
  
  
  df_1 <- df
  df_1$outcome <- df_1$outcome_1
  df_2 <- df
  df_2$outcome <- df_2$outcome_2
  
  ## Outlier removal 
  
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
  
  ## Create multiverse 
  
  outlier_detection <- c('none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'none', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR', 'IQR')
  predictor_variable <- c('1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum', '1', '2', 'sum')
  confounding_variable <- c('none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction', 'none', '1', '1 + interaction', '2', '2 + interaction')
  outcome_variable <- c('1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2', '1', '2')
  
  
  
  estimates <- c(
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
    
    ## df_2 - no outlier removal, outcome_2
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
    
    ## df_3 - outlier removal, outcome_1
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
    
    ## df_4 - outlier removal, outcome_2
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
    
    ## df_2 - no outlier removal, outcome_2
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
    
    ## df_3 - outlier removal, outcome_1
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
    
    ## df_4 - outlier removal, outcome_2
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
  
  
  
  temp <- data.frame(p_values, estimates)
  
  p_value_share <- append(p_value_share, nrow(temp[temp$p_values <.05 & temp$estimates > 0,]))
  
  estimate_median <- append(estimate_median, median(estimates))
  
}

estimate_median <- data.frame(estimate_median)
p_value_share <- data.frame(p_value_share)


# True multiverse(black line) versus zero-effect multiverses (density distribution) ----

## Median effect
ggplot(estimate_median, aes(x = estimate_median)) +
  geom_density() +
  geom_vline(xintercept = .5846434) +
  theme_bw()

## Proportion of significant effects 
ggplot(p_value_share, aes(x = p_value_share)) +
  geom_density() +
  geom_vline(xintercept = 42) +
  theme_bw()
 











