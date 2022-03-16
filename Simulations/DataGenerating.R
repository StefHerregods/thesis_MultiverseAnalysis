# Simulates the data used in the multiverse analyses
# Partially based on https://debruine.github.io/tutorials/sim-lmer.html

setwd('C:\\Users\\herre\\OneDrive\\Documenten\\GitHub\\thesis_MultiverseAnalysis\\Simulations')
rm(list = ls(all = TRUE))

# Load packages

library(tidyverse)  # Tidy data
library(car)

# Parameters

set.seed(0)
a <- rnorm(300, mean = 20, sd = 3)
b <- rnorm(300, mean = 90, sd = 7)
c <- rnorm(300, mean = 70, sd = 9)
d <- rnorm(300, mean = 18, sd = 13)

x_variation <- rnorm(300, mean = 0, sd = 1)
y_variation <- rnorm(300, mean = 0, sd = 7)
z_variation <- rnorm(300, mean = 0, sd = 7)

x_noise <- rnorm(300, mean = 0, sd = 10)  
y_noise <- rnorm(300, mean = 0, sd = 7)
z_noise <- rnorm(300, mean = 0, sd = 9)

dv_variation <- rnorm(300, mean = 0, sd = 65) 
dv_noise <- rnorm(300, mean = 0, sd = 95)
add_noise <- runif(300,0,1) > 0.9  # Only add outlier noise in 90% of the trials
  
# Calculate variables

y <- b + c + y_variation
z <- c + d + z_variation
x <- y + z + x_variation
dv <- x + y + z + dv_variation + dv_noise * add_noise
x <- x + x_noise
y <- y + y_noise
z <- z + z_noise
df <- data.frame(x, y, z, dv)

# Checks

plot(df)
model <- lm(dv ~ x + y + z, data = df)
summary(model)
vif(model)
cor(y, z)

# Save data

save(df, file = 'sim_data.rdata')

