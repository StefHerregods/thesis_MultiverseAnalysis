#

setwd('C:\\Users\\herre\\OneDrive\\Documenten\\GitHub\\thesis_MultiverseAnalysis\\Simulations')
rm(list = ls(all = TRUE))

# Load packages

library(ggplot2)
library(dplyr)
library(grid)

# Load data

load('sim_data.rdata')


### REMOVING OUTLIERS ###


# No outliers removed

df

# Visual check (less strict)

ggplot(df, aes(y = dv, x = x)) +
  geom_point() +
  geom_hline(yintercept = c(0, 1100))


df1 <- filter(df, (dv < 1100) & (dv > 0))

# Visual check (very strict)

ggplot(df, aes(y = dv, x = x)) +
  geom_point() +
  geom_hline(yintercept = c(200, 800))


df2 <- filter(df, (dv < 800) & (dv > 200))

# Interquartile range

ggplot(df, aes(y = dv)) +
  geom_boxplot()

outliers <- boxplot(df$dv, plot=FALSE)$out

df3 <- filter(df, !(dv %in% outliers))


### W AS A BINARY VARIABLE ###


ggplot(data = df, aes(x = w)) +
  geom_histogram() +
  geom_vline(xintercept = mean(0))

df4 <- mutate(df, w = ifelse(w > 0, 1, 0))
df5 <- mutate(df1, w = ifelse(w > 0, 1, 0))
df6 <- mutate(df2, w = ifelse(w > 0, 1, 0))
df7 <- mutate(df3, w = ifelse(w > 0, 1, 0))


### MODELLING ###


## Only w as confounding variable ##

m0 <- lm(dv ~ x + w, data = df)
m1 <- lm(dv ~ x + w, data = df1)
m2 <- lm(dv ~ x + w, data = df2)
m3 <- lm(dv ~ x + w, data = df3)
m4 <- lm(dv ~ x + w, data = df4)
m5 <- lm(dv ~ x + w, data = df5)
m6 <- lm(dv ~ x + w, data = df6)
m7 <- lm(dv ~ x + w, data = df7)

## w & y ##

m8 <- lm(dv ~ x + w + y, data = df)
m9 <- lm(dv ~ x + w + y, data = df1)
m10 <- lm(dv ~ x + w + y, data = df2)
m11 <- lm(dv ~ x + w + y, data = df3)
m12 <- lm(dv ~ x + w + y, data = df4)
m13 <- lm(dv ~ x + w + y, data = df5)
m14 <- lm(dv ~ x + w + y, data = df6)
m15 <- lm(dv ~ x + w + y, data = df7)

## w $ z ##

m16 <- lm(dv ~ x + w + z, data = df)
m17 <- lm(dv ~ x + w + z, data = df1)
m18 <- lm(dv ~ x + w + z, data = df2)
m19 <- lm(dv ~ x + w + z, data = df3)
m20 <- lm(dv ~ x + w + z, data = df4)
m21 <- lm(dv ~ x + w + z, data = df5)
m22 <- lm(dv ~ x + w + z, data = df6)
m23 <- lm(dv ~ x + w + z, data = df7)

## w, y & z ##

m24 <- lm(dv ~ x + w + y + z, data = df)
m25 <- lm(dv ~ x + w + y + z, data = df1)
m26 <- lm(dv ~ x + w + y + z, data = df2)
m27 <- lm(dv ~ x + w + y + z, data = df3)
m28 <- lm(dv ~ x + w + y + z, data = df4)
m29 <- lm(dv ~ x + w + y + z, data = df5)
m30 <- lm(dv ~ x + w + y + z, data = df6)
m31 <- lm(dv ~ x + w + y + z, data = df7)

# Save models
models <- c(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20, m21, m22, m23, m24, m25, m26, m27, m28, m29, m30, m31)
j <- 0
for (i in models){
  save(i, file = paste0('m', j, '.rdata'))
  j <- j + 1
}


### CREATING A DATAFRAME WITH ESTIMATED EFFECT SIZES AND P-VALUES PER MODEL ###


# Load models
for (i in 0:31){
  load(paste0('m', as.character(i), '.rdata'))
}

# Create dataframe
estimate <- NULL
p_value <- NULL
models <- list(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20, m21, m22, m23, m24, m25, m26, m27, m28, m29, m30, m31)
summary <- lapply(models, summary)
outliers <- c('none', 'visual1', 'visual2', 'interquartile', 'none', 'visual1', 'visual2', 'interquartile', 'none', 'visual1', 'visual2', 'interquartile', 'none', 'visual1', 'visual2', 'interquartile', 'none', 'visual1', 'visual2', 'interquartile', 'none', 'visual1', 'visual2', 'interquartile', 'none', 'visual1', 'visual2', 'interquartile', 'none', 'visual1', 'visual2', 'interquartile')
w_binary <- c('no', 'no', 'no', 'no', 'yes', 'yes', 'yes', 'yes', 'no', 'no', 'no', 'no', 'yes', 'yes', 'yes', 'yes', 'no', 'no', 'no', 'no', 'yes', 'yes', 'yes', 'yes', 'no', 'no', 'no', 'no', 'yes', 'yes', 'yes', 'yes')
Y <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)
Z <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
for (i in 1){
  estimate <- append(estimate, summary(m0)$coefficients[2,1])
  estimate <- append(estimate, summary(m1)$coefficients[2,1])
  estimate <- append(estimate, summary(m2)$coefficients[2,1])
  estimate <- append(estimate, summary(m3)$coefficients[2,1])
  estimate <- append(estimate, summary(m4)$coefficients[2,1])
  estimate <- append(estimate, summary(m5)$coefficients[2,1])
  estimate <- append(estimate, summary(m6)$coefficients[2,1])
  estimate <- append(estimate, summary(m7)$coefficients[2,1])
  estimate <- append(estimate, summary(m8)$coefficients[2,1])
  estimate <- append(estimate, summary(m9)$coefficients[2,1])
  estimate <- append(estimate, summary(m10)$coefficients[2,1])
  estimate <- append(estimate, summary(m11)$coefficients[2,1])
  estimate <- append(estimate, summary(m12)$coefficients[2,1])
  estimate <- append(estimate, summary(m13)$coefficients[2,1])
  estimate <- append(estimate, summary(m14)$coefficients[2,1])
  estimate <- append(estimate, summary(m15)$coefficients[2,1])
  estimate <- append(estimate, summary(m16)$coefficients[2,1])
  estimate <- append(estimate, summary(m17)$coefficients[2,1])
  estimate <- append(estimate, summary(m18)$coefficients[2,1])
  estimate <- append(estimate, summary(m19)$coefficients[2,1])
  estimate <- append(estimate, summary(m20)$coefficients[2,1])
  estimate <- append(estimate, summary(m21)$coefficients[2,1])
  estimate <- append(estimate, summary(m22)$coefficients[2,1])
  estimate <- append(estimate, summary(m23)$coefficients[2,1])
  estimate <- append(estimate, summary(m24)$coefficients[2,1])
  estimate <- append(estimate, summary(m25)$coefficients[2,1])
  estimate <- append(estimate, summary(m26)$coefficients[2,1])
  estimate <- append(estimate, summary(m27)$coefficients[2,1])
  estimate <- append(estimate, summary(m28)$coefficients[2,1])
  estimate <- append(estimate, summary(m29)$coefficients[2,1])
  estimate <- append(estimate, summary(m30)$coefficients[2,1])
  estimate <- append(estimate, summary(m31)$coefficients[2,1])
  p_value <- append(p_value, summary(m0)$coefficients[2,4])
  p_value <- append(p_value, summary(m1)$coefficients[2,4])
  p_value <- append(p_value, summary(m2)$coefficients[2,4])
  p_value <- append(p_value, summary(m3)$coefficients[2,4])
  p_value <- append(p_value, summary(m4)$coefficients[2,4])
  p_value <- append(p_value, summary(m5)$coefficients[2,4])
  p_value <- append(p_value, summary(m6)$coefficients[2,4])
  p_value <- append(p_value, summary(m7)$coefficients[2,4])
  p_value <- append(p_value, summary(m8)$coefficients[2,4])
  p_value <- append(p_value, summary(m9)$coefficients[2,4])
  p_value <- append(p_value, summary(m10)$coefficients[2,4])
  p_value <- append(p_value, summary(m11)$coefficients[2,4])
  p_value <- append(p_value, summary(m12)$coefficients[2,4])
  p_value <- append(p_value, summary(m13)$coefficients[2,4])
  p_value <- append(p_value, summary(m14)$coefficients[2,4])
  p_value <- append(p_value, summary(m15)$coefficients[2,4])
  p_value <- append(p_value, summary(m16)$coefficients[2,4])
  p_value <- append(p_value, summary(m17)$coefficients[2,4])
  p_value <- append(p_value, summary(m18)$coefficients[2,4])
  p_value <- append(p_value, summary(m19)$coefficients[2,4])
  p_value <- append(p_value, summary(m20)$coefficients[2,4])
  p_value <- append(p_value, summary(m21)$coefficients[2,4])
  p_value <- append(p_value, summary(m22)$coefficients[2,4])
  p_value <- append(p_value, summary(m23)$coefficients[2,4])
  p_value <- append(p_value, summary(m24)$coefficients[2,4])
  p_value <- append(p_value, summary(m25)$coefficients[2,4])
  p_value <- append(p_value, summary(m26)$coefficients[2,4])
  p_value <- append(p_value, summary(m27)$coefficients[2,4])
  p_value <- append(p_value, summary(m28)$coefficients[2,4])
  p_value <- append(p_value, summary(m29)$coefficients[2,4])
  p_value <- append(p_value, summary(m30)$coefficients[2,4])
  p_value <- append(p_value, summary(m31)$coefficients[2,4])
}
full <- data.frame(outliers, w_binary, Y, Z, estimate, p_value)

# Save models

j <- 0
for (i in models){
  save(i, file = paste0('m', j, '.rdata'))
  j <- j + 1
}

# Plots to check data frame

ggplot(full, aes(x=outliers, colour = w_binary, y = p_value)) + 
  geom_jitter(stat='identity', fill="black", size=4, alpha = 0.3, width = 0.2)  +
  labs(title = "P-values", 
       color = 'w as binary variable') + 
  ylim(0, 0.1) +
  coord_flip() + 
  theme_bw() + 
  geom_hline(yintercept = 0.05, lty = 2) +
  xlab('Dealing with outliers') +
  ylab('P-values')

ggplot(full, aes(x = as.factor(Z), colour = as.factor(Y), y = p_value)) + 
  geom_jitter(stat='identity', fill="black", size = 4, alpha = 0.3, width = 0.1)  +
  labs(title="P-values", 
       color = 'Adding confounding variable Y') + 
  ylim(0, 0.1) +
  coord_flip() +
  theme_bw() + 
  geom_hline(yintercept = 0.05, lty = 2) +
  xlab('Adding confounding variable X') +
  ylab('P-values')




