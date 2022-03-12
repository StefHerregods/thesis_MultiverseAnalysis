#

setwd('C:\\Users\\herre\\OneDrive\\Documenten\\GitHub\\thesis_MultiverseAnalysis\\Simulations')
rm(list = ls(all = TRUE))

# Load packages

library(ggplot2)
library(dplyr)
library(grid)

# Load data

load('sim.rdata')


### 1 - REMOVING OUTLIERS ###


# No outliers removed

df

# Visual check (less strict)

ggplot(df, aes(x = sub_cond, y = dv, colour = stim_version)) +
  geom_boxplot() +
  geom_hline(yintercept = 3000)


df1 <- filter(df, dv < 3000)

# Visual check (very strict)

ggplot(df, aes(x = sub_cond, y = dv, colour = stim_version)) +
  geom_boxplot() +
  geom_hline(yintercept = 2000)


df2 <- filter(df, dv < 2000)

# Interquartile range

ggplot(df, aes(x = sub_cond, y = dv, colour = stim_version)) +
  geom_boxplot()

outliers <- boxplot(df$dv, plot=FALSE)$out

df3 <- filter(df, !(dv %in% outliers))


### 2 - Motivation ###


ggplot(data = df, aes(x = sub_id, fill = resp)) + 
  geom_bar(position = "fill") +
  geom_hline(yintercept = 0.9) +
  geom_hline(yintercept = 0.1) +
  xlab('subject identification number') +
  ylab('Proportion left vs. right responses') +
  labs(fill = 'Response')

df4 <- filter(df, (resp_mean >= 0.1) & (resp_mean <= 0.9))
df5 <- filter(df1, (resp_mean >= 0.1) & (resp_mean <= 0.9))
df6 <- filter(df2, (resp_mean >= 0.1) & (resp_mean <= 0.9))
df7 <- filter(df3, (resp_mean >= 0.1) & (resp_mean <= 0.9))


### 3 - MODELLING ###


## No confounding variable ##


# Basic model
m0.1 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1|stim_id), data = df, control = lmerControl(optimizer = "bobyqa"))
m1.1 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1|stim_id), data = df1, control = lmerControl(optimizer = "bobyqa"))
m2.1 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1|stim_id), data = df2, control = lmerControl(optimizer = "bobyqa"))
m3.1 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1|stim_id), data = df3, control = lmerControl(optimizer = "bobyqa"))
m4.1 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1|stim_id), data = df4, control = lmerControl(optimizer = "bobyqa"))
m5.1 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1|stim_id), data = df5, control = lmerControl(optimizer = "bobyqa"))
m6.1 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1|stim_id), data = df6, control = lmerControl(optimizer = "bobyqa"))
m7.1 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1|stim_id), data = df7, control = lmerControl(optimizer = "bobyqa"))

# Model 0
m0.2 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1|stim_id), data = df, control = lmerControl(optimizer = "bobyqa"))
m0.3 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1 + stim_version|stim_id), data = df, control = lmerControl(optimizer = "bobyqa"))
m0.4 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1 + sub_cond|stim_id), data = df, control = lmerControl(optimizer = "bobyqa"))
                   # AIC    BIC    logLik
anova(m0.1, m0.2)  # 213369 213440 -106675 **
anova(m0.1, m0.3)  # 213706 213777 -106844 *
anova(m0.1, m0.4)  # 210798 210869 -105390 ***
m0.5 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + sub_cond|stim_id), data = df, control = lmerControl(optimizer = "bobyqa"))
anova(m0.4, m0.5)  # Significant
m0.6 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + stim_version + sub_cond|stim_id), data = df, control = lmerControl(optimizer = "bobyqa"))
anova(m0.5, m0.6)  # Significant
m0.7 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df, control = lmerControl(optimizer = "bobyqa"))
anova(m0.6, m0.7)  # Significant
m0 <- m0.7

# Model 1
m1.2 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1|stim_id), data = df1, control = lmerControl(optimizer = "bobyqa"))
m1.3 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1 + stim_version|stim_id), data = df1, control = lmerControl(optimizer = "bobyqa"))
m1.4 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1 + sub_cond|stim_id), data = df1, control = lmerControl(optimizer = "bobyqa"))
                   # AIC    BIC    logLik
anova(m1.1, m1.2)  # 209014 209085 -104498 **
anova(m1.1, m1.3)  # 209471 209542 -104726 *
anova(m1.1, m1.4)  # 205665 205736 -102823 ***
m1.5 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + sub_cond|stim_id), data = df1, control = lmerControl(optimizer = "bobyqa"))
anova(m1.4, m1.5)  # Significant
m1.6 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + stim_version + sub_cond|stim_id), data = df1, control = lmerControl(optimizer = "bobyqa"))
anova(m1.5, m1.6)  # Significant
m1.7 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df1, control = lmerControl(optimizer = "bobyqa"))
anova(m1.6, m1.7)  # Significant
m1 <- m1.7

# Model 2
m2.2 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1|stim_id), data = df2, control = lmerControl(optimizer = "bobyqa"))
m2.3 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1 + stim_version|stim_id), data = df2, control = lmerControl(optimizer = "bobyqa"))
m2.4 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1 + sub_cond|stim_id), data = df2, control = lmerControl(optimizer = "bobyqa"))
                   # AIC    BIC    logLik
anova(m2.1, m2.2)  # 197973 198044 -98978 **
anova(m2.1, m2.3)  # 198902 198973 -99442 *
anova(m2.1, m2.4)  # 191810 191881 -95896 ***
m2.5 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + sub_cond|stim_id), data = df2, control = lmerControl(optimizer = "bobyqa"))
anova(m2.4, m2.5)  # Significant
m2.6 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + stim_version + sub_cond|stim_id), data = df2, control = lmerControl(optimizer = "bobyqa"))
anova(m2.5, m2.6)  # Significant
m2.7 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df2, control = lmerControl(optimizer = "bobyqa"))
anova(m2.6, m2.7)  # Significant
m2 <- m2.7

# Model 3
m3.2 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1|stim_id), data = df3, control = lmerControl(optimizer = "bobyqa"))
m3.3 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1 + stim_version|stim_id), data = df3, control = lmerControl(optimizer = "bobyqa"))
m3.4 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1 + sub_cond|stim_id), data = df3, control = lmerControl(optimizer = "bobyqa"))
                   # AIC    BIC    logLik
anova(m3.1, m3.2)  # 195663 195734 -97822 **
anova(m3.1, m3.3)  # 196583 196654 -98282 *
anova(m3.1, m3.4)  # 189553 189624 -94768 ***
m3.5 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + sub_cond|stim_id), data = df3, control = lmerControl(optimizer = "bobyqa"))
anova(m3.4, m3.5)  # Significant
m3.6 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + stim_version + sub_cond|stim_id), data = df3, control = lmerControl(optimizer = "bobyqa"))
anova(m3.5, m3.6)  # Significant
m3.7 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df3, control = lmerControl(optimizer = "bobyqa"))
anova(m3.6, m3.7)  # Significant
m3 <- m3.7

# Model 4
m4.2 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1|stim_id), data = df4, control = lmerControl(optimizer = "bobyqa"))
m4.3 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1 + stim_version|stim_id), data = df4, control = lmerControl(optimizer = "bobyqa"))
m4.4 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1 + sub_cond|stim_id), data = df4, control = lmerControl(optimizer = "bobyqa"))
                   # AIC    BIC    logLik
anova(m4.1, m4.2)  # 208287 208358 -104134 **
anova(m4.1, m4.3)  # 208617 208688 -104300 *
anova(m4.1, m4.4)  # 205844 205915 -102913 ***
m4.5 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + sub_cond|stim_id), data = df4, control = lmerControl(optimizer = "bobyqa"))
anova(m4.4, m4.5)  # Significant
m4.6 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + stim_version + sub_cond|stim_id), data = df4, control = lmerControl(optimizer = "bobyqa"))
anova(m4.5, m4.6)  # Significant
m4.7 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df4, control = lmerControl(optimizer = "bobyqa"))
anova(m4.6, m4.7)  # Significant
m4 <- m4.7

# Model 5
m5.2 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1|stim_id), data = df5, control = lmerControl(optimizer = "bobyqa"))
m5.3 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1 + stim_version|stim_id), data = df5, control = lmerControl(optimizer = "bobyqa"))
m5.4 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1 + sub_cond|stim_id), data = df5, control = lmerControl(optimizer = "bobyqa"))
                   # AIC    BIC    logLik
anova(m5.1, m5.2)  # 192990 193061 -96486 **
anova(m5.1, m5.3)  # 193915 193986 -96949 *
anova(m5.1, m5.4)  # 187039 187110 -93510 ***
m5.5 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + sub_cond|stim_id), data = df5, control = lmerControl(optimizer = "bobyqa"))
anova(m5.4, m5.5)  # Significant
m5.6 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + stim_version + sub_cond|stim_id), data = df5, control = lmerControl(optimizer = "bobyqa"))
anova(m5.5, m5.6)  # Significant
m5.7 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df5, control = lmerControl(optimizer = "bobyqa"))
anova(m5.6, m5.7)  # Significant
m5 <- m5.7

# Model 6
m6.2 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1|stim_id), data = df6, control = lmerControl(optimizer = "bobyqa"))
m6.3 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1 + stim_version|stim_id), data = df6, control = lmerControl(optimizer = "bobyqa"))
m6.4 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1 + sub_cond|stim_id), data = df6, control = lmerControl(optimizer = "bobyqa"))
                   # AIC    BIC    logLik
anova(m6.1, m6.2)  # 190888 190959 -95435 **
anova(m6.1, m6.3)  # 191809 191879 -95895 *
anova(m6.1, m6.4)  # 184972 185042 -92477 ***
m6.5 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + sub_cond|stim_id), data = df6, control = lmerControl(optimizer = "bobyqa"))
anova(m6.4, m6.5)  # Significant
m6.6 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + stim_version + sub_cond|stim_id), data = df6, control = lmerControl(optimizer = "bobyqa"))
anova(m6.5, m6.6)  # Significant
m6.7 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df6, control = lmerControl(optimizer = "bobyqa"))
anova(m6.6, m6.7)  # Significant
m6 <- m6.7

# Model 7
m7.2 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1|stim_id), data = df7, control = lmerControl(optimizer = "bobyqa"))
m7.3 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1 + stim_version|stim_id), data = df7, control = lmerControl(optimizer = "bobyqa"))
m7.4 <- lmer(dv ~ sub_cond * stim_version + (1|sub_id) + (1 + sub_cond|stim_id), data = df7, control = lmerControl(optimizer = "bobyqa"))
                   # AIC    BIC    logLik
anova(m7.1, m7.2)  # 190888 190959 -95435 **
anova(m7.1, m7.3)  # 191809 191879 -95895 *
anova(m7.1, m7.4)  # 184972 185042 -92477 ***
m7.5 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + sub_cond|stim_id), data = df7, control = lmerControl(optimizer = "bobyqa"))
anova(m7.4, m7.5)  # Significant
m7.6 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + stim_version + sub_cond|stim_id), data = df7, control = lmerControl(optimizer = "bobyqa"))
anova(m7.5, m7.6)  # Significant
m7.7 <- lmer(dv ~ sub_cond * stim_version + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df7, control = lmerControl(optimizer = "bobyqa"))
anova(m7.6, m7.7)  # Significant
m7 <- m7.7

# Adding X
m8 <- lmer(dv ~ sub_cond * stim_version + X + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df, control = lmerControl(optimizer = "bobyqa"))
m9 <- lmer(dv ~ sub_cond * stim_version + X + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df1, control = lmerControl(optimizer = "bobyqa"))
m10 <- lmer(dv ~ sub_cond * stim_version + X + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df2, control = lmerControl(optimizer = "bobyqa"))
m11 <- lmer(dv ~ sub_cond * stim_version + X + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df3, control = lmerControl(optimizer = "bobyqa"))
m12 <- lmer(dv ~ sub_cond * stim_version + X + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df4, control = lmerControl(optimizer = "bobyqa"))
m13 <- lmer(dv ~ sub_cond * stim_version + X + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df5, control = lmerControl(optimizer = "bobyqa"))
m14 <- lmer(dv ~ sub_cond * stim_version + X + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df6, control = lmerControl(optimizer = "bobyqa"))
m15 <- lmer(dv ~ sub_cond * stim_version + X + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df7, control = lmerControl(optimizer = "bobyqa"))

# Adding Y
m16 <- lmer(dv ~ sub_cond * stim_version + Y + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df, control = lmerControl(optimizer = "bobyqa"))
m17 <- lmer(dv ~ sub_cond * stim_version + Y + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df1, control = lmerControl(optimizer = "bobyqa"))
m18 <- lmer(dv ~ sub_cond * stim_version + Y + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df2, control = lmerControl(optimizer = "bobyqa"))
m19 <- lmer(dv ~ sub_cond * stim_version + Y + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df3, control = lmerControl(optimizer = "bobyqa"))
m20 <- lmer(dv ~ sub_cond * stim_version + Y + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df4, control = lmerControl(optimizer = "bobyqa"))
m21 <- lmer(dv ~ sub_cond * stim_version + Y + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df5, control = lmerControl(optimizer = "bobyqa"))
m22 <- lmer(dv ~ sub_cond * stim_version + Y + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df6, control = lmerControl(optimizer = "bobyqa"))
m23 <- lmer(dv ~ sub_cond * stim_version + Y + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df7, control = lmerControl(optimizer = "bobyqa"))

# Adding Y and X
m24 <- lmer(dv ~ sub_cond * stim_version + Y + X + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df, control = lmerControl(optimizer = "bobyqa"))
m25 <- lmer(dv ~ sub_cond * stim_version + Y + X + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df1, control = lmerControl(optimizer = "bobyqa"))
m26 <- lmer(dv ~ sub_cond * stim_version + Y + X + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df2, control = lmerControl(optimizer = "bobyqa"))
m27 <- lmer(dv ~ sub_cond * stim_version + Y + X + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df3, control = lmerControl(optimizer = "bobyqa"))
m28 <- lmer(dv ~ sub_cond * stim_version + Y + X + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df4, control = lmerControl(optimizer = "bobyqa"))
m29 <- lmer(dv ~ sub_cond * stim_version + Y + X + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df5, control = lmerControl(optimizer = "bobyqa"))
m30 <- lmer(dv ~ sub_cond * stim_version + Y + X + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df6, control = lmerControl(optimizer = "bobyqa"))
m31 <- lmer(dv ~ sub_cond * stim_version + Y + X + (1 + stim_version|sub_id) + (1 + stim_version * sub_cond|stim_id), data = df7, control = lmerControl(optimizer = "bobyqa"))


### CREATING A DATAFRAME WITH ESTIMATED EFFECT SIZES AND P-VALUES PER MODEL ###

intercept <- NULL
sub_condhard <- NULL
stim_versionincongruent <- NULL
sub_condhard_p <- NULL
stim_versionincongruent_p <- NULL
models <- c(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20, m21, m22, m23, m24, m25, m26, m27, m28, m29, m30, m31)
summary <- lapply(models, summary)
outliers <- c('none', 'visual1', 'visual2', 'interquartile', 'none', 'visual1', 'visual2', 'interquartile', 'none', 'visual1', 'visual2', 'interquartile', 'none', 'visual1', 'visual2', 'interquartile', 'none', 'visual1', 'visual2', 'interquartile', 'none', 'visual1', 'visual2', 'interquartile', 'none', 'visual1', 'visual2', 'interquartile', 'none', 'visual1', 'visual2', 'interquartile')
motivation_check <- c('no', 'no', 'no', 'no', 'yes', 'yes', 'yes', 'yes', 'no', 'no', 'no', 'no', 'yes', 'yes', 'yes', 'yes', 'no', 'no', 'no', 'no', 'yes', 'yes', 'yes', 'yes', 'no', 'no', 'no', 'no', 'yes', 'yes', 'yes', 'yes')
X <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)
Y <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
for (i in 1:32){
  intercept <- rbind(intercept, summary[[i]]$coefficients[1])
  sub_condhard <- rbind(sub_condhard, summary[[i]]$coefficients[2])
  stim_versionincongruent <- rbind(stim_versionincongruent, summary[[i]]$coefficients[1])
  #sub_condhard.stim_versionincongruent <- rbind(sub_condhard.stim_versionincongruent, summary[[i]]$coefficients[9])
  if ((X[i] == 0) & (Y[i] == 0)){
    sub_condhard_p <- rbind(sub_condhard_p, summary[[i]]$coefficients[18])
    stim_versionincongruent_p <- rbind(stim_versionincongruent_p, summary[[i]]$coefficients[19])
  } else if ((X[i] == 1) & (Y[i] == 0)){
    sub_condhard_p <- rbind(sub_condhard_p, summary[[i]]$coefficients[22])
    stim_versionincongruent_p <- rbind(stim_versionincongruent_p, summary[[i]]$coefficients[23])
  } else if ((X[i] == 0) & (Y[i] == 1)){
    sub_condhard_p <- rbind(sub_condhard_p, summary[[i]]$coefficients[22])
    stim_versionincongruent_p <- rbind(stim_versionincongruent_p, summary[[i]]$coefficients[23])
  } else if ((X[i] == 1) & (Y[i] == 1)){
    sub_condhard_p <- rbind(sub_condhard_p, summary[[i]]$coefficients[26])
    stim_versionincongruent_p <- rbind(stim_versionincongruent_p, summary[[i]]$coefficients[27])
  }
}
full <- data.frame(outliers, motivation_check, X, Y, intercept, sub_condhard, stim_versionincongruent, sub_condhard_p, stim_versionincongruent_p)

save(m31, file = 'm31.rdata')


# Plots to check data frame

ggplot(full, aes(x=outliers, colour = motivation_check, y=sub_condhard_p, label=sub_condhard_p)) + 
  geom_jitter(stat='identity', fill="black", size=4, alpha = 0.3, width = 0.2)  +
  labs(title="P-values sub_condhard", 
       color = 'motivation check') + 
  ylim(0, 0.1) +
  coord_flip() + 
  theme_bw() + 
  geom_hline(yintercept = 0.05, lty = 2) +
  xlab('Dealing with outliers') +
  ylab('P-values sub_condhard')

ggplot(full, aes(x = as.factor(X), colour = as.factor(Y), y=sub_condhard_p, label=sub_condhard_p)) + 
  geom_jitter(stat='identity', fill="black", size = 4, alpha = 0.3, width = 0.1)  +
  labs(title="P-values variable 'sub_condhard'", 
       color = 'Adding confounding variable Y') + 
  ylim(0, 0.1) +
  coord_flip() +
  theme_bw() + 
  geom_hline(yintercept = 0.05, lty = 2) +
  xlab('Adding confounding variable X') +
  ylab('P-values sub_condhard')




