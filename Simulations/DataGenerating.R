# Simulates the data used in the multiverse analyses
# Partially based on https://debruine.github.io/tutorials/sim-lmer.html

setwd('C:\\Users\\herre\\OneDrive\\Documenten\\GitHub\\thesis_MultiverseAnalysis\\Simulations')
rm(list = ls(all = TRUE))

# Load packages

library(tidyverse)  # Tidy data
library(lmerTest)   # Mixed effects models 
library(faux)       # Simulating correlated variables
library(GGally)

# Variable values

set.seed(1)

sub_n  <- 200  # Number of subjects in this simulation
sub_sd <- 100  # SD for the subjects' random intercept

stim_n  <- 50  # Number of stimuli in this simulation
stim_sd <- 50  # SD for the stimuli's random intercept

grand_i          <- 1000  # Overall mean DV
sub_cond_eff     <- 50  # Mean difference between conditions: hard - easy
stim_version_eff <- 50  # Mean difference between versions: incongruent - congruent
cond_version_ixn <- 0.7  # Interaction between version and condition
error_sd         <- 25  # Residual (error) SD

sub_version_sd <- 20  # Variation in size of effect of stim_version_eff
sub_i_version_cor <- -0.2  # Correlation between random intercept and random slope

stim_version_sd <- 10  # SD for the stimuli's random slope for stim_version
stim_cond_sd <- 30  # SD for the stimuli's random slope for sub_cond
stim_cond_version_sd <- 15  # SD for the stimuli's random slope for sub_cond:stim_version
stim_i_cor <- -0.4  # Correlations between intercept and slopes
stim_s_cor <- +0.2  # Correlations among slopes

IQ_mean <- 100  # IQ mean 
IQ_sd <- 15  # IQ sd

X_mean <- 0
X_sd <- 100


### Random intercepts & slopes ###


# 1 - Subjects

sub <- faux::rnorm_multi(
  n = sub_n, 
  vars = 2, 
  r = sub_i_version_cor,
  mu = 0, # means of random intercepts and slopes are always 0
  sd = c(sub_sd, sub_version_sd),
  varnames = c("sub_i", "sub_version_slope")
) %>%
  mutate(
    sub_id = 1:sub_n,
    sub_cond = rep(c("easy","hard"), each = sub_n/2) # between-subjects factor
  )

ggplot(sub, aes(sub_i, sub_version_slope, color = sub_cond)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(sub, aes(sub_i, color = sub_cond)) +
  geom_density()

# 2 - Stimuli

stim_cors <- c(stim_i_cor, stim_i_cor, stim_i_cor,
               stim_s_cor, stim_s_cor,
               stim_s_cor)
stim <- rnorm_multi(
  n = stim_n, 
  vars = 4, 
  r = stim_cors, 
  mu = 0,  # Means of random intercepts and slopes are always 0
  sd = c(stim_sd, stim_version_sd, stim_cond_sd, stim_cond_version_sd),
  varnames = c("stim_i", "stim_version_slope", "stim_cond_slope", "stim_cond_version_slope")
) %>%
  mutate(
    stim_id = 1:stim_n
  )

GGally::ggpairs(stim, columns = 1:4, 
                lower = list(continuous = "smooth"),
                progress = FALSE)

# 3 - Trials

trials <- crossing(
  sub_id = sub$sub_id, # Get subject IDs from the sub data table
  stim_id = stim$stim_id, # Get stimulus IDs from the stim data table
  stim_version = c("congruent", "incongruent") # All subjects see both congruent and incongruent versions of all stimuli
) %>%
  left_join(sub, by = "sub_id") %>% # Includes the intercept and condition for each subject
  left_join(stim, by = "stim_id")   # Includes the intercept for each stimulus


### Calculate dependent variables ###


df <- trials %>%
  mutate(
    # effect-code subject condition and stimulus version
    sub_cond.e = recode(sub_cond, "hard" = -0.5, "easy" = +0.5),
    stim_version.e = recode(stim_version, "congruent" = -0.5, "incongruent" = +0.5),
    # calculate trial-specific effects by adding overall effects and slopes
    cond_eff = sub_cond_eff + stim_cond_slope,
    version_eff = stim_version_eff + stim_version_slope + sub_version_slope,
    cond_version_eff = cond_version_ixn + stim_cond_version_slope,
    # calculate error term (normally distributed residual with SD set above)
    err = rnorm(nrow(.), 0, error_sd),
    # calculate DV from intercepts, effects, and error
    dv = grand_i + sub_i + stim_i + err +
      (sub_cond.e * cond_eff) + 
      (stim_version.e * version_eff) + 
      (sub_cond.e * stim_version.e * cond_version_eff)
  )

ggplot(df, aes(x = sub_cond, y = dv, color = stim_version)) +
  geom_hline(yintercept = grand_i) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.9))

# Add IQ as a related variable

df$IQ <- NULL
for (i in unique(df$sub_id)){
  standardized <- (df$dv[df$sub_id == i] - mean(df$dv)) / sd(df$dv)
  df$IQ[df$sub_id == i] <- round(rnorm(1, mean = standardized + IQ_mean, sd = IQ_sd))
}

# Add X as a confounding variable

df$X <- NULL
for (n in unique(df$sub_id)){
  for (i in unique(df$stim_id)){
    for (j in unique(df$stim_version)){
      
      df$X[(df$stim_id == i) & (df$stim_version == j) & (df$sub_id == n)] <- rnorm(1, X_mean + 10 * df$stim_version.e[(df$stim_id == i) & (df$stim_version == j) & (df$sub_id == n)], X_sd)
      df$dv[(df$stim_id == i) & (df$stim_version == j) & (df$sub_id == n)] <- df$dv[(df$stim_id == i) & (df$stim_version == j) & (df$sub_id == n)] + df$X[(df$stim_id == i) & (df$stim_version == j) & (df$sub_id == n)] * rnorm(1, 0.5, 1)
      
    }
  }
}

# Add random outliers

for (i in 1:nrow(df)){
  noise1 <- rnorm(1, mean = 1000, sd = 300)
  while (noise1 < 0){
    noise1 <- rnorm(1, mean = 1000, sd = 300)
  }
  noise2 <- rnorm(1, mean = 2000, sd = 300)
  while (noise2 < 0){
    noise2 <- rnorm(1, mean = 2000, sd = 300)
  }
  add_noise1 <- ifelse(runif(1,0,1) > 0.999, 1, 0)
  add_noise2 <- ifelse(runif(1,0,1) > 0.9999, 1, 0)
  df$dv[i] <- df$dv[i] + add_noise1 * noise1 + add_noise2 * noise2
}

# Left vs. right response

df$resp <- NULL
df$resp_mean <- NULL

for (n in unique(df$sub_id)){
  
  mean <- runif(1, 0.1, 0.9)
  
  for (i in unique(df$stim_id)){
    for (j in unique(df$stim_version)){
      
      df$resp[(df$stim_id == i) & (df$stim_version == j) & (df$sub_id == n)] <- ifelse(rbernoulli(1, p = mean), 'left', 'right')
      
    }
  }
  
  df$resp_mean[df$sub_id == n] <- prop.table(table(df$resp[df$sub_id == n]))[1]
}



# Save data

save(df, file = 'sim.rdata')


### Analysis (sense checks) ###


mod <- lmer(dv ~ sub_cond.e * stim_version.e +
              (1 + stim_version.e | sub_id) + 
              (1 + stim_version.e*sub_cond.e | stim_id),
            data = df)

mod.sum <- summary(mod)

mod.sum

mod.sum$ngrps
mod.sum$varcor
mod.sum$coefficients

ranef(mod)$sub_id %>%
  as_tibble(rownames = "sub_id") %>%
  rename(mod_sub_i = `(Intercept)`) %>%
  mutate(sub_id = as.integer(sub_id)) %>%
  left_join(sub, by = "sub_id") %>%
  ggplot(aes(sub_i,mod_sub_i)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Simulated random intercepts (sub_i)") +
  ylab("Modeled random intercepts")

ranef(mod)$stim_id %>%
  as_tibble(rownames = "stim_id") %>%
  rename(mod_stim_i = `(Intercept)`) %>%
  mutate(stim_id = as.integer(stim_id)) %>%
  left_join(stim, by = "stim_id") %>%
  ggplot(aes(stim_i,mod_stim_i)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Simulated random intercepts (stim_i)") +
  ylab("Modeled random intercepts")

# Plot the subject intercepts and slopes from our code above (sub$sub_i) against the subject intercepts and slopes calculcated by lmer (ranef(mod)$sub_id)

ranef(mod)$sub_id %>%
  as_tibble(rownames = "sub_id") %>%
  rename(mod_i = `(Intercept)`,
         mod_version_slope = stim_version.e) %>%
  mutate(sub_id = as.integer(sub_id)) %>%
  left_join(sub, by = "sub_id") %>%
  select(mod_i, sub_i, 
         mod_version_slope,  sub_version_slope) %>%
  GGally::ggpairs(lower = list(continuous = "smooth"),
                  progress = FALSE)

# Plot the stimulus intercepts and slopes from our code above (stim$stim_i) against the stimulus intercepts and slopes calculcated by lmer (ranef(mod)$stim_id).

ranef(mod)$stim_id %>%
  as_tibble(rownames = "stim_id") %>%
  rename(mod_i = `(Intercept)`,
         mod_version_slope = stim_version.e,
         mod_cond_slope = sub_cond.e,
         mod_cond_version_slope = `stim_version.e:sub_cond.e`) %>%
  mutate(stim_id = as.integer(stim_id)) %>%
  left_join(stim, by = "stim_id") %>%
  select(mod_i, stim_i, 
         mod_version_slope, stim_version_slope, 
         mod_cond_slope, stim_cond_slope, 
         mod_cond_version_slope, stim_cond_version_slope) %>%
  GGally::ggpairs(lower = list(continuous = "smooth"),
                  progress = FALSE)
