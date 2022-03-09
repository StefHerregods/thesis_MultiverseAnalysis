# Simulates the data used in the multiverse analyses
# Partially based on https://debruine.github.io/tutorials/sim-lmer.html

# Load packages

library(tidyverse)  # Tidy data
library(lmerTest)   # Mixed effects models 
library(faux)       # Simulating correlated variables

# Variable values

set.seed(1)

sub_n  <- 200 # Number of subjects in this simulation
sub_sd <- 100 # SD for the subjects' random intercept

stim_n  <- 50 # Number of stimuli in this simulation
stim_sd <- 50 # SD for the stimuli's random intercept


### Random intercepts ###


# 1 - Subjects

sub <- tibble(
  sub_id = 1:sub_n,
  sub_i  = rnorm(sub_n, 0, sub_sd), # Random intercept
  sub_cond = rep(c("easy","hard"), each = sub_n/2) # Between-subjects factor
)

ggplot(sub, aes(sub_i, color = sub_cond)) +
  geom_density()

# 2 - Stimuli

stim <- tibble(
  stim_id = 1:stim_n,
  stim_i = rnorm(stim_n, 0, stim_sd) # random intercept
)

ggplot(stim, aes(stim_i)) +
  geom_density()

# 3 - Trials

trials <- crossing(
  sub_id = sub$sub_id, # get subject IDs from the sub data table
  stim_id = stim$stim_id, # get stimulus IDs from the stim data table
  stim_version = c("congruent", "incongruent") # all subjects see both congruent and incongruent versions of all stimuli
) %>%
  left_join(sub, by = "sub_id") %>% # includes the intercept and conditin for each subject
  left_join(stim, by = "stim_id")   # includes the intercept for each stimulus


### Calculate dependent variables ###
