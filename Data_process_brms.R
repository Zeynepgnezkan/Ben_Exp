
# Required libraries
library(stringr)
library(tidyverse)
library(MASS)
library(lme4)
library(readr)
library(ggpubr)
library(dplyr)
library(brms)
library(cmdstanr)

# BRMS: Set priors

iter <- 5000
warmup <- 1000
chains <- 4

priors <- set_prior("normal(0,100)", class = "b")

# First Fixation Duration / Word N

blm_ffd <- brm(
  data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN),
  formula = bf(
    ffd ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item),  
    beta ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item)
  ),
  warmup = warmup,
  iter = iter,
  chains = chains,
  prior = priors,
  sample_prior = "yes",
  family = exgaussian(),
  init = "0",
  control = list(adapt_delta = 0.8),
  cores = 4,
  #backend = "cmdstanr",
  #threads = threading(4),
  seed = "6122022"
)

summary(blm_ffd)
# Skipping

lm_skip <- glmer(data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN ), skipping ~ target_changed * scale(inhibition_score) + (1|sub), family = binomial(link = "logit")) 
summary(lm_skip)



# Gaze Duration / Word N

blm_gd <- brm(
  data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN),
  formula = bf(
    gd ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item),  
    beta ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item)),
  warmup = warmup,
  iter = iter,
  chains = chains,
  prior = priors,
  sample_prior = "yes",
  family = exgaussian(),
  init = "0",
  control = list(adapt_delta = 0.8),
  cores = 4,
  backend = "cmdstanr",
  threads = threading(4),
  seed = "6122022"
)

summary(blm_gd)

# Go-past Time / Word N

blm_gp <- brm(
  data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN),
  formula = bf(
    gp ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item),  
    beta ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item)),
  warmup = warmup,
  iter = iter,
  chains = chains,
  prior = priors,
  sample_prior = "yes",
  family = exgaussian(),
  init = "0",
  control = list(adapt_delta = 0.8),
  cores = 4,
  backend = "cmdstanr",
  threads = threading(4),
  seed = "6122022"
)

summary(blm_gp)

# Total View Time / Word N

blm_tvt <- brm(
  data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN),
  formula = bf(
    tvt ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item),  
    beta ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item)),
  warmup = warmup,
  iter = iter,
  chains = chains,
  prior = priors,
  sample_prior = "yes",
  family = exgaussian(),
  init = "0",
  control = list(adapt_delta = 0.8),
  cores = 4,
  backend = "cmdstanr",
  threads = threading(4),
  seed = "6122022"
)

summary(blm_tvt)

### N+1 ###

# First fixation duration / Word N + 1

blm_ffd1 <- brm(
  data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN + 1),
  formula = bf(
    ffd ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item),  
    beta ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item)),
  warmup = warmup,
  iter = iter,
  chains = chains,
  prior = priors,
  sample_prior = "yes",
  family = exgaussian(),
  init = "0",
  control = list(adapt_delta = 0.8),
  cores = 4,
  backend = "cmdstanr",
  threads = threading(4),
  seed = "6122022"
)

summary(blm_ffd1)

# Skipping

lm_skip1 <- glmer(data = fixation_time_measures %>% filter(wordN == boundaryN + 1), skipping ~ target_changed * scale(inhibition_score) + (1|sub), family = binomial(link = "logit")) 
summary(lm_skip1)

# Gaze duration / Word N + 1

blm_gd1 <- brm(
  data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN + 1),
  formula = bf(
    gd ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item),  
    beta ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item)),
  warmup = warmup,
  iter = iter,
  chains = chains,
  prior = priors,
  sample_prior = "yes",
  family = exgaussian(),
  init = "0",
  control = list(adapt_delta = 0.8),
  cores = 4,
  backend = "cmdstanr",
  threads = threading(4),
  seed = "6122022"
)

summary(blm_gd1)

# Go-past Time

blm_gp1 <- brm(
  data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN + 1),
  formula = bf(
    gopast ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item),  
    beta ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item)),
  warmup = warmup,
  iter = iter,
  chains = chains,
  prior = priors,
  sample_prior = "yes",
  family = exgaussian(),
  init = "0",
  control = list(adapt_delta = 0.8),
  cores = 4,
  backend = "cmdstanr",
  threads = threading(4),
  seed = "6122022"
)

summary(blm_gp1)

## TVT 

blm_tvt1 <- brm(
  data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN + 1),
  formula = bf(
    tvt ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item),  
    beta ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item)),
  warmup = warmup,
  iter = iter,
  chains = chains,
  prior = priors,
  sample_prior = "yes",
  family = exgaussian(),
  init = "0",
  control = list(adapt_delta = 0.8),
  cores = 4,
  backend = "cmdstanr",
  threads = threading(4),
  seed = "6122022"
)

summary(blm_tvt1)

# Gaze Duration / Word N - 1

blm_gd1m <- brm(
  data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN - 1),
  formula = bf(
    gd ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item),  
    beta ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item)),
  warmup = warmup,
  iter = iter,
  chains = chains,
  prior = priors,
  sample_prior = "yes",
  family = exgaussian(),
  init = "0",
  control = list(adapt_delta = 0.8),
  cores = 4,
  backend = "cmdstanr",
  threads = threading(4),
  seed = "6122022"
)

summary(blm_gd1m)

# Skipping N -1

lm_skip1m <- glmer(data = fixation_time_measures %>% filter(wordN == boundaryN - 1), skipping ~ target_changed * scale(inhibition_score) + (1|sub), family = binomial(link = "logit")) 
summary(lm_skip1m)







