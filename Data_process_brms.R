
# Required libraries
library(stringr)
library(tidyverse)
library(MASS)
library(lme4)
library(readr)
library(ggpubr)
library(dplyr)
library(Rcpp)
library(brms)
library(cmdstanr)

# BRMS: Set priors

blmm_ben <- function(data, dv, warmup = 1000, iter = 5000, chains = 4, cores = 4, adapt_delta = 0.8, prior = priors){
  blm <- brm(
    data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN),
    formula = bf(
      paste(dv, "~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item)"),  
      beta ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item)
    ),
    warmup = warmup,
    iter = iter,
    chains = chains,
    prior = priors,
    sample_prior = "yes",
    family = exgaussian(),
    init = "0",
    control = list(adapt_delta = adapt_delta),
    cores = cores,
    backend = "cmdstanr",
    threads = threading(4),
    seed = "6122022"
  )
  return(blm)
}

blmm_ben_skip <- function(data, dv, warmup = 1000, iter = 5000, chains = 4, cores = 4, adapt_delta = 0.8, prior = priors){
  blm <- brm(
    data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN),
    formula = bf(
      skipping ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item)
    ),
    warmup = warmup,
    iter = iter,
    chains = chains,
    prior = priors,
    sample_prior = "yes",
    family = bernoulli(),
    init = "0",
    control = list(adapt_delta = adapt_delta),
    cores = cores,
    backend = "cmdstanr",
    threads = threading(4),
    seed = "6122022"
  )
  return(blm)
}


iter <- 500
warmup <- 10
chains <- 4

priors <- set_prior("normal(0,100)", class = "b")


# First Fixation Duration / Word N

blm_ffd 
summary(blm_ffd)

# Skipping

lm_skip <- glmer(data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN ), skipping ~ target_changed * scale(inhibition_score) + (1|sub), family = binomial(link = "logit")) 

blm_ffd_n <- blmm_ben(data = fixation_time_measures %>% filter(wordN == boundaryN), dv = "ffd", warmup = 100, iter = 500)
save(blmm_ffd, "blmm_ffd_n.RData")
summary(blm_ffd)

# Skipping

lm_skip <- glmer(data = fixation_time_measures %>% filter(wordN == boundaryN ), skipping ~ target_changed * scale(inhibition_score) + (1|sub), family = binomial(link = "logit")) 
blmm_ben_skip(data = fixation_time_measures %>% filter(wordN == boundaryN, warmup = 10, iter = 50))

summary(lm_skip)

# Gaze Duration / Word N

blm_gd_n <- blmm_ben(data = fixation_time_measures %>% filter(wordN == boundaryN), dv = "gd", warmup = 1000, iter = 5000)
save(blmm_gd, "blmm_gd_n.RData")
# Go-past Time


blm_gd 

summary(blm_gd)

# Go-past Time / Word N

blm_gp 
summary(blm_gp)

# Total View Time / Word N

blm_tvt 

summary(blm_tvt)

### N+1 ###

# First fixation duration / Word N + 1

blm_ffd1 
summary(blm_ffd1)

blm_ffd_n1 <- blmm_ben(data = fixation_time_measures %>% filter(wordN == boundaryN + 1), dv = "ffd", warmup = 1000, iter = 5000)
save(blmm_ffd_1, "blmm_ffd_n1.RData")
summary(blm_ffd_n1)

# Skipping

lm_skip1 <- glmer(data = fixation_time_measures %>% filter(wordN == boundaryN + 1), skipping ~ target_changed * scale(inhibition_score) + (1|sub), family = binomial(link = "logit")) 
summary(lm_skip1)

# Gaze duration / Word N + 1

blm_gd1 
summary(blm_gd1)

# Go-past Time

blm_gp1 

summary(blm_gp1)

## TVT 

blm_tvt1 
summary(blm_tvt1)

# Gaze Duration / Word N - 1

blm_gd1m 

summary(blm_gd1m)

# Skipping N -1

lm_skip1m <- glmer(data = fixation_time_measures %>% filter(wordN == boundaryN - 1), skipping ~ target_changed * scale(inhibition_score) + (1|sub), family = binomial(link = "logit")) 
summary(lm_skip1m)







