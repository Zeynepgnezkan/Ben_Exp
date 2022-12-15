
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

# Load Data

load("Data/fixation_time_measures_withdelete.Rda")

# set contrasts for preview manipulation

fixation_time_measures_withdelete$target_changed = as.factor(fixation_time_measures_withdelete$target_changed)
#levels(fixation_time_measures_withdelete$target_changed) 
mycontrast <- cbind(identical_vs_diff = c(.5,.5,-1), ben_vs_bir = c(-1,1,0))
mycontrastNames <- colnames(mycontrast)
mycontrast <- zapsmall(t(ginv(mycontrast)))
colnames(mycontrast) = mycontrastNames
contrasts(fixation_time_measures_withdelete$target_changed) = mycontrast


# BRMS: Set priors 
priors <- set_prior("normal(0,100)", class = "b")

# BRMS: functions

### ??? I tried to add bourndary and data as a argument inside of the function so we dont have to add
### ??? data = fixation_time_measures %>% filter(wordN == boundaryN + 1) everytime but I couldn't do it. 
### ??? how can we do it like that ???

# you can't just paste a command into a string, you'd have to evaluate it
# on the other hand, bf() converts strings into formulas (if they are in the . ~ . format), so using paste here is fine

blmm_ben <- function(dataA, word_n, dv, warmup = 1000, iter = 5000, chains = 4, cores = 4, adapt_delta = 0.8, prior = priors){
  blm <- brm(
    data = dataA %>% filter(wordN == boundaryN + word_n),
      # it works like this: word_n can be 0 for the target word or -1, +1, etc. for words relative to the target word
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

blmm_ben_skip <- function(dataA, word_n, warmup = 1000, iter = 5000, chains = 4, cores = 4, adapt_delta = 0.8, prior = priors){
  # now word_n can be -1, 0, or +1 depending on which target word we want 
  blm <- brm(
    data = dataA %>% filter(wordN == boundaryN + word_n),
      # it works like this: word_n can be 0 for the target word or -1, +1, etc. for words relative to the target word,
    formula = bf(
      as.numeric(skipping) ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item)
    ),
    warmup = warmup,
    iter = iter,
    chains = chains,
    prior = priors,
    sample_prior = "yes",
    family = bernoulli(link = "logit"),
    init = "0",
    control = list(adapt_delta = adapt_delta),
    cores = cores,
    backend = "cmdstanr",
    threads = threading(4),
    seed = "6122022"
  )
  return(blm)
}



# First Fixation Duration / Target word (N)

blm_ffd_n <- blmm_ben(dataA = fixation_time_measures_withdelete, word_n = 0, dv = "ffd", warmup = 1000, iter = 5000)
save(blmm_ffd_n, "blmm_ffd_n.RData")
summary(blm_ffd_n)

# Skipping / Word N

# Variable is coded such that higher = more skipping

#lm_skip <- glmer(data = fixation_time_measures %>% filter(wordN == boundaryN ), skipping ~ target_changed * scale(inhibition_score) + (1|sub), family = binomial(link = "logit")) 
blm_skip_n <- blmm_ben_skip(data = fixation_time_measures_withdelete, word_n = 0, warmup = 1000, iter = 5000)
save(blm_skip_n, "blm_skip_n.RData")
summary(blm_skip_n)


# Gaze Duration / Word N

blm_gd_n <- blmm_ben(data = fixation_time_measures_withdelete ,word_n = 0, dv = "gd", warmup = 1000, iter = 5000)
save(blm_gd_n, "blm_gd_n.RData")
summary(blm_gd_n)


# Go-past Time / Word N

blm_gp_n <- blmm_ben(data = fixation_time_measures_withdelete, word_n = 0, dv = "gopast", warmup = 1000, iter = 5000)
save(blm_gp_n, "blm_gp_n.RData")
summary(blm_gp_n)


# Total View Time / Word N

blm_tvt_n <- blmm_ben(data = fixation_time_measures_withdelete ,word_n = 0, dv = "tvt", warmup = 1000, iter = 5000)
save(blm_tvt_n, "blm_tvt_n.RData")
summary(blm_tvt_n)

### N+1 ###

# First fixation duration / Word N + 1

blm_ffd_n1 <- blmm_ben(data = fixation_time_measures_withdelete, word_n = 1, dv = "ffd", warmup = 1000, iter = 5000)
save(blmm_ffd_n1, "blmm_ffd_n1.RData")
summary(blm_ffd_n1)

# Skipping / Word N + 1

#lm_skip1 <- glmer(data = fixation_time_measures %>% filter(wordN == boundaryN + 1), skipping ~ target_changed * scale(inhibition_score) + (1|sub), family = binomial(link = "logit")) 
blm_skip_n1 <- blmm_ben_skip(data = fixation_time_measures_withdelete, word_n = 1, warmup = 1000, iter = 5000)
save(blm_skip_n1, "blm_skip_n1.RData")
summary(blm_skip_n1)

# Gaze duration / Word N + 1

blm_gd_n1 <- blmm_ben(data = fixation_time_measures_withdelete, word_n = 1, dv = "gd", warmup = 1000, iter = 5000)
save(blm_gd_n1, "blm_gd_n1.RData")
summary(blm_gd_n1)


# Go-past Time / Word N + 1

blm_gp_n1 <- blmm_ben(data = fixation_time_measures_withdelete, word = (boundaryN + 1), dv = "gp", warmup = 10, iter = 50)
save(blm_gp_n1, "blm_gp_n1.RData")
summary(blm_gp_n1)

# TVT / Word N + 1

blm_tvt_n1 <- blmm_ben(data = fixation_time_measures_withdelete, word = (boundaryN + 1), dv = "tvt", warmup = 10, iter = 50)
save(blm_tvt_n1, "blm_tvt_n1.RData")
summary(blm_tvt_n1)


# Gaze Duration / Word N - 1

blm_gd_n1m <- blmm_ben(data = fixation_time_measures_withdelete, word_n = - 1, dv = "gd", warmup = 1000, iter = 5000)
save(blm_gd_n1m, "blm_gd_n1m.RData")
summary(blm_tvt_n1m)

# Skipping N -1

#lm_skip1m <- glmer(data = fixation_time_measures %>% filter(wordN == boundaryN - 1), skipping ~ target_changed * scale(inhibition_score) + (1|sub), family = binomial(link = "logit")) 

blm_skip_n1m <- blmm_ben_skip(data = fixation_time_measures_withdelete, word_n = -1, warmup = 1000, iter = 5000)
save(blm_skip_n1m, "blm_skip_n1m.RData")
summary(blm_skip_n1m)

