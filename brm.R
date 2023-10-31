


priors <- set_prior("normal(0,100)", class = "b")
iter = 5000
warmup = 1000

## For Target

blm_ffd_n <- brm(
  formula = ffd ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item),
  data = fixation_time_measures_withdelete,
  chains = 4,     
  iter = iter,      
  warmup = warmup,    
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.9),
  seed = 12345,
  prior = priors,
  family = exgaussian(),
)

blm_gd_n <- brm(
  formula = gd ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item),
  data = fixation_time_measures_withdelete,
  chains = 4,     
  iter = iter,      
  warmup = warmup,    
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.9),
  seed = 12345,
  prior = priors,
  family = exgaussian(),
)


blm_skip_n <- brm(
  formula = gd ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item),
  data = fixation_time_measures_withdelete,
  chains = 4,     
  iter = iter,      
  warmup = warmup,    
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.9),
  seed = 12345,
  prior = priors,
  family = bernoulli(),
)


blm_ffd_n <- brm(
  formula = ffd ~ target_changed * scale(inhibition_score) + (target_changed|sub) + (target_changed * scale(inhibition_score)|item),
  data = fixation_time_measures_withdelete,
  chains = 4,     
  iter = iter,      
  warmup = warmup,    
  cores = parallel::detectCores(),
  control = list(adapt_delta = 0.9),
  seed = 12345,
  prior = priors,
  family = exgaussian(),
)

