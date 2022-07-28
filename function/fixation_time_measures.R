
first_pass_measures <- function(fixation_data){
  df <- fixation_data %>% group_by(sub, item) %>%
  mutate(word_max = cummax(wordN),
         is_max = wordN == word_max,
         first_pass = is_max & lag(is_max, default = TRUE)) %>%
  group_by(sub, item, wordN) %>%
    mutate(first_pass = cummin(first_pass) %>% as.logical()) %>% # a fixation on a word cannot be first pass after there was a second pass fixation on that word in the same trial
  filter(first_pass) %>%
  summarise(ffd = fix_dur[1], gd = sum(as.numeric(fix_dur))) %>%
    mutate(sfd = ifelse(ffd == gd, ffd, NA))
  return(df)
}

go_past <- function(fixation_data){
  df <- fixation_data %>% group_by(sub, item) %>%
    mutate(word_max = cummax(wordN)) %>%
    group_by(sub, item, word_max) %>%
    summarise(gopast = sum(as.numeric(fix_dur)))
  return(df)
}

tvt <- function(fixation_data){
  df <- fixation_data %>% group_by(sub, item, wordN) %>%
    summarise(tvt = sum(as.numeric(fix_dur)))
  return(df)
}
