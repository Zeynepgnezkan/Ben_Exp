library(tidyverse)


source("function/Flanker.R")
F_data = flanker(data_dir = "Data/Flanker")

source("function/Simon.R")
S_data = simon(data_dir = "Data/Simon")

# Delete RT <200 ms

F_data<-F_data %>% filter(!(key_resp.rt < 0.200 | key_resp.rt > 1.5)) %>% separate(images, into = c("congruency","side"), sep = "[ -]") 
S_data<-S_data %>% filter(!(key_resp.rt < 0.200 | key_resp.rt > 1.5)) %>% separate(images, into = c("colour","congruency"), sep = "_") 

#Inhibition Score

F_data <- F_data %>% group_by(participant,congruency) %>% 
  summarise(avg = mean(key_resp.rt)) %>%
  pivot_wider(names_from = congruency, values_from = avg) %>%
  mutate(flanker_effect = incongruent - congruent)


S_data <- S_data %>% group_by(participant,congruency) %>% 
  summarise(avg = mean(key_resp.rt)) %>%
  pivot_wider(names_from = congruency, values_from = avg) %>%
  mutate(simon_effect = incongruent - congruent)


inhibition <- F_data %>% left_join(S_data, by = "participant", suffix = c("_flanker", "_simon")) %>%
  mutate(inhibition_score = mean(c(flanker_effect, simon_effect)))

write_csv(inhibition, "inhibition_scores_by_participant.csv")
