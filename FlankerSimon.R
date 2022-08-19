


source("function/Flanker.R")
F_data = flanker(data_dir = "Data/Flanker")

source("function/Simon.R")
S_data = simon(data_dir = "Data/Simon")

# Delete RT <200 ms

F_data<-F_data[!(F_data$key_resp.rt < 0.200 | F_data$key_resp.rt > 1.5),]
S_data<-S_data[!(S_data$key_resp.rt < 0.200 | S_data$key_resp.rt > 1.5),]

#Inhibition Score

F_data <- F_data %>% group_by(participant,congruentcy) %>% 
  mutate(avg = mean(scale(key_resp.rt))) %>% 
  group_by(participant) %>% mutate(inhibition = min(avg)-max(avg))


S_data <- S_data %>% group_by(participant,congruentcy) %>%   
  mutate( avg = mean(scale(key_resp.rt))) %>%
  group_by(participant) %>% mutate(inhibition = min(avg)-max(avg))



