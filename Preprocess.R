# Dr Bernhard Angele & Zeynep G Ozkan, 2022

# Required libraries

packages= c("tidyverse", "ggpubr", "lme4", "readr","stringr","performance", "MASS") 

for(i in 1:length(packages)){
  
  if(packages[i] %in% rownames(installed.packages())==FALSE){
    install.packages(packages[i])
    library(packages[i], character.only=TRUE)
  }else{
    library(packages[i], character.only=TRUE)
  }
}

##### Functions #####

source("function/PreprocFunction.R")
source("function/get_words.R")
source("function/Fixation.R")
source("function/fixation_time_measures.R")
source("function/blinkTable.R")

##### Process Raw Data #####
raw_data = preproc(data_dir = "Data/Ben")
raw_data_fix = fixations(data_dir = "Data/Ben")
first_pass <- first_pass_measures(raw_data_fix)
raw_words = get_words(data_dir = "Data/Ben",sentence_start_x = 125)
gopast <- go_past(raw_data_fix)
tvt <- tvt(raw_data_fix)
blink_data <- blinkTable(data_dir = "Data/Ben")


###### Remove Practice Trials ######

raw_data <- raw_data %>% filter(trial_type != "practice")
raw_data_fix <- raw_data_fix %>% filter(trial_type != "practice" & !is.na(wordN))
raw_words <- raw_words %>% filter(trial_type != "practice")

###### Add Blinks ######
raw_data_fix <- raw_data_fix %>% group_by(sub,item) %>% mutate(min1 = min(blink1^2),
                                                               min2 = min(blink2^2),
                                                               min3 = min(blink3^2))
raw_data_fix$blink = NA
for(i in 1:nrow(raw_data_fix)){
  
  if(!is.na(raw_data_fix$blink1[i])){
    if(as.numeric(raw_data_fix$blink1[i])^2 == as.numeric(raw_data_fix$min1[i])){
      if(as.numeric(raw_data_fix$blink1[i]) > 0){
        raw_data_fix$blink[i] = "before"
      }else{
        if(as.numeric(raw_data_fix$blink1[i]) < 0){
          raw_data_fix$blink[i] = "after"
        }
      }
    }
  }
  
  
  if(!is.na(raw_data_fix$blink2[i])){
    if(as.numeric(raw_data_fix$blink2[i])^2 == as.numeric(raw_data_fix$min2[i])){
      if(as.numeric(raw_data_fix$blink2[i]) > 0){
        raw_data_fix$blink[i] = "before"
      }else{
        if(as.numeric(raw_data_fix$blink2[i]) < 0){
          raw_data_fix$blink[i] = "after"
        }
      }
    }
  }
  
  
  if(!is.na(raw_data_fix$blink3[i])){
    if(as.numeric(raw_data_fix$blink3[i])^2 == as.numeric(raw_data_fix$min3[i])){
      if(as.numeric(raw_data_fix$blink3[i]) > 0){
        raw_data_fix$blink[i] = "before"
      }else{
        if(as.numeric(raw_data_fix$blink1[i]) < 0){
          raw_data_fix$blink[i] = "after"
        }
      }
    }
  }
  
}#end
raw_data_fix <- raw_data_fix[ , -which(names(raw_data_fix) %in% c("blink1","blink2","blink3","min1","min2","min3"))]

raw_data_fix$delete <- NA
for(i in 1:nrow(raw_data_fix)){
  if(raw_data_fix$boundaryN[i] == raw_data_fix$wordN[i] && !is.na(raw_data_fix$blink[i])){
    raw_data_fix$delete[i] <- 1
  }else{
    if(raw_data_fix$boundaryN[i]+1 == raw_data_fix$wordN[i] && !is.na(raw_data_fix$blink[i])){
      if(raw_data_fix$blink[i] == "before"){
        raw_data_fix$delete[i] <- 1
      }
    }else{
      if(raw_data_fix$boundaryN[i]-1 == raw_data_fix$wordN[i] && !is.na(raw_data_fix$blink[i])){
        if(raw_data_fix$blink[i] == "after"){
          raw_data_fix$delete[i] <- 1
        }
      }
    }
  }
}

###### Adding skipping ######

words_fixs <-full_join(raw_words,first_pass, by=c("item", "sub","wordN"))
words_fixs$skipping <- NA
for(i in 1:nrow(words_fixs)){
  if(is.na(words_fixs$ffd[i])){
    words_fixs$skipping[i] <- 'TRUE'
  }else{
    words_fixs$skipping[i] <- 'FALSE'
  }
}

###### Condition Added ######

words_fixs <- words_fixs %>% left_join(raw_data %>% dplyr::select(sub,item,target_changed))
words_fixs <- words_fixs[ , -which(names(words_fixs) %in% c("cond"))]


###### Adding inhibition score ######

inhibition <- read_csv("inhibition_scores_by_participant.csv")

fixation_time_measures <- words_fixs %>%  left_join(inhibition, by = c("sub" = "participant")) %>%
  mutate(ffd = as.numeric(ffd), sfd = as.numeric(sfd), skipping = factor(skipping))

###### Adding GP and TVT ######

fixation_time_measures <- fixation_time_measures %>% left_join(gopast,by=c("item","sub","wordN"))
fixation_time_measures <- fixation_time_measures %>% left_join(tvt,by=c("item","sub","wordN"))

##### Extract display latency and blinks #####

rateaccuracy <- fixation_time_measures %>% left_join(raw_data, by = c("sub", "item")) %>% 
  group_by(sub)%>% summarise(accuracy = mean(accuracy,na.rm=TRUE))

DCLate <- subset(raw_data, raw_data$Display_lat < 0)
DCLate <- subset(DCLate, DCLate$target_changed != "identical")
DCLate <- DCLate %>% group_by(sub,item) %>% summarise()

DCLate1 <- subset(raw_data, raw_data$Display_lat > 4)
DCLate1 <- subset(DCLate1, DCLate1$target_changed != "identical")
DCLate1 <- DCLate %>% group_by(sub,item) %>% summarise()

DCLate2 <- subset(raw_data, raw_data$Display_time > 10)
DCLate2 <- subset(DCLate2, DCLate2$target_changed != "identical")
DCLate2 <- DCLate2 %>% group_by(sub,item) %>% summarise()

blink <- subset(raw_data_fix,raw_data_fix$delete == 1)
blink <- blink %>% group_by(sub,item) %>% summarise()

sentencesskips <- fixation_time_measures %>% group_by(sub,item) %>% mutate(skip = as.numeric(skipping)-1) %>% summarise(rate = mean(skip,na.rm=TRUE))
sentencesskips <- subset(sentencesskips, rate > 0.7)
deleted <- rbind(DCLate,DCLate1,DCLate2,blink,sentencesskips)
deleted$delete <- 1


fixation_time_measures_withdelete <- left_join(fixation_time_measures,deleted, by=c("sub","item"))
fixation_time_measures_withdelete <- subset(fixation_time_measures_withdelete, is.na(fixation_time_measures_withdelete$delete))


##### saving and writing raw datas #####

save(raw_data, file= "Data/raw_data.Rda")

save(raw_data_fix, file= "Data/raw_data_fix.Rda")

save(raw_words, file= "Data/raw_words.Rda")

save(gopast, file= "Data/gopast.Rda")

save(tvt, file= "Data/tvt.Rda")

save(first_pass, file= "Data/first_pass.Rda")

save(blink_data, file= "Data/blink_data.Rda")

save(fixation_time_measures, file= "Data/fixation_time_measures.Rda")

write.csv(fixation_time_measures, "Data/fixation_time_measures.csv")

save(words_fixs, file= "Data/words_fixs.Rda")

write.csv(words_fixs, file= "Data/words_fixs.csv")

save(fixation_time_measures_withdelete, file= "Data/fixation_time_measures_withdelete.Rda")

write.csv(fixation_time_measures_withdelete, file= "Data/fixation_time_measures_withdelete.csv")

