library(ggplot2)
library(tidyquant)
library(tidyverse)
library(ggforce)
library(ggdist)
library(dplyr)
library(MASS)
library(lme4)
library(reshape)

source("function/Code.R")
raw_data = preproc(data_dir = "Data/Ben")

source("function/Fixation.R")
raw_data2 = fixations(data_dir = "Data/Ben")

raw_data2 <- raw_data2 %>% filter(trial_type != "practice" & !is.na(wordN))

source("function/fixation_time_measures.R")
first_pass <- first_pass_measures(raw_data2)


source("function/get_words.R")
raw_words = get_words(data_dir = "Data/Ben")
raw_words <- raw_words %>% filter(trial_type != "practice")

gopast <- go_past(raw_data2)

tvt <- tvt(raw_data2)

save(raw_data, file= "data/raw_data.Rda")

write.csv(raw_data, "data/raw_data.csv")

save(raw_data2, file= "data/raw_data2.Rda")

write.csv(raw_data2, "data/raw_data2.csv")

save(raw_words, file= "data/raw_words.Rda")

write.csv(raw_words, "data/raw_words.csv")


# word fix match
words_fixs <-full_join(raw_words,first_pass, by=c("item", "sub","wordN"))
words_fixs$skipping <- NA
for(i in 1:nrow(words_fixs)){
  if(is.na(words_fixs$ffd[i])){
    words_fixs$skipping[i] <- 'TRUE'
  }else{
    words_fixs$skipping[i] <- 'FALSE'
  }
}

##############################
library("viridis") 

pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F")
ggplot(raw_data, aes(x= cond, y=fix_dur))+
  geom_jitter()+
  theme_minimal()

ggplot(dat, aes(x= cond, fix_dur, fill=cond)) +
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.4) + 
  geom_boxplot(width = .1, outlier.shape = NA) +
  ggdist::stat_dots(side = "left", dotsize = .3, justification = 1.1, binwidth = 15,col="dark green")+
  theme_classic()+
  scale_fill_manual(values=pallete1[3:4])+
  coord_flip()  


#Analysis attemp 

lmer(fix_dur ~ cond + (1|item), data = raw_data)

dat <- raw_data

#Centering fix_dur
dat$fix_dur_c <- scale(dat$fix_dur, scale = F)

#Contrast
dat$target_changed= as.factor(dat$target_changed)
levels(dat$target_changed)
mycontrast <- cbind(identical_vs_diff = c(.5,.5,-1), ben_vs_bir = c(-1,1,0))
mycontrastNames <- colnames(mycontrast)
mycontrast <- zapsmall(t(ginv(mycontrast)))
colnames(mycontrast) = mycontrastNames
contrasts(dat$target_changed) = mycontrast

# Fixed Effects

# Fix dur, Gaze dur, Go-past time, Xpos, Fixation probability, Probability of making a regression
# Condition(ben/bir/identical)

#add column true/false for skip DONE
#add condition column DONE
# script for flanker simon DONE
# delete rt < 200ms DONE
#centered avgerage differences between congruent and incongr trials DONE

