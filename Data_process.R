
# Required libraries

library(tidyverse)
library(MASS)
library(lme4)

# Functions

source("function/Code.R")
source("function/get_words.R")
source("function/Fixation.R")
source("function/fixation_time_measures.R")

raw_data = preproc(data_dir = "Data/Ben")
raw_data2 = fixations(data_dir = "Data/Ben")
first_pass <- first_pass_measures(raw_data2)
raw_words = get_words(data_dir = "Data/Ben")
gopast <- go_past(raw_data2)
tvt <- tvt(raw_data2)

## Remove Practice Trials

raw_data <- raw_data %>% filter(trial_type != "practice")
raw_data2 <- raw_data2 %>% filter(trial_type != "practice" & !is.na(wordN))
raw_words <- raw_words %>% filter(trial_type != "practice")

## saving and writing raw datas

save(raw_data, file= "Data/raw_data.Rda")

save(raw_data2, file= "Data/raw_data2.Rda")

save(raw_words, file= "Data/raw_words.Rda")

save(gopast, file= "Data/gopast.Rda")

save(tvt, file= "Data/tvt.Rda")

save(first_pass, file= "Data/first_pass.Rda")

# Adding skipping

words_fixs <-full_join(raw_words,first_pass, by=c("item", "sub","wordN"))
words_fixs$skipping <- NA
for(i in 1:nrow(words_fixs)){
  if(is.na(words_fixs$ffd[i])){
    words_fixs$skipping[i] <- 'TRUE'
  }else{
    words_fixs$skipping[i] <- 'FALSE'
  }
}

save(words_fixs, file= "Data/words_fixs.Rda")


# Condition Added

words_fixs <- words_fixs %>% left_join(raw_data %>% dplyr::select(sub,item,target_changed))
words_fixs <- words_fixs[ , -which(names(words_fixs) %in% c("cond"))]


# Adding inhibition score

inhibition <- read_csv("inhibition_scores_by_participant.csv")

fixation_time_measures <- words_fixs %>%  left_join(inhibition, by = c("sub" = "participant")) %>%
  mutate(ffd = as.numeric(ffd), sfd = as.numeric(sfd), skipping = factor(skipping))

# Adding GP and TVT

fixation_time_measures <- fixation_time_measures %>% left_join(gopast,by=c("item","sub","wordN"))
fixation_time_measures <- fixation_time_measures %>% left_join(tvt,by=c("item","sub","wordN"))

## Contrast ##

#fixation_time_measures$condition = factor(fixation_time_measures$target_changed, levels = c("identical", "ben","bir"))

#contrasts(fixation_time_measures$condition) <- contr.sum

fixation_time_measures$target_changed = as.factor(fixation_time_measures$target_changed)
levels(fixation_time_measures$target_changed)
mycontrast <- cbind(identical_vs_diff = c(.5,.5,-1), ben_vs_bir = c(-1,1,0))
mycontrastNames <- colnames(mycontrast)
mycontrast <- zapsmall(t(ginv(mycontrast)))
colnames(mycontrast) = mycontrastNames
contrasts(fixation_time_measures$target_changed) = mycontrast

save(fixation_time_measures, file= "Data/fixation_time_measures.Rda")

write.csv(fixation_time_measures, "Data/fixation_time_measures.csv")

#### ANALYSIS ####

### N ###
# First fixation duration

lm_ffd <- lmer(data = fixation_time_measures %>% filter(wordN == boundaryN), log(ffd) ~ target_changed * scale(inhibition_score) + (1|sub)) #standart preview effect 
summary(lm_ffd)

# Skipping

lm_skip <- glmer(data = fixation_time_measures %>% filter(wordN == boundaryN ), skipping ~ target_changed * scale(inhibition_score) + (1|sub), family = binomial(link = "logit")) 
summary(lm_skip)

# Gaze Duration

lm_gd <- lmer(data = fixation_time_measures %>% filter(wordN == boundaryN), log(gd) ~ target_changed * scale(inhibition_score) + (1|sub)) 
summary(lm_gd)

# Go-past Time

lm_gp <- lmer(data = fixation_time_measures %>% filter(wordN == boundaryN), log(gopast) ~ target_changed * scale(inhibition_score) + (1|sub)) 
summary(lm_gp)

# Total View Time

lm_tvt <- lmer(data = fixation_time_measures %>% filter(wordN == boundaryN), log(tvt) ~ target_changed * scale(inhibition_score) + (1|sub)) 
summary(lm_tvt)

### N+1 ###

# First fixation duration

lm_ffd1 <- lmer(data = fixation_time_measures %>% filter(wordN == boundaryN +1), log(ffd) ~ target_changed * scale(inhibition_score) + (1|sub)) #standart preview effect 
summary(lm_ffd1)

# Skipping

lm_skip1 <- glmer(data = fixation_time_measures %>% filter(wordN == boundaryN + 1), skipping ~ target_changed * scale(inhibition_score) + (1|sub), family = binomial(link = "logit")) 
summary(lm_skip1)

# Gaze duration

lm_gd1 <- lmer(data = fixation_time_measures %>% filter(wordN == boundaryN + 1), log(gd) ~ target_changed * scale(inhibition_score) + (1|sub)) 
summary(lm_gd1)

# Go-past Time

lm_gp1 <- lmer(data = fixation_time_measures %>% filter(wordN == boundaryN + 1), log(gopast) ~ target_changed * scale(inhibition_score) + (1|sub)) 
summary(lm_gp1)

## TVT 

lm_tvt1 <- lmer(data = fixation_time_measures %>% filter(wordN == boundaryN + 1), log(tvt) ~ target_changed * scale(inhibition_score) + (1|sub)) 
summary(lm_tvt1)

### N-1 ###

# Gaze Duration

lm_gd1m <- lmer(data = fixation_time_measures %>% filter(wordN == boundaryN - 1), log(gd) ~ target_changed * scale(inhibition_score) + (1|sub)) 
summary(lm_gd1m)


##### Data Visualization ######

pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F")

# General Fix Dur/Condition

ggplot(raw_data, aes(x= target_changed, fix_dur, fill=target_changed)) +
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.4) + 
  geom_boxplot(width = .1, outlier.shape = NA) +
  ggdist::stat_dots(side = "left", dotsize = .2, justification = 1.1, binwidth = 9,col="black")+
  theme_classic()+
  scale_fill_manual(values=pallete1[1:3])+
  coord_flip()

# First fix dur / Condition (Target)

ggplot(data = fixation_time_measures %>% filter(wordN == boundaryN), aes(x= log(ffd), y=target_changed, fill=target_changed)) +
  ggdist::stat_halfeye(adjust = .5, .width = 0, justification = -.02) + 
  theme_classic()+
  scale_fill_manual(values=pallete1[4:6])+
  geom_boxplot(width = .1, outlier.shape = NA)

# Gaze duration / Condition (Target)

ggplot(data = fixation_time_measures %>% filter(wordN == boundaryN), aes(x= log(gd), y=target_changed, fill=target_changed)) +
  ggdist::stat_halfeye(adjust = .5, .width = 0, justification = -.02) + 
  theme_classic()+
  scale_fill_manual(values=pallete1[3:5])+
  geom_boxplot(width = .1, outlier.shape = NA)

# Go Past (Target)

ggplot(data = fixation_time_measures %>% filter(wordN == boundaryN), aes(x= log(gopast), y=target_changed, fill=target_changed)) +
  ggdist::stat_halfeye(adjust = .5, .width = 0, justification = -.02) + 
  theme_classic()+
  scale_fill_manual(values=pallete1[2:4])+
  geom_boxplot(width = .1, outlier.shape = NA)

# Skipping 

rate <- fixation_time_measures %>% filter(wordN == boundaryN) %>% group_by(sub)%>% count(skipping) %>% mutate(rate = (n/245)*100)
ratem1 <- fixation_time_measures %>% filter(wordN == boundaryN-1) %>% group_by(sub)%>% count(skipping) %>% mutate(rate = (n/245)*100)
rate1 <- fixation_time_measures %>% filter(wordN == boundaryN+1) %>% group_by(sub)%>% count(skipping) %>% mutate(rate = (n/245)*100)

skiprate <- subset(rate,skipping == TRUE)
skipratem1 <- subset(ratem1,skipping == TRUE)
skiprate1 <- subset(rate1,skipping == TRUE)

ggplot(data = rate, aes(x=sub,y=rate,fill=skipping)) + 
  geom_bar(position="dodge", stat="identity",width = 0.6)+
  theme_classic()+
  scale_fill_manual(values=pallete1[1:2])
  

plot(x=scale(inhibition$inhibition_score),y=skiprate$rate,
     main = "Skipping rate and Inhibiton scores",
     xlab = "Scaled Inhibition scores",
     ylab = "Skipping rate",
     ylim = range(0,100),
     col = alpha(pallete1[1],0.6),
     pch = 16,
     cex=1.2)
points(x=scale(inhibition$inhibition_score),y=skipratem1$rate,
     col = alpha(pallete1[3],0.6),
     pch = 16,
     cex=1.2)
points(x=scale(inhibition$inhibition_score),y=skiprate1$rate,
       col = alpha(pallete1[5],0.6),
       pch = 16,
       cex=1.2)
legend("topright", 
       pch = 16, 
       c("N", "N-1","N+1"), 
       col = alpha(c(pallete1[1],pallete1[3],pallete1[5]),0.6),
       cex=0.7
)




