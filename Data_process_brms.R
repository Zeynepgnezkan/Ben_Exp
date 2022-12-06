
# Required libraries
library(stringr)
library(tidyverse)
library(MASS)
library(lme4)
library(readr)
library(ggpubr)
library(brms)

# Functions

source("function/Code.R")
source("function/get_words.R")
source("function/Fixation.R")
source("function/fixation_time_measures.R")
source("function/blinkTable.R")

raw_data = preproc(data_dir = "Data/Ben")
raw_data2 = fixations(data_dir = "Data/Ben")
first_pass <- first_pass_measures(raw_data2)
raw_words = get_words(data_dir = "Data/Ben",sentence_start_x = 125)
gopast <- go_past(raw_data2)
tvt <- tvt(raw_data2)
blink_data <- blinkTable(data_dir = "Data/Ben")

## Remove Practice Trials

raw_data <- raw_data %>% filter(trial_type != "practice")
raw_data2 <- raw_data2 %>% filter(trial_type != "practice" & !is.na(wordN))
raw_words <- raw_words %>% filter(trial_type != "practice")


raw_data2 <- raw_data2 %>% group_by(sub,item) %>% mutate(min1 = min(blink1^2),
                                                         min2 = min(blink2^2),
                                                         min3 = min(blink3^2))
raw_data2$blink = NA
for(i in 1:nrow(raw_data2)){
  
  if(!is.na(raw_data2$blink1[i])){
    if(as.numeric(raw_data2$blink1[i])^2 == as.numeric(raw_data2$min1[i])){
      if(as.numeric(raw_data2$blink1[i]) > 0){
        raw_data2$blink[i] = "before"
      }else{
        if(as.numeric(raw_data2$blink1[i]) < 0){
          raw_data2$blink[i] = "after"
        }
      }
    }
  }
  
  
  if(!is.na(raw_data2$blink2[i])){
    if(as.numeric(raw_data2$blink2[i])^2 == as.numeric(raw_data2$min2[i])){
      if(as.numeric(raw_data2$blink2[i]) > 0){
        raw_data2$blink[i] = "before"
      }else{
        if(as.numeric(raw_data2$blink2[i]) < 0){
          raw_data2$blink[i] = "after"
        }
      }
    }
  }
  
  
  if(!is.na(raw_data2$blink3[i])){
    if(as.numeric(raw_data2$blink3[i])^2 == as.numeric(raw_data2$min3[i])){
      if(as.numeric(raw_data2$blink3[i]) > 0){
        raw_data2$blink[i] = "before"
      }else{
        if(as.numeric(raw_data2$blink1[i]) < 0){
          raw_data2$blink[i] = "after"
        }
      }
    }
  }
  
}#end
raw_data2 <- raw_data2[ , -which(names(raw_data2) %in% c("blink1","blink2","blink3","min1","min2","min3"))]

raw_data2$delete <- NA
for(i in 1:nrow(raw_data2)){
  if(raw_data2$boundaryN[i] == raw_data2$wordN[i] && !is.na(raw_data2$blink[i])){
    raw_data2$delete[i] <- 1
  }else{
    if(raw_data2$boundaryN[i]+1 == raw_data2$wordN[i] && !is.na(raw_data2$blink[i])){
      if(raw_data2$blink[i] == "before"){
      raw_data2$delete[i] <- 1
      }
    }else{
      if(raw_data2$boundaryN[i]-1 == raw_data2$wordN[i] && !is.na(raw_data2$blink[i])){
        if(raw_data2$blink[i] == "after"){
          raw_data2$delete[i] <- 1
        }
      }
    }
  }
}

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



## saving and writing raw datas

save(raw_data, file= "Data/raw_data.Rda")

save(raw_data2, file= "Data/raw_data2.Rda")

save(raw_words, file= "Data/raw_words.Rda")

save(gopast, file= "Data/gopast.Rda")

save(tvt, file= "Data/tvt.Rda")

save(first_pass, file= "Data/first_pass.Rda")

save(blink_data, file= "Data/blink_data.Rda")

save(fixation_time_measures, file= "Data/fixation_time_measures.Rda")

write.csv(fixation_time_measures, "Data/fixation_time_measures.csv")

save(words_fixs, file= "Data/words_fixs.Rda")

# Extract display latency and blinks 

### these deletion criteria are purely my attempts. You can delete and change

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

blink <- subset(raw_data2,raw_data2$delete == 1)
blink <- blink %>% group_by(sub,item) %>% summarise()

sentencesskips <- fixation_time_measures %>% group_by(sub,item) %>% mutate(skip = as.numeric(skipping)-1) %>% summarise(rate = mean(skip,na.rm=TRUE))
sentencesskips <- subset(sentencesskips, rate > 0.7)
deleted <- rbind(DCLate,DCLate1,DCLate2,blink,sentencesskips)
deleted$delete <- 1


fixation_time_measures_withdelete <- left_join(fixation_time_measures,deleted, by=c("sub","item"))
fixation_time_measures_withdelete <- subset(fixation_time_measures_withdelete, is.na(fixation_time_measures_withdelete$delete))
save(fixation_time_measures_withdelete, file= "Data/fixation_time_measures_withdelete.Rda")

# Skipping rates

rateskip <- fixation_time_measures %>% filter(wordN == boundaryN) %>% group_by(target_changed)%>% mutate(skip = as.numeric(skipping)-1) %>% summarise(rate = mean(skip,na.rm=TRUE))

#### ANALYSIS ####

### N ###
# First fixation duration

lm_ffd <- lmer(data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN), log(ffd) ~ target_changed * scale(inhibition_score) + (1|sub)) #standart preview effect 
summary(lm_ffd)

# BRMS: Set priors

iter <- 5000
warmup <- 1000
chains <- 4

priors <- set_prior("normal(0,100)", class = "b")

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
  backend = "cmdstanr",
  threads = threading(4),
  seed = "6122022"
)

summary(blm_ffd)
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

lm_skip1m <- glmer(data = fixation_time_measures %>% filter(wordN == boundaryN - 1), skipping ~ target_changed * scale(inhibition_score) + (1|sub), family = binomial(link = "logit")) 
summary(lm_skip1m)

##### Data Visualization ######

pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F")

# General Fix Dur/Condition

ggplot(raw_data, aes(x= target_changed, log(fix_dur), fill=target_changed)) +
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.4) + 
  geom_boxplot(width = .1, outlier.shape = NA) +
  ggdist::stat_dots(side = "left", dotsize = .07, justification = 1.1, binwidth = 0.08,col="black")+
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


# Fixation plots

fixation_time_measures$wordCode <- NA
for(i in 1:nrow(fixation_time_measures)){
  if((fixation_time_measures$wordN[i] - fixation_time_measures$boundaryN[i]) == -1 ){
    fixation_time_measures$wordCode[i] <- "n-1"
  }else{
    if((fixation_time_measures$wordN[i] - fixation_time_measures$boundaryN[i]) == 1 ){
      fixation_time_measures$wordCode[i] <- "n + 1"
    }else{
      if((fixation_time_measures$wordN[i] - fixation_time_measures$boundaryN[i]) == 0){
        fixation_time_measures$wordCode[i] <- "n"
      }
    }
  }
}

fixation_time_measures$gd <- as.numeric(fixation_time_measures$gd)
fixation_time_measures$sfd <- as.numeric(fixation_time_measures$sfd)
fixation_time_measures$ffd <- as.numeric(fixation_time_measures$ffd)
fixation_time_measures$tvt <- as.numeric(fixation_time_measures$tvt)
fixation_time_measures$gopast <- as.numeric(fixation_time_measures$gopast)
x <- na.omit(x)


x <- fixation_time_measures %>% group_by(target_changed,wordCode) %>% summarise(meanffd = mean(ffd,na.rm = TRUE),
                                                                    meangd = mean(gd,na.rm = TRUE),
                                                                    meansfd = mean(sfd,na.rm = TRUE),
                                                                    meantvt = mean(tvt,na.rm = TRUE),
                                                                    meangpt = mean(gopast,na.rm = TRUE))
                                                                    

ffdplot <- ggplot(data = x, aes(x = target_changed, y = meanffd, fill = wordCode))+
  geom_bar(position="dodge", stat="identity",width = 0.6) + 
  ggtitle("First fixation duration")+
  theme(text=element_text(size=12,
                          family="Arial"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 10),
        panel.background = element_rect(colour = "black", size=1.3, fill=NA),
        legend.position  = "none") +
  coord_cartesian(ylim = range(200:250))+
  #scale_x_discrete(limits = c("n-1", "n", "n + 1"))+
  xlab("Target Position") +
  ylab("Mean fixation time") + 
  scale_fill_manual(values = pallete1[2:5]) +
  facet_grid( wordCode ~ .)

gdplot <- ggplot(data = x, aes(x = target_changed, y = meangd, fill = wordCode))+
  geom_bar(position="dodge", stat="identity",width = 0.6) + 
  ggtitle("Gaze duration")+
  theme(text=element_text(size=12,
                          family="Arial"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 10),
        panel.background = element_rect(colour = "black", size=1.3, fill=NA)) +
  coord_cartesian(ylim = range(200:330))+
  #scale_x_discrete(limits = c("n-1", "n", "n + 1"))+
  xlab("Target Position") +
  ylab("Mean fixation time") + 
  scale_fill_manual(name="Display Condition", values = pallete1[2:5],
                    labels = c("ben", "bir", "identical"))+
  facet_grid( wordCode ~ .)

tvtplot <- ggplot(data = x, aes(x = target_changed, y = meantvt, fill = wordCode))+
  geom_bar(position="dodge", stat="identity",width = 0.6) + 
  ggtitle("Total view time")+
  theme(text=element_text(size=12,
                          family="Arial"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 10),
        panel.background = element_rect(colour = "black", size=1.3, fill=NA)) +
  coord_cartesian(ylim = range(200:400))+
  #scale_x_discrete(limits = c("n-1", "n", "n + 1"))+
  xlab("Target Position") +
  ylab("Mean fixation time") + 
  scale_fill_manual(name="Display Condition", values = pallete1[2:5],
                    labels = c("ben", "bir", "identical"))+
  facet_grid( wordCode ~ .)

gptplot <- ggplot(data = x, aes(x = target_changed, y = meangpt, fill = wordCode))+
  geom_bar(position="dodge", stat="identity",width = 0.6) + 
  ggtitle("Go-Past time")+
  theme(text=element_text(size=12,
                          family="Arial"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 10),
        panel.background = element_rect(colour = "black", size=1.3, fill=NA)) +
  coord_cartesian(ylim = range(200:400))+
  #scale_x_discrete(limits = c("n-1", "n", "n + 1"))+
  xlab("Target Position") +
  ylab("Mean fixation time") + 
  scale_fill_manual(name="Display Condition", values = pallete1[2:5],
                    labels = c("ben", "bir", "identical"))+
  facet_grid( wordCode ~ .)

awesome_figure <- ggarrange(ffdplot, gdplot,gptplot,tvtplot + rremove("x.text"), 
                            labels = c("A", "B","C","D"),
                            ncol = 4, nrow = 1, legend= "none")













