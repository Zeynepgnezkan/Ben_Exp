# Dr Bernhard Angele & Zeynep G Ozkan, 2022

#### Required libraries ####
packages= c("tidyverse", "ggpubr", "lme4", "readr","stringr","performance", "MASS") 

for(i in 1:length(packages)){
  
  if(packages[i] %in% rownames(installed.packages())==FALSE){
    install.packages(packages[i])
    library(packages[i], character.only=TRUE)
  }else{
    library(packages[i], character.only=TRUE)
  }
}

#### Load Data ####
load("Data/raw_data.Rda")
load("raw_data_fix.Rda")
load("Data/raw_words.Rda")
load("Data/gopast.Rda")
load("Data/tvt.Rda")
load("Data/first_pass.Rda")
load("Data/blink_data.Rda")
load("Data/fixation_time_measures.Rda")
load("Data/words_fixs.Rda")
load("Data/fixation_time_measures_withdelete.Rda")

#### Contrast ####

#contrasts(fixation_time_measures$condition) <- contr.sum

fixation_time_measures_withdelete$target_changed = as.factor(fixation_time_measures_withdelete$target_changed)
levels(fixation_time_measures_withdelete$target_changed)
mycontrast <- cbind(identical_vs_diff = c(.5,.5,-1), ben_vs_bir = c(-1,1,0))
mycontrastNames <- colnames(mycontrast)
mycontrast <- zapsmall(t(ginv(mycontrast)))
colnames(mycontrast) = mycontrastNames
contrasts(fixation_time_measures_withdelete$target_changed) = mycontrast


# Skipping rates

rateskip <-  fixation_time_measures_withdelete %>% filter(wordN == boundaryN) %>% group_by(target_changed)%>% mutate(skip = as.numeric(skipping)-1) %>% summarise(rate = mean(skip,na.rm=TRUE))

#### ANALYSIS ####

##### N #####
###### First fixation duration #####

lm_ffd <- lmer(data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN), log(ffd) ~ target_changed * scale(inhibition_score) + (1|sub)) #standart preview effect 
summary(lm_ffd)

###### Skipping ###### 

lm_skip <- glmer(data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN ), skipping ~ target_changed * scale(inhibition_score) + (1|sub), family = binomial(link = "logit")) 
summary(lm_skip)

###### Gaze Duration ###### 

lm_gd <- lmer(data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN), log(gd) ~ target_changed * scale(inhibition_score) + (1|sub)) 
summary(lm_gd)

###### Go-past Time ######

lm_gp <- lmer(data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN), log(gopast) ~ target_changed * scale(inhibition_score) + (1|sub)) 
summary(lm_gp)

###### Total View Time ######

lm_tvt <- lmer(data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN), log(tvt) ~ target_changed * scale(inhibition_score) + (1|sub)) 
summary(lm_tvt)

##### N+1 #####

###### First fixation duration ######

lm_ffd1 <- lmer(data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN +1), log(ffd) ~ target_changed * scale(inhibition_score) + (1|sub)) #standart preview effect 
summary(lm_ffd1)

###### Skipping ######

lm_skip1 <- glmer(data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN + 1), skipping ~ target_changed * scale(inhibition_score) + (1|sub), family = binomial(link = "logit")) 
summary(lm_skip1)

###### Gaze duration ######

lm_gd1 <- lmer(data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN + 1), log(gd) ~ target_changed * scale(inhibition_score) + (1|sub)) 
summary(lm_gd1)

###### Go-past Time ######

lm_gp1 <- lmer(data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN + 1), log(gopast) ~ target_changed * scale(inhibition_score) + (1|sub)) 
summary(lm_gp1)

###### TVT ######

lm_tvt1 <- lmer(data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN + 1), log(tvt) ~ target_changed * scale(inhibition_score) + (1|sub)) 
summary(lm_tvt1)

##### N-1 #####

###### Gaze Duration ######

lm_gd1m <- lmer(data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN - 1), log(gd) ~ target_changed * scale(inhibition_score) + (1|sub)) 
summary(lm_gd1m)

###### Skipping ######

lm_skip1m <- glmer(data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN - 1), skipping ~ target_changed * scale(inhibition_score) + (1|sub), family = binomial(link = "logit")) 
summary(lm_skip1m)

#### Data Visualization #####

pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F")

# General Fix Dur/Condition

ggplot(raw_data, aes(x= target_changed, log(fix_dur), fill=target_changed)) +
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.4) + 
  geom_boxplot(width = .1, outlier.shape = NA) +
  ggdist::stat_dots(side = "left", dotsize = .06, justification = 1.1, binwidth = 0.055,col=pallete1[4])+
  theme_classic()+
  scale_fill_manual(values=pallete1[1:3])+
  coord_flip()

# First fix dur / Condition (Target)

ggplot(data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN), aes(x= log(ffd), y=target_changed, fill=target_changed)) +
  ggdist::stat_halfeye(adjust = .5, .width = 0, justification = -.02) + 
  theme_classic()+
  scale_fill_manual(values=pallete1[4:6])+
  geom_boxplot(width = .1, outlier.shape = NA)

# Gaze duration / Condition (Target)

ggplot(data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN), aes(x= log(gd), y=target_changed, fill=target_changed)) +
  ggdist::stat_halfeye(adjust = .5, .width = 0, justification = -.02) + 
  theme_classic()+
  scale_fill_manual(values=pallete1[3:5])+
  geom_boxplot(width = .1, outlier.shape = NA)

# Go Past (Target)

ggplot(data = fixation_time_measures_withdelete %>% filter(wordN == boundaryN), aes(x= log(gopast), y=target_changed, fill=target_changed)) +
  ggdist::stat_halfeye(adjust = .5, .width = 0, justification = -.02) + 
  theme_classic()+
  scale_fill_manual(values=pallete1[2:4])+
  geom_boxplot(width = .1, outlier.shape = NA)


# Fixation plots

fixation_time_measures_withdelete$wordCode <- NA
for(i in 1:nrow(fixation_time_measures_withdelete)){
  if((fixation_time_measures_withdelete$wordN[i] - fixation_time_measures_withdelete$boundaryN[i]) == -1 ){
    fixation_time_measures_withdelete$wordCode[i] <- "n-1"
  }else{
    if((fixation_time_measures_withdelete$wordN[i] - fixation_time_measures_withdelete$boundaryN[i]) == 1 ){
      fixation_time_measures_withdelete$wordCode[i] <- "n + 1"
    }else{
      if((fixation_time_measures_withdelete$wordN[i] - fixation_time_measures_withdelete$boundaryN[i]) == 0){
        fixation_time_measures_withdelete$wordCode[i] <- "n"
      }
    }
  }
}

fixation_time_measures_withdelete$gd <- as.numeric(fixation_time_measures_withdelete$gd)
fixation_time_measures_withdelete$sfd <- as.numeric(fixation_time_measures_withdelete$sfd)
fixation_time_measures_withdelete$ffd <- as.numeric(fixation_time_measures_withdelete$ffd)
fixation_time_measures_withdelete$tvt <- as.numeric(fixation_time_measures_withdelete$tvt)
fixation_time_measures_withdelete$gopast <- as.numeric(fixation_time_measures_withdelete$gopast)
x <- na.omit(x)


x <- fixation_time_measures_withdelete %>% group_by(target_changed,wordCode) %>% summarise(meanffd = mean(ffd,na.rm = TRUE),
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


## Descriptives

x <- fixation_time_measures_withdelete %>% filter(boundaryN +1 == wordN) %>% group_by(target_changed) %>% summarise(sdffd = sd(ffd,na.rm = TRUE),
                                                                                           sdgd = sd(gd,na.rm = TRUE),
                                                                                           sdsfd = sd(sfd,na.rm = TRUE),
                                                                                           
                                                                                           sdgpt = sd(gopast,na.rm = TRUE))


rateskip <-  fixation_time_measures_withdelete %>% filter(wordN == boundaryN +1) %>% group_by(target_changed)%>% mutate(skip = as.numeric(skipping)-1) %>% summarise(rate = sd(skip,na.rm=TRUE))








