rm(list= ls())

source("function/Code.R")
raw_data = preproc(data_dir = "/Users/zeynepgunesozkan/Desktop/Dr. Angele/Ben_exp/test")

source("function/Fixation.R")
raw_data2 = fixations(data_dir = "/Users/zeynepgunesozkan/Desktop/Dr. Angele/Ben_exp/test")
source("function/GazeDur.R")
dat <- gazedur(raw_data2)

save(raw_data, file= "data/raw_data.Rda")

write.csv(raw_data, "data/raw_data.csv")

save(raw_data2, file= "data/raw_data2.Rda")

write.csv(raw_data2, "data/raw_data2.csv")


library("viridis") 

library(ggplot2)
library(tidyquant)
library(tidyverse)
library(ggforce)
library(ggdist)
library(dplyr)
library(MASS)
library(lme4)
library(reshape)


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


#Analysis attemp :D


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

#Regression
m1 = lm(formula = fix_dur ~ target_changed , data = dat)
m2 = lmer(formula = fix_dur ~ target_changed  + (1|item) + (1|sub), data = dat)
m3 = lmer(formula = fix_dur ~ target_changed  + (1|item) + (cond|sub), data = dat)
m4 = lmer(formula = fix_dur ~ target_changed  + (1|item) + (1|sub) + (cond|sub), data = dat)
anova(m2,m3,m4,m5)
m5 = lmer(formula = log(fix_dur) ~ target_changed + (1|item) + (1|sub) + (cond|sub), data = dat) 
summary(m1)


# Fixed Effects

# Fix dur, Gaze dur, Go-past time, Xpos, Fixation probability, Probability of making a regression
# Condition(ben/bir/identical)



#Gaze Duration and Go-Past time

library(reshape)

dat2 <- raw_data2

Ndat <- aggregate(dat2$fix_dur, by = list(dat2$sub,dat2$item,dat2$wordN,dat2$trial_type) , 
                function(x) c(go_past = mean(as.numeric(x), na.rm= T), 
                              GD = sum(as.numeric(x), na.rm=T) ))

na.omit(dat2) #WHY NOT WORKING!!!

dat22 <- NULL
dat22<- melt(dat2,id.vars = c("sub","fix_dur","item","trial_type"),
                      measure.vars = "wordN")
dat22 <- na.omit(dat22)
dat23 <- cast(dat22, sub+item+trial_type+fix_dur ~ variable
                       ,function(x) c(CM=cummax(x)))

for(a in 1:dat2$sub){
  for(b in 1:dat2$item[a]){
    for(m in 1:dat2$wordN[b]){
      # seq1 <- data.frame(matrix(ncol = 1, nrow = wordN))
      # colnames(seq1) <- c('sequence')
      if(cummax(dat2$wordN[m]) == as.numeric(dat2$wordN[m])){
        dat2$sequence[m] <- 'first'
        }else{
          dat2$sequence[m] <- 'not first'

            }
          } 
        }
      }                               
 
