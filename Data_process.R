rm(list= ls())

source("function/Code.R")
source("function/Fixation.R")

raw_data = preproc(data_dir = "/Users/zeynepgunesozkan/Desktop/Dr. Angele/Ben_exp/test")

raw_data2 = fixations(data_dir = "/Users/zeynepgunesozkan/Desktop/Dr. Angele/Ben_exp/test")

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

library(lme4)
lmer(fix_dur ~ cond + (1|item), data = raw_data)

dat <- raw_data

#Centering fix_dur
dat$fix_dur_c <- scale(dat$fix_dur, scale = F)

#Contrast
dat$cond= as.factor(dat$cond)
levels(dat$cond)
contrasts(dat$cond)
#Regression
LinReg = lm(formula = fix_dur ~ cond + (1|item) + (1+cond|sub), data = dat)
#lm(least squares) gives singularity error but lmer(random effects) not why?
summary(LinearRegression)


#fixed effects
#cond





