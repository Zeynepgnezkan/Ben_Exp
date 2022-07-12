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
ggplot(raw_data, aes(x= cond, y=fix_dur, col= fix_dur))+
  geom_jitter()+
  theme_minimal()


### Bar Charts (just to try) ###

library(reshape)

subdat<- melt(raw_data, id=c('sub', 'item','cond'), 
             measure=c("fix_dur", 'sacc_dur', 'sacc_ampl', 'peak_vel'),na.rm = TRUE)
MSD<- cast(subdat, cond ~ variable
           ,function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))

MSD$fix_dur_SE <- sqrt(MSD$fix_dur_SD)

fix_bar <- ggplot(MSD) +
  geom_bar( aes(x=cond, y=fix_dur_M), stat="identity",fill="steelblue",width = 0.4) +
  geom_errorbar( aes(x=cond, ymin=fix_dur_M-fix_dur_SE, ymax=fix_dur_M+fix_dur_SE),
                 width=0.1, colour="black", alpha=0.9, size=0.7) +
  ylab("Mean Fixation Duration")+
  xlab("Condition")+
  ylim(0, 350)+
  ggtitle("Bars represent standard errors")
fix_bar

MSD$sacc_dur_SE <- sqrt(MSD$sacc_dur_SD)

sacc_bar <- ggplot(MSD) +
  geom_bar( aes(x=cond, y=sacc_dur_M), stat="identity",fill="steelblue",width = 0.4) +
  geom_errorbar( aes(x=cond, ymin=sacc_dur_M-sacc_dur_SE, ymax=sacc_dur_M+sacc_dur_SE),
                 width=0.1, colour="black", alpha=0.9, size=0.7) +
  ylab("Mean Saccade Duration")+
  xlab("Condition")+
  ggtitle("Bars represent standard errors")
sacc_bar

MSD$peak_vel_SE <- sqrt(MSD$peak_vel_SD)

velo_bar <- ggplot(MSD) +
  geom_bar( aes(x=cond, y=peak_vel_M), stat="identity",fill="steelblue",width = 0.4) +
  geom_errorbar( aes(x=cond, ymin=peak_vel_M-peak_vel_SE, ymax=peak_vel_M+peak_vel_SE),
                 width=0.1, colour="black", alpha=0.9, size=0.7) +
  ylab("Mean Saccade Velocity")+
  xlab("Condition")+
  ggtitle("Bars represent standard errors")
velo_bar

MSD$sacc_ampl_SE <- sqrt(MSD$sacc_ampl_SD)

ampl_bar <- ggplot(MSD) +
  geom_bar( aes(x=cond, y=sacc_ampl_M), stat="identity",fill="steelblue",width = 0.4) +
  geom_errorbar( aes(x=cond, ymin=sacc_ampl_M-sacc_ampl_SE, ymax=sacc_ampl_M+sacc_ampl_SE),
                 width=0.1, colour="black", alpha=0.9, size=0.7) +
  ylab("Mean Saccade Amplitude")+
  xlab("Condition")+
  ggtitle("Bars represent standard errors")
ampl_bar



