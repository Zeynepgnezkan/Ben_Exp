rm(list= ls())

source("function/Code.R")

raw_data = preproc(data_dir = "/Users/zeynepgunesozkan/Desktop/Dr. Angele/Ben_exp/Ben_Experiment/test")

save(raw_data, file= "data/raw_data.Rda")

write.csv(raw_data, "data/raw_data.csv")

