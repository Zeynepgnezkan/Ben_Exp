
#only for fixations

fixations <- function(data_dir = "/Users/zeynepgunesozkan/Desktop/Dr. Angele/Ben_exp/test"){
  options(scipen=999)
  
  #steal some fun from my Professor,
  
  get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
  
  # get a list of .asc files from a user-provided directory
  get_files<- function(dir= "", file_ext= ".asc"){
    
    if(dir== ""){
      dir= getwd()
    }
    
    # get a list of all file in dir:
    all_files<- list.files(dir)
    # remove non-asc files (if present)
    all_files<- all_files[grepl(file_ext, all_files)]
    # remove txt files (of present):
    all_files<- all_files[!grepl(".txt", all_files)]
    
    # sort files by number in string
    get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
    num<- get_num(all_files)
    
    if(!is.na(num[1])){
      all_files<- all_files[order(num, all_files)]
    }
    # convert to directory string for each data file:
    if(length(all_files)>0){
      all_dirs<- NULL
      for(i in 1:length(all_files)){
        all_dirs[i]<- paste(dir, "/", all_files[i], sep = "")
      }
      
      message(paste("Found", toString(length(all_files)), file_ext, "files in the specified directory!", "\n"))
      return(all_dirs)
    }else{
      stop(paste("Found no", file_ext, "files in the specified directory!"))
    }
  } # end of get_files()
  
  
  # check if user provided data dir:
  if(length(data_dir)==0){
    data_dir= file.choose() # make them chose a file
    message("To process multiple files, please specify a directory in 'data_dir'")
  }
  
  # Get data file names:
  dataASC<- get_files(data_dir)
  
  
  data<- NULL
  
  ## PROCESS ##
  
  for(i in 1:length(dataASC)){ # for each subject..
    
    cat("\n"); cat(sprintf("Loading data file: %s", dataASC[i]))
    dataF<- readLines(dataASC[i]) # load asc file;
    cat(". Done"); cat("\n")
    
    library(stringr)
    
    ### get trial names:
    ID<- which(grepl('TRIALID', dataF));
    trial_text<- dataF[ID]
    trials<- substr(trial_text, unlist(gregexpr(pattern =' ',trial_text[1]))[2]+1, nchar(trial_text))
    trials<- gsub(" ", "", trials)
    itemN <- as.numeric(str_match(trials, pattern = '\\d{1,3}'))
    cond <- str_match(trials, pattern = '_(\\w{3,9})')[,2]
    
    ### get start and end times ###
    
    start<- which(grepl('DISPLAY ON', dataF))
    trial_start_t<- which(grepl('start_trial', dataF))
    S_W <- which(grepl('TRIAL \\d+ ITEM \\d+ WORD 1\\s', dataF))
    
    end<- which(grepl('DISPLAY OFF', dataF))
    trial_end_t  <- which(grepl('stop_trial', dataF))
    
    trial_db<- data.frame(cond, itemN, start, end, ID, trial_start_t,trial_end_t,S_W)
    
    trial_db$filename<- dataASC[i]
    
    curr_file<- unlist(strsplit(dataASC[i], '/'))
    curr_file<- curr_file[length(curr_file)]
    curr_file_split<- unlist(strsplit(curr_file, '_'))
    curr_file_split<- curr_file_split[2]
    trial_db$subject<- get_num(curr_file_split)
    
    
    ntrials<- nrow(trial_db)
    cat(sprintf("Processing trial: "));
    for(j in 1:ntrials){
      temp<- data.frame(sub=NA, item=NA, cond=NA,number = NA, SFIX_t = NA,EFIX_t = NA,
                        fix_dur=NA, Xpos = NA, Ypos=NA, pupil = NA)
      
      cat(toString(j)); cat(" ")
      db<- trial_db[j,]
      trialF<- dataF[db$trial_start_t:db$trial_end_t]
      trialW <- dataF[S_W[j]:S_W[j+1]]
      trialInfo<- dataF[db$ID:db$start]
      # generic info about trial:
      temp$sub<- db$subject
      temp$item<- trial_db$itemN[j]
      temp$cond<- trial_db$cond[j]
      
      #fixations 
      # maybe we can narrow 
      
      EFixFlags<- which(grepl('EFIX', trialF))
      EFixStrings<- trialF[EFixFlags]
      
      all_fix <-as.data.frame(do.call( rbind, strsplit( EFixStrings, '\t' ) ))
      all_fix$V1 <- get_num(all_fix$V1)
      temp$word = NA
      temp$wordN = NA
      for(n in 1:nrow(all_fix)){
      
      
      temp$number <- as.numeric(n)
      temp$SFIX_t <- all_fix$V1[n]
      temp$EFIX_t <- all_fix$V2[n]
      temp$fix_dur <- all_fix$V3[n]
      temp$Xpos <- all_fix$V4[n]
      temp$Ypos <- all_fix$V5[n]
      temp$pupil <- all_fix$V6[n]

  
      
      ##Boundaries
      library(stringr)
      sentence_start_x <- 125
      word_count <- sum(str_count(trialW, pattern = 'WORD')) - 1
      words <- trialW[which(grepl('WORD', trialW))]
      words <- as.data.frame(do.call( rbind, strsplit(words, ' ' )))
      words <- head(words,word_count)
      distance <-  as.numeric(words$V10) - as.numeric(temp$Xpos)
      
      words$V11 = NA
      for(f in 2:nrow(words)){#1 manuel
        
        words$V11[f] <- as.numeric(words$V10[f])- as.numeric(words$V10[f-1])
        words$V11[1] <- as.numeric(words$V10[1]) - sentence_start_x
      }
      
      distanceP <- subset(distance, distance > 0)
      
      if(length(distanceP) != 0){
        min(distanceP)
        wordsb <- as.numeric(temp$Xpos) + min(distanceP)
        number <- which(grepl(wordsb, words$V10))
        temp$word <- words$V8[number]
        temp$wordN <- number
      }
      

     

      data<- rbind(data, temp)
      }
    }
}

return(data)

}
