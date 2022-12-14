
blinkTable <- function(data_dir = "Data/Ben"){
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
  
  for(i in 1:length(dataASC)){
    
    cat("\n"); cat(sprintf("Loading data file: %s", dataASC[i]))
    dataF<- readLines(dataASC[i]) # load asc file;
    cat(". Done"); cat("\n")
    
    ID<- which(grepl('TRIALID', dataF));
    trial_text<- dataF[ID]
    trials<- substr(trial_text, unlist(gregexpr(pattern =' ',trial_text[1]))[2]+1, nchar(trial_text))
    trials<- gsub(" ", "", trials)
    itemN <- as.numeric(str_match(trials, pattern = '\\d{1,3}'))
    cond <- str_match(trials, pattern = '_(\\w{3,9})')[,2]
    
    ### get start and end times ###
    
    start<- which(grepl('DISPLAY ON', dataF))
    
    trial_start_t<- which(grepl('start_trial', dataF))
    
    S_W <- which(grepl('TRIAL \\d+ ITEM \\d+ WORD 1\\s', dataF)) #weird :D
    
    
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
      temp<- data.frame(sub=NA, item=NA, cond=NA, blink_number = NA,
                        blink_start = NA, blink_end = NA, blink_s_pos = NA, blink_dur=NA)
      
      cat(toString(j)); cat(" ")
      db<- trial_db[j,]
      trialF<- dataF[db$trial_start_t:db$trial_end_t]
      trialDis <- dataF[db$start:db$end]
      trialInfo<- dataF[db$ID:db$start]
      
      # generic info about trial:
      temp$sub<- db$subject
      temp$item<- trial_db$itemN[j]
      temp$cond<- trial_db$cond[j]
      
      # blink check

      sblinkflag <- which(grepl('SBLINK',trialDis))
      eblinkflag <- which(grepl('EBLINK',trialDis))
      blinkflag <- trialDis[eblinkflag]
      blinkstamp <- as.data.frame(do.call( rbind, strsplit(blinkflag, '\t' )))
      blinkstamp$V1 <- get_num(blinkstamp$V1)
      if(length(blinkflag) != 0){
        if(length(sblinkflag) == length(eblinkflag)){
      for(k in 1:length(blinkflag)){
        temp$blink_number <- k
        temp$blink_start <- blinkstamp$V1[k]
        temp$blink_end <- blinkstamp$V2[k]
        temp$blink_dur <- blinkstamp$V3[k]
        
        poscap <- trialDis[as.numeric(sblinkflag[k])-1]
        poscap <- as.data.frame(do.call( rbind, strsplit(poscap, '\t ' )))
        isdisplay <- which(grepl('DISPLAY',poscap))
        if(length(isdisplay) == 0){
          temp$blink_s_pos <- as.numeric(poscap$V2)
        }else{
          temp$blink_s_pos <- "display begining"
        }
        data <- rbind(data, temp)
        }
        }else{
        if(length(eblinkflag) > length(sblinkflag) && length(sblinkflag) != 0){
          eblinkflag <- eblinkflag[-1]
          for(k in 1:length(eblinkflag)){
            temp$blink_number <- k
            temp$blink_start <- blinkstamp$V1[k]
            temp$blink_end <- blinkstamp$V2[k]
            temp$blink_dur <- blinkstamp$V3[k]
            
            poscap <- trialDis[as.numeric(sblinkflag[k])-1]
            poscap <- as.data.frame(do.call( rbind, strsplit(poscap, '\t ' )))
            temp$blink_s_pos <- as.numeric(poscap$V2)
            
            data <- rbind(data, temp)
          }
        }
      }
     }# end blinks
    }# end of trial
  }# end of subj
  return(data)
}



