# mutate = add or delete columns split apply and bind again
# summarise using group for 1 value for each combination 
# mutate you can have multiple output 
# summarise'ı cast and melt yerine kullan

get_words <- function(data_dir = "Data/Ben"){
  
  library(stringr)
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
    
    
    ### get start and end times ###
    
    S_W <- which(grepl('TRIAL \\d+ ITEM \\d+ WORD 1\\s', dataF)) #weird :D
    end<- which(grepl('DISPLAY OFF', dataF))
    trial_db <- data.frame(end, S_W)
    
    ntrials<- nrow(trial_db)
    
    for(j in 1:ntrials){
      temp <- data.frame(sub= NA, item = NA,cond=NA, boundaryN = NA, wordN = NA, word= NA, trial_type = NA)
      
      curr_file<- unlist(strsplit(dataASC[i], '/'))
      curr_file<- curr_file[length(curr_file)]
      curr_file_split<- unlist(strsplit(curr_file, '_'))
      curr_file_split<- curr_file_split[2]
      temp$sub<- get_num(curr_file_split)
      
      db<- trial_db[j,]
     
      

      
      if(!is.na(S_W[j + 1])){
        trialW <- dataF[S_W[j]:S_W[j+1]]
        
        boundaryN <- trialW[which(grepl('target_word_nr', trialW))]
        boundaryN <- as.data.frame(do.call( rbind, strsplit(boundaryN, ' ' )))
        temp$boundaryN <- as.numeric(boundaryN$V4)+1 #python starts with 0
        #trial type
        trial_ty <- trialW[which(grepl('var trial_type ', trialW))]
        trial_ty <- as.data.frame(do.call( rbind, strsplit(trial_ty, ' ' )))
        temp$trial_type <- trial_ty$V4
        
        
        #Condition
         
        cond <- trialW[which(grepl('TRIALID',trialW))]
        cond <- substr(cond, unlist(gregexpr(pattern =' ',cond[1]))[2]+1, nchar(cond))
        cond <- gsub(" ", "", cond)
        cond <- str_match(cond, pattern = '_(\\w{3,9})')[,2]
        
        temp$cond<- cond
        
        # if(temp$cond == 'ben'){
        #   target_word_chg <- trialW[which(grepl('var changed ', trialW))]
        #   target_word_chg <- as.data.frame(do.call( rbind, strsplit(target_word_chg, ' ' )))
        #   temp$cond <- target_word_chg$V4
        # }else{
        #   temp$cond <- 'identical'
        # }
        
        # ID<- trialW[which(grepl('TRIALID', trialW))]
        # 
        # ID<- substr(ID, unlist(gregexpr(pattern =' ',ID[1]))[2]+1, nchar(ID))
        # ID<- gsub(" ", "", ID)
        # itemN <- as.numeric(str_match(ID, pattern = '\\d{1,3}'))
        # cond <- str_match(trials, pattern = '_(\\w{3,9})')[,2]
        # 
        # temp$cond<- cond
        
        sentence_start_x <- 125
        word_count <- sum(str_count(trialW, pattern = 'WORD')) - 1
        words <- trialW[which(grepl('WORD', trialW))]
        words <- as.data.frame(do.call( rbind, strsplit(words, ' ' )))
        words <- head(words,word_count)
        
        for(m in 1:nrow(words)){
        temp$item <- as.numeric(words$V5[m])
        temp$wordN <- as.numeric(words$V7[m])
        temp$word <- words$V8[m]
        
        data <- rbind(data, temp)
        }
      
      }else{
        trialW <- dataF[S_W[j]:db$end]
        sentence_start_x <- 125
        words <- trialW[which(grepl('WORD', trialW))]
        words <- as.data.frame(do.call( rbind, strsplit(words, ' ' )))
        
        for(m in 1:nrow(words)){
          temp$item <- as.numeric(words$V5[m])
          temp$wordN <- as.numeric(words$V7[m])
          temp$word <- words$V8[m]
          data <- rbind(data, temp)
        }
        
      }
      
    }
    }
    return(data)
}
