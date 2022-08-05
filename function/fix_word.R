
### Fix For Each Word

fix_word <- function(data_dir = "/Users/zeynepgunesozkan/Desktop/Dr. Angele/Ben_exp/Ben_Experiment/test"){
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
    
    ### get time ###
    S_W <- which(grepl('TRIAL \\d+ ITEM \\d+ WORD 1\\s', dataF))
    ##Boundaries
    library(stringr)
    sentence_start_x <- 125
    word_count <- sum(str_count(trialW, pattern = 'WORD')) - 1
    words <- trialW[which(grepl('WORD', trialW))]
    words <- as.data.frame(do.call( rbind, strsplit(words, ' ' )))
    
    
    
    
  }
    
    
    