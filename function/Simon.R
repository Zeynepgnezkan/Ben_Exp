
# Function for Simon

simon <- function(data_dir = "Data/Simon"){
  
  get_files<- function(dir= "", file_ext= ".csv"){
    
    if(dir== ""){
      dir= getwd()
    }
    
    # get a list of all file in dir:
    all_files<- list.files(dir)
    # remove non-asc files (if present)
    all_files<- all_files[grepl(file_ext, all_files)]
    
    # sort files by number in string
    get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
    num<- get_num(all_files) #change
    
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
  data = NULL
  dataALL<- get_files(data_dir)
  for(i in 1:length(dataALL)){#each subj
    data1 <- read_csv(dataALL[i])
    data1 <- data1[ , -which(names(data1) %in% c("box_x","pract.thisRepN","pract.thisTrialN","pract.thisN","pract.thisIndex","trials.thisRepN",
                                                 "trials.thisTrialN","trials.thisIndex","trials.thisN","trials_2.thisRepN","trials_2.thisTrialN","trials_2.thisN",
                                                 "trials_2.thisIndex","trials_3.thisRepN","trials_3.thisTrialN","trials_3.thisN",
                                                 "trials_3.thisIndex","key_resp_instr.keys","key_resp_instr.rt","FixCross.started","FixCross.stopped",
                                                 "Box_p.started","Box_p.stopped","key_resp_P.keys","key_resp_P.corr","key_resp_P.rt","key_resp_P.started",
                                                 "key_resp_P.stopped","text_3.started","text_3.stopped","key_resp_2.keys","key_resp_2.rt","Box.started",
                                                 "Box.stopped","key_resp.started","key_resp.stopped","key_resp_3.keys","key_resp_3.rt","key_resp_4.keys",
                                                 "key_resp_4.rt","session","date","psychopyVersion","frameRate"))]
    
    
    
    data1 <- data1[ , -which(names(data1) %in% c("...55"))]
    
    
    #clean NAs
    data1 <- na.omit(data1)
    data1$expName <- 'Simon'
    # Correcting participant 8's first block
    if(i == 8){
      for(m in 1:nrow(data1)){
        data1$key_resp.corr[m] = 1
        if(m == 49){
          break
        }
      }
    }
    
    data<- rbind(data1, data)
    
  }#End Subj
  
  return(data)
  
}#END OF FUNC
